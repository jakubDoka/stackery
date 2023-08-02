use std::ops::{Index, IndexMut, Range};

use mini_alloc::{FnvHashMap, IdentStr};

use crate::{
    instrs, parser::expr::DeclAst, ArrayLenFolder, BinaryAst, BlockAst, BreakAst, CallAst, Const,
    ConstFoldCtx, ContinueAst, Diagnostics, EnumAst, ExprAst, FieldAst, FieldIdentAst, Files,
    FilledArrayAst, Func, FuncAst, FuncMetaBuilder, FuncRef, Ident, IdentAst, IfAst, IndexAst,
    Instr, InstrEmiter, InstrIndex, InstrItem, InstrItemKind, InstrKind, InstrModule,
    InstrModuleMetaBuilder, InstrModuleMetaSlice, InstrRef, IntLit, LitAst, LitKindAst, Loop,
    LoopAst, LoopData, ModuleRef, Modules, NamedExprAst, OpCode, Parser, Ref, Severty, Span,
    StringParser, StructFieldAst, SymData, TempMemBase, UnaryAst, UnitAst, VecStore,
};

#[derive(Default)]
struct InstrEmiterRes {
    used_consts: FnvHashMap<LitKindAst, Const>,
    used_idents: FnvHashMap<IdentStr, Ident>,
    decl_scope: Scope<SymData>,
    loop_scope: Scope<LoopData>,
    breaks: Vec<BreakData>,
    returns: Vec<RetData>,
    modules: Vec<(IdentStr, ModuleRef)>,
    string_parser: StringParser,
}

struct BreakData {
    addr: InstrRef,
    label: Loop,
}

struct RetData {
    addr: InstrRef,
}

impl InstrEmiterRes {
    fn prepare_for_function(&mut self) {
        self.used_consts.clear();
        self.used_idents.clear();
        assert!(self.loop_scope.is_empty());
    }
}

impl<'a> InstrEmiter<'a> {
    pub fn emmit(mut self, temp_mem: &mut TempMemBase) -> Option<()> {
        let mut res = InstrEmiterRes::default();

        self.clean_meta();
        self.parse_modules(&mut res, temp_mem);

        Some(())
    }

    fn clean_meta(&mut self) {
        let preserved_modules = self.modules.preserved();
        let mut preserved_modules = self
            .instrs
            .modules
            .multiple_mut(preserved_modules)
            .map(|module| &mut module.meta)
            .collect::<Vec<_>>();

        self.instrs
            .module_meta
            .preserve_ranges(preserved_modules.as_mut_slice());

        let mut funcs = self
            .instrs
            .module_meta
            .funcs
            .values_mut()
            .map(|func| &mut func.meta)
            .collect::<Vec<_>>();

        self.instrs.func_meta.preserve_ranges(funcs.as_mut_slice());
    }

    fn parse_modules(&mut self, res: &mut InstrEmiterRes, temp_mem: &mut TempMemBase) {
        for module in self.modules.changed() {
            let (diver, arena) = temp_mem.diver_and_arena();
            let Some(ast) = Parser::new(
                self.modules.files(),
                self.modules.module_file(module),
                self.diags,
                &arena,
                &mut res.string_parser,
            )
            .parse(diver) else {
                continue;
            };

            res.modules.clear();
            res.modules.extend(self.modules.deps_of(module));

            let meta = self.emmit_module(ast, res);
            self.instrs.modules[module] = InstrModule { meta };
        }
    }

    fn emmit_module<'arena>(
        &mut self,
        items: &[ExprAst<'arena>],
        res: &mut InstrEmiterRes,
    ) -> InstrModuleMetaSlice {
        let mut module_meta = self.instrs.module_meta.builder();

        let mut func_stack = Self::collect_module_items(
            self.modules.files(),
            self.diags,
            items,
            res,
            &mut module_meta,
        );
        let frame = Self::prepare_scope(self.modules.files(), self.diags, res, &mut module_meta);

        while let Some(FuncStackTask {
            id,
            func,
            scope_snapshot,
        }) = func_stack.pop()
        {
            let module_frame = res.decl_scope.start_frame();

            res.decl_scope.load_snapshot(scope_snapshot);

            let base_source_offset =
                self.modules.files()[func.pipe.file()].offset_for_span(func.pipe);
            res.prepare_for_function();
            FuncBuilder {
                inner: self.instrs.func_meta.builder(),
                entities: &mut module_meta,
                modules: self.modules,
                diags: self.diags,
                res,
                func_stack: &mut func_stack,
                last_source_offset: base_source_offset,
                array_len_folder: self.array_len_folder,
                module_frame,
            }
            .build_func(func, id);
        }

        res.decl_scope.end_frame(frame);

        module_meta.finish()
    }

    fn prepare_scope(
        files: &Files,
        diags: &mut Diagnostics,
        res: &mut InstrEmiterRes,
        entities: &mut InstrModuleMetaBuilder,
    ) -> ScopeFrame {
        let frame = res.decl_scope.start_frame();

        entities.items.sort_unstable_by(|a, b| a.name.cmp(&b.name));

        for group in entities
            .items
            .group_by(|a, b| a.name == b.name)
            .filter(|g| g.len() > 1)
        {
            let mut builder = diags
                .builder(files)
                .footer(Severty::Error, "duplicate declaration")
                .footer(Severty::Note, "module use name only once");

            for item in group {
                builder =
                    builder.annotation(Severty::Error, item.span, "found duplicate declaration");
            }
        }

        for item in entities.items.iter() {
            res.decl_scope.push(instrs::SymData {
                name: item.name.clone(),
            });
        }

        frame
    }

    fn collect_module_items<'arena>(
        files: &Files,
        diags: &mut Diagnostics,
        items: &[ExprAst<'arena>],
        res: &mut InstrEmiterRes,
        entities: &mut InstrModuleMetaBuilder,
    ) -> Vec<FuncStackTask<'arena>> {
        let mut funcs = Vec::with_capacity(items.len());
        for item in items {
            let DeclAst {
                keyword,
                name,
                ty,
                value,
            } = match item {
                ExprAst::Unit(UnitAst::Decl(n)) => n,
                expr => {
                    diags
                        .builder(files)
                        .footer(Severty::Error, "expected declaration as top level item")
                        .annotation(Severty::Error, expr.span(), "found expression");
                    continue;
                }
            };

            let Some(value) = value else {
                diags
                    .builder(files)
                    .footer(Severty::Error, "expected declaration with value")
                    .annotation(Severty::Error, *keyword, "found declaration without value");
                continue;
            };

            let kind = match value {
                ExprAst::Unit(UnitAst::Func(func)) => {
                    let func_ref = entities.funcs.push(Func::default());

                    funcs.push(FuncStackTask {
                        id: func_ref,
                        func,
                        scope_snapshot: ScopeSnapshot::default(),
                    });
                    InstrItemKind::Func(func_ref)
                }
                //ExprAst::Unit(UnitAst::Import(module)) => {
                //    let Some(module) = res
                //        .modules
                //        .iter()
                //        .find_map(|&(ref name, id)| (name == &module.ident).then_some(id))
                //    else {
                //        diags
                //            .builder(files)
                //            .footer(Severty::Error, "module not found")
                //            .annotation(Severty::Error, module.span, "requested here");
                //        continue;
                //    };

                //    InstrItemKind::Import(module)
                //}
                expr => {
                    diags
                        .builder(files)
                        .footer(Severty::Error, "expected function as top level item")
                        .footer(Severty::Note, "static values are not yet supported")
                        .annotation(Severty::Error, expr.span(), "found expression");
                    continue;
                }
            };

            entities.items.push(InstrItem {
                name: name.ident.clone(),
                kind,
                span: name.span,
            });
        }
        funcs
    }
}

struct FuncStackTask<'a> {
    id: FuncRef,
    func: &'a FuncAst<'a>,
    scope_snapshot: ScopeSnapshot<SymData>,
}

struct FuncBuilder<'a, 'arena, 'ctx> {
    inner: FuncMetaBuilder<'a>,
    entities: &'a mut InstrModuleMetaBuilder<'ctx>,
    modules: &'a Modules,
    diags: &'a mut Diagnostics,
    res: &'a mut InstrEmiterRes,
    func_stack: &'a mut Vec<FuncStackTask<'arena>>,
    last_source_offset: usize,
    array_len_folder: &'a mut (dyn ArrayLenFolder + 'a),
    module_frame: ScopeFrame,
}

impl<'a, 'arena, 'ctx> FuncBuilder<'a, 'arena, 'ctx> {
    fn build_func(
        mut self,
        &FuncAst {
            args,
            ref body,
            pipe,
            ..
        }: &'arena FuncAst<'arena>,
        into: FuncRef,
    ) {
        let scope = self.res.decl_scope.start_frame();

        for arg in args {
            self.res
                .decl_scope
                .push(instrs::SymData::new(arg.name.ident.clone()));
        }

        self.expr(body);

        self.end_frame(scope, pipe);

        let instr_count = self.current_offset();
        for ret in self.res.returns.drain(..) {
            self.inner.instrs[ret.addr].kind = InstrKind::Jump {
                to: instr_count,
                conditional: false,
            };
        }

        let func = &mut self.entities.funcs[into];

        func.meta = self.inner.finish();
        func.arg_count = args.len().try_into().unwrap();
    }

    fn expr(&mut self, expr: &'arena ExprAst<'arena>) -> Option<()> {
        match expr {
            ExprAst::Unit(u) => self.unit(u),
            &ExprAst::Binary(b) => self.binary(b),
        }
    }

    fn unit(&mut self, unit: &'arena UnitAst<'arena>) -> Option<()> {
        match unit {
            UnitAst::Literal(ref l) => Some(self.literal(l)),
            UnitAst::Ident(ref i) => Some(self.ident(i)),
            //UnitAst::Import(ref i) => Some(self.import(i)),
            UnitAst::Block(b) => self.block(b),
            UnitAst::Unary(u) => self.unary(u),
            //&UnitAst::Array { bracket, elems } => self.array(bracket, elems),
            //UnitAst::FilledArray(fa) => self.filled_array(fa),
            //&UnitAst::Struct { keyword, fields } => self.struct_(keyword, fields),
            //UnitAst::Enum(e) => self.enum_(e),
            UnitAst::Call(c) => self.call(c),
            UnitAst::Func(f) => Some(self.func(f)),
            UnitAst::Decl(d) => self.decl(d),
            //UnitAst::Loop(l) => self.loop_(l),
            //UnitAst::Index(i) => self.index(i),
            //UnitAst::ForLoop(..) => todo!(),
            //UnitAst::Break(b) => self.break_(b)?,
            //UnitAst::Continue(c) => self.continue_(c)?,
            //UnitAst::Field(f) => self.field(f),
            //UnitAst::If(i) => self.if_(i),
            //&UnitAst::Ret { value, keyword } => self.ret(value, keyword),
            UnitAst::Paren(p) => self.expr(p),
            //UnitAst::Unknown(u) => Some(self.unknown(u)),
            //UnitAst::Self_(s) => Some(self.self_(s)),
        }
    }

    fn literal(&mut self, LitAst { kind, span }: &'arena LitAst) {
        let &mut lit = self
            .res
            .used_consts
            .entry(kind.clone())
            .or_insert_with(|| self.inner.consts.push(kind.clone()));

        self.add_instr(InstrKind::Const(lit), *span);
    }

    fn ident(&mut self, IdentAst { ident, span }: &'arena IdentAst) {
        let instr = match self.res.decl_scope.resolve(&ident) {
            Some(sym) => InstrKind::Sym(sym),
            None => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "symbol not found")
                    .annotation(Severty::Error, *span, "requested here");
                InstrKind::Error
            }
        };

        self.add_instr(instr, *span);
    }

    fn import(&mut self, IdentAst { ident, span }: &'arena IdentAst) {
        let module = self
            .res
            .modules
            .iter()
            .find_map(|&(ref name, module)| (name == ident).then_some(module));
        let instr = match module {
            Some(instr) => InstrKind::Mod(instr),
            None => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "module not found")
                    .annotation(Severty::Error, *span, "requested here");
                InstrKind::Error
            }
        };

        self.add_instr(instr, *span);
    }

    fn block(
        &mut self,
        &BlockAst {
            exprs,
            trailing_semi,
            brace,
        }: &'arena BlockAst<'arena>,
    ) -> Option<()> {
        let scope = self.res.decl_scope.start_frame();

        for expr in exprs {
            self.expr(expr)?;
            if !matches!(
                self.inner.instrs.last().map(|i| i.kind),
                Some(InstrKind::Drop | InstrKind::Decl | InstrKind::Binary(OpCode::Assign))
            ) {
                self.add_instr(InstrKind::Drop, expr.span());
            }
        }

        if !trailing_semi {
            self.pop_instr();
        }

        self.end_frame(scope, brace);

        Some(())
    }

    fn unary(&mut self, UnaryAst { op, expr }: &'arena UnaryAst<'arena>) -> Option<()> {
        self.unit(expr)?;
        self.add_instr(InstrKind::Unary(op.kind), op.span);
        Some(())
    }

    fn array(&mut self, bracket: Span, elems: &'arena [ExprAst<'arena>]) -> Option<()> {
        self.expr_list(elems)?;
        self.add_instr(
            InstrKind::Array {
                item_count: elems.len().try_into().unwrap(),
            },
            bracket,
        );
        Some(())
    }

    fn filled_array(
        &mut self,
        &FilledArrayAst {
            ref expr,
            ref len,
            bracket,
        }: &'arena FilledArrayAst<'arena>,
    ) -> Option<()> {
        self.expr(expr)?;
        let len = self
            .array_len_folder
            .fold(len, ConstFoldCtx { diags: self.diags });
        let len_const = self
            .inner
            .consts
            .push(LitKindAst::Int(IntLit::new(len as u64)));
        self.add_instr(InstrKind::FilledArray { len_const }, bracket);
        Some(())
    }

    fn struct_(&mut self, keyword: Span, fields: &'arena [StructFieldAst<'arena>]) -> Option<()> {
        for field in fields {
            self.sturct_field(field)?;
        }

        let field_count = fields.len().try_into().unwrap();
        self.add_instr(InstrKind::Struct { field_count }, keyword);

        Some(())
    }

    fn sturct_field(&mut self, field: &'arena StructFieldAst<'arena>) -> Option<()> {
        let (instr, span) = match field {
            StructFieldAst::Decl(NamedExprAst {
                name: name_ident,
                expr,
            }) => {
                self.expr(expr)?;
                let name = self.intern_ident(&name_ident.ident);
                (
                    InstrKind::StructField {
                        name,
                        has_value: true,
                    },
                    name_ident.span,
                )
            }
            StructFieldAst::Inline(name_ident) => {
                let name = self.intern_ident(&name_ident.ident);
                (
                    InstrKind::StructField {
                        name,
                        has_value: false,
                    },
                    name_ident.span,
                )
            }
        };

        self.add_instr(instr, span);

        Some(())
    }

    fn enum_(
        &mut self,
        &EnumAst {
            ref name,
            ref value,
            keyword,
        }: &'arena EnumAst<'arena>,
    ) -> Option<()> {
        if let Some(value) = value {
            self.expr(value)?;
        }
        let name = self.intern_ident(&name.ident);
        self.add_instr(
            InstrKind::Enum {
                name,
                has_value: value.is_some(),
            },
            keyword,
        );

        Some(())
    }

    fn call(&mut self, &CallAst { ref caller, args }: &'arena CallAst<'arena>) -> Option<()> {
        self.unit(caller)?;
        self.expr_list(args)?;
        let arg_count = args.len().try_into().unwrap();
        self.add_instr(InstrKind::Call { arg_count }, caller.span());
        Some(())
    }

    fn func(&mut self, func: &'arena FuncAst<'arena>) {
        let func_ref = self.entities.funcs.push(Default::default());
        self.func_stack.push(FuncStackTask {
            id: func_ref,
            func,
            scope_snapshot: self.res.decl_scope.take_snapshot(&self.module_frame),
        });
        self.add_instr(InstrKind::Func(func_ref), func.pipe);
    }

    fn decl(
        &mut self,
        DeclAst {
            keyword,
            name,
            value,
            ..
        }: &'arena DeclAst,
    ) -> Option<()> {
        self.res.decl_scope.push(SymData::new(name.ident.clone()));
        match value {
            Some(value) => self.expr(value)?,
            None => {
                self.add_instr(InstrKind::Uninit, *keyword);
            }
        }
        self.add_instr(InstrKind::Decl, name.span);
        Some(())
    }

    fn loop_(
        &mut self,
        &LoopAst {
            ref label,
            ref body,
            keyword,
        }: &'arena LoopAst<'arena>,
    ) -> Option<()> {
        let label = label
            .as_ref()
            .map(|label| label.ident.clone())
            .unwrap_or_default();

        let offset = self.current_offset();
        let dest = self.add_instr(InstrKind::BackJumpDest { used: false }, keyword);
        let data = LoopData {
            label,
            offset,
            break_frame: self.res.breaks.len(),
            dest,
            scope: self.res.decl_scope.start_frame(),
        };
        let label = self.res.loop_scope.push(data);

        if self.block(body).is_some() {
            self.add_instr(
                InstrKind::Jump {
                    to: offset,
                    conditional: false,
                },
                keyword,
            );
            self.inner.instrs[dest].kind = InstrKind::BackJumpDest { used: true };
        }

        let LoopData { break_frame, .. } = self.res.loop_scope.pop();

        let offset = self.current_offset();

        let (preserved, to_modify) =
            partition_by_predicate(&mut self.res.breaks[break_frame..], |b| b.label != label);

        let is_finite = !to_modify.is_empty();

        for break_data in to_modify {
            self.inner.instrs[break_data.addr].kind = InstrKind::Jump {
                to: offset,
                conditional: false,
            };
        }

        let new_len = break_frame + preserved.len();
        self.res.breaks.truncate(new_len);

        is_finite.then_some(())
    }

    fn index(&mut self, IndexAst { expr, index }: &'arena IndexAst<'arena>) -> Option<()> {
        self.unit(expr)?;
        self.expr(index)?;
        self.add_instr(InstrKind::Index, expr.span());
        Some(())
    }

    fn break_(
        &mut self,
        &BreakAst {
            ref label,
            ref expr,
            keyword,
        }: &'arena BreakAst<'arena>,
    ) -> Option<!> {
        let Some(label) = self.find_loop(label, keyword) else {
            return None;
        };

        if let Some(expr) = expr {
            self.expr(expr);
        }

        let scope = &self.res.loop_scope[label].scope;
        let decl_count = self.res.decl_scope.frame_size(scope);
        self.add_decl_drop(decl_count, keyword);
        let addr = self.add_instr(InstrKind::Error, keyword);
        self.res.breaks.push(BreakData { addr, label });

        None
    }

    fn continue_(&mut self, &ContinueAst { ref label, keyword }: &'arena ContinueAst) -> Option<!> {
        let Some(label) = self.find_loop(label, keyword) else {
            return None;
        };

        let LoopData {
            dest,
            offset,
            ref scope,
            ..
        } = self.res.loop_scope[label];

        self.inner.instrs[dest].kind = InstrKind::BackJumpDest { used: true };
        let decl_count = self.res.decl_scope.frame_size(scope);
        self.add_decl_drop(decl_count, keyword);
        self.add_instr(
            InstrKind::Jump {
                to: offset,
                conditional: false,
            },
            keyword,
        );

        None
    }

    fn find_loop(&mut self, label: &Option<IdentAst>, keyword: Span) -> Option<Loop> {
        let default = IdentStr::default();
        let label = label.as_ref().map(|label| &label.ident).unwrap_or(&default);
        let res = self.res.loop_scope.resolve(&label);

        if res.is_none() {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "control flow does not have a matching loop")
                .annotation(Severty::Error, keyword, "this");
            self.add_instr(InstrKind::Error, keyword);
        }

        res
    }

    fn field(
        &mut self,
        &FieldAst {
            ref expr,
            name:
                FieldIdentAst {
                    ref ident,
                    is_meta,
                    span,
                },
        }: &'arena FieldAst<'arena>,
    ) -> Option<()> {
        self.unit(expr)?;
        let name = self.intern_ident(&ident);
        self.add_instr(InstrKind::Field { name, is_meta }, span);
        Some(())
    }

    fn if_(
        &mut self,
        &IfAst {
            ref cond,
            ref then,
            ref else_,
            keyword,
        }: &'arena IfAst<'arena>,
    ) -> Option<()> {
        self.expr(cond)?;

        let if_ = self.add_instr(InstrKind::Error, keyword);
        let then_terminated = self.block(then).is_none();

        let else_addr = else_
            .as_ref()
            .map(|else_| (else_, self.add_instr(InstrKind::Error, else_.brace)));

        self.inner.instrs[if_].kind = InstrKind::Jump {
            to: self.current_offset(),
            conditional: true,
        };

        if let Some((else_, else_jump)) = else_addr {
            let else_terminates = self.block(else_).is_none();
            self.inner.instrs[else_jump].kind = InstrKind::Jump {
                to: self.current_offset(),
                conditional: false,
            };

            if then_terminated && else_terminates {
                return None;
            }
        }

        Some(())
    }

    fn ret(&mut self, expr: Option<&'arena ExprAst<'arena>>, keyword: Span) -> Option<()> {
        if let Some(expr) = expr {
            self.expr(expr)?;
        }

        let addr = self.add_instr(InstrKind::Error, keyword);

        let decl_count = self.res.decl_scope.len();
        self.add_decl_drop(decl_count, keyword);
        self.res.returns.push(RetData { addr });

        None
    }

    fn expr_list(&mut self, exprs: &'arena [ExprAst<'arena>]) -> Option<()> {
        for expr in exprs {
            self.expr(expr)?;
        }

        Some(())
    }

    fn binary(&mut self, BinaryAst { op, lhs, rhs }: &'arena BinaryAst<'arena>) -> Option<()> {
        self.expr(lhs)?;
        self.expr(rhs)?;
        self.add_instr(InstrKind::Binary(op.kind), op.span);
        Some(())
    }

    fn intern_ident(&mut self, ident: &IdentStr) -> Ident {
        *self
            .res
            .used_idents
            .entry(ident.clone())
            .or_insert_with(|| self.inner.idents.push(ident.clone()))
    }

    fn current_offset(&self) -> InstrIndex {
        self.inner
            .instrs
            .len()
            .try_into()
            .expect("too many instructions")
    }

    fn add_instr(&mut self, instr: InstrKind, span: Span) -> InstrRef {
        let next_offset = self.modules.files()[span.file()].offset_for_span(span);
        let incremental_offset = (next_offset as isize - self.last_source_offset as isize)
            .try_into()
            .unwrap();
        self.last_source_offset = next_offset;
        self.inner
            .instrs
            .push(Instr::new(instr, incremental_offset))
    }

    fn pop_instr(&mut self) -> Instr {
        self.inner.instrs.pop().unwrap()
    }

    fn add_decl_drop(&mut self, decl_count: InstrIndex, span: Span) {
        if decl_count == 0 {
            return;
        }

        if let Some(InstrKind::DropDecl {
            decl_count: prev_count,
        }) = self.inner.instrs.last_mut().map(|instr| &mut instr.kind)
        {
            *prev_count += decl_count;
            return;
        }

        self.add_instr(InstrKind::DropDecl { decl_count }, span);
    }

    fn end_frame(&mut self, scope: ScopeFrame, span: Span) {
        let decl_count = self.res.decl_scope.frame_size(&scope);
        self.add_decl_drop(decl_count, span);
        self.res.decl_scope.end_frame(scope);
    }
}

fn partition_by_predicate<T, F>(slice: &mut [T], mut pred: F) -> (&mut [T], &mut [T])
where
    F: FnMut(&T) -> bool,
{
    let Range { start, end } = slice.as_mut_ptr_range();
    let mut left_cursor = start;
    let mut right_cursor = start;

    unsafe {
        while right_cursor != end {
            if pred(&*right_cursor) {
                std::ptr::swap(left_cursor, right_cursor);
                left_cursor = left_cursor.add(1);
            }
            right_cursor = right_cursor.add(1);
        }
    }

    unsafe {
        (
            std::slice::from_mut_ptr_range(start..left_cursor),
            std::slice::from_mut_ptr_range(left_cursor..end),
        )
    }
}

struct Scope<T> {
    vec: VecStore<T, InstrIndex>,
}

impl<T> Default for Scope<T> {
    fn default() -> Self {
        Self {
            vec: VecStore::default(),
        }
    }
}

impl<T> Scope<T> {
    fn push(&mut self, sym: T) -> Ref<T, InstrIndex> {
        self.vec.push(sym)
    }

    fn resolve(&self, name: &T::Key) -> Option<Ref<T, InstrIndex>>
    where
        T: ScopeItem,
    {
        self.vec
            .iter()
            .rev()
            .find_map(|(index, item)| (item.key() == name).then_some(index))
    }

    fn pop(&mut self) -> T {
        self.vec.pop().unwrap()
    }

    fn start_frame(&self) -> ScopeFrame {
        ScopeFrame(self.vec.len())
    }

    fn end_frame(&mut self, frame: ScopeFrame) {
        self.vec.truncate(frame.0);
    }

    fn frame_size(&self, frame: &ScopeFrame) -> InstrIndex {
        (self.vec.len() - frame.0).try_into().unwrap()
    }

    fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    fn len(&self) -> InstrIndex {
        self.vec.len().try_into().unwrap()
    }

    fn take_snapshot(&self, up_to: &ScopeFrame) -> ScopeSnapshot<T>
    where
        T: Clone,
    {
        self.vec.values().skip(up_to.0).cloned().collect()
    }

    fn load_snapshot(&mut self, scope_snapshot: Vec<T>) {
        self.vec.extend(scope_snapshot);
    }
}

impl<T> Index<Ref<T, InstrIndex>> for Scope<T> {
    type Output = T;

    fn index(&self, index: Ref<T, InstrIndex>) -> &Self::Output {
        &self.vec[index]
    }
}

impl<T> IndexMut<Ref<T, InstrIndex>> for Scope<T> {
    fn index_mut(&mut self, index: Ref<T, InstrIndex>) -> &mut Self::Output {
        &mut self.vec[index]
    }
}

type ScopeSnapshot<T> = Vec<T>;

pub(super) struct ScopeFrame(usize);

trait ScopeItem {
    type Key: Eq;
    fn key(&self) -> &Self::Key;
}

impl ScopeItem for LoopData {
    type Key = IdentStr;

    fn key(&self) -> &Self::Key {
        &self.label
    }
}

impl ScopeItem for SymData {
    type Key = IdentStr;

    fn key(&self) -> &Self::Key {
        &self.name
    }
}

#[cfg(test)]
mod test {
    use pollster::FutureExt;

    use crate::{
        format_instrs, ArrayLenFolder, BinaryAst, Diagnostics, ExprAst, InstrEmiter, InstrItemKind,
        Instrs, LitAst, LitKindAst, LoaderMock, ModuleLoader, Modules, OpCode, TempMemBase,
        UnitAst,
    };

    #[derive(Default)]
    struct ArrayLenFolderMock;

    impl ArrayLenFolder for ArrayLenFolderMock {
        fn fold(&mut self, expr: &ExprAst, mut ctx: crate::ConstFoldCtx) -> usize {
            match expr {
                ExprAst::Unit(UnitAst::Literal(LitAst {
                    kind: LitKindAst::Int(i),
                    ..
                })) => i.value() as usize,
                ExprAst::Binary(BinaryAst { lhs, rhs, op }) => {
                    let lhs = self.fold(lhs, ctx.stack_borrow());
                    let rhs = self.fold(rhs, ctx);
                    match op.kind {
                        OpCode::Add => lhs + rhs,
                        OpCode::Sub => lhs - rhs,
                        OpCode::Mul => lhs * rhs,
                        OpCode::Div => lhs / rhs,
                        OpCode::Mod => lhs % rhs,
                        _ => unimplemented!(),
                    }
                }
                _ => unimplemented!(),
            }
        }
    }

    fn perform_test(sources: &str, ctx: &mut String) {
        let mut loader = LoaderMock::new(sources);

        let mut diagnostics = Diagnostics::default();
        let mut modules = Modules::default();

        let root = "root".into();

        let meta = ModuleLoader::new(&mut loader, &mut modules, &mut diagnostics)
            .update(root)
            .block_on();

        if !diagnostics.diagnostic_view().is_empty() {
            ctx.push_str("loader diagnostics:\n");
            ctx.push_str(diagnostics.diagnostic_view());
            diagnostics.clear();
        }

        let Some(_meta) = meta else {
            ctx.push_str("no meta");
            return;
        };

        let mut instrs = Instrs::default();
        let mut temp_mem = TempMemBase::default();
        let mut array_len_folder = ArrayLenFolderMock::default();

        InstrEmiter::new(
            &mut instrs,
            &mut modules,
            &mut diagnostics,
            &mut array_len_folder,
        )
        .emmit(&mut temp_mem);

        if !diagnostics.diagnostic_view().is_empty() {
            ctx.push_str("emmiter diagnostics:\n");
            ctx.push_str(diagnostics.diagnostic_view());
        }

        ctx.push_str("instrs:\n");
        for module in modules.changed() {
            let module_name = modules.name_of(module);
            let instr_module = &instrs.modules[module];
            ctx.push_str("  ");
            ctx.push_str(&module_name);
            ctx.push_str(":\n");

            let meta_view = instrs.module_meta.view(instr_module.meta);
            for item in meta_view.items {
                ctx.push_str("    ");
                ctx.push_str(&item.name);

                match item.kind {
                    InstrItemKind::Func(func) => {
                        ctx.push_str(":\n");
                        let func = &meta_view.funcs[func];
                        let func_meta_view = instrs.func_meta.view(func.meta);
                        format_instrs(func_meta_view, ctx, "  ");
                    }
                    InstrItemKind::Import(module) => {
                        ctx.push_str(": :{");
                        ctx.push_str(modules.name_of(module));
                        ctx.push_str("}\n");
                    }
                }
            }
        }
    }

    macro_rules! cases {
        ($(
            $name:ident $sources:literal
        )*) => {print_test::cases! {$(
            fn $name(ctx) {
                perform_test($sources, ctx);
            }
        )*}};
    }

    cases! {
        single_module "`root
            main: || foo(1, 1);
            foo: |a, b| a + b + 1;
        `"

        multy_module "
            `root
                main: || :{a}.foo(1, 1);
            `

            `a
                foo: |a, b| a + b + 1;
            `
        "
        variables "`root
            main: || {
                a: 1;
                b: 2;
                c: a + b;
                c
            };
        `"
        variables_in_scopes "`root
            main: || {
                a: 1;
                b: 2;
                c: {
                    a: 2;
                    b: 3;
                    a + b
                };
                c + a + b
            };
        `"
        control_flow "`root
            fib: |x| {
                if x < 2 {
                    x
                } else {
                    fib(x - 1) + fib(x - 2)
                }
            };

            iter_fib: |x| {
                a: 0;
                b: 1;
                loop { if x == 0 {break} else {
                    c: a + b;
                    a = b;
                    b = c;
                    x -= 1;
                } };
                a
            };
        `"
        imports "
            `root
                a: :{a};
                main: || a.foo(1, 1);
            `

            `a
                foo: |a, b| a + b + 1;
            `
        "
        nested_loops "`root
            main: || loop.a {
                a: loop {
                    break 10;
                } + 0;

                loop {
                    break.a 10 + a;
                }
            };
        `"
        array "`root
            main: || {
                a: [1, 2, 3];
                a[0] + a[1] + a[2]
            };

            filled_main: || {
                [3; 10 / 2 + 1]
            };
        `"
    }
}

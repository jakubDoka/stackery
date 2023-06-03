use mini_alloc::*;

use crate::{parser::expr::NamedExprAst, *};

#[derive(Default)]
struct InstrEmiterRes<'a> {
    used_consts: FnvHashMap<LiteralKindAst, Const>,
    used_idents: FnvHashMap<InternedStr, Ident>,
    label_count: InstrIndex,
    function_stack: Vec<(FuncRef, FuncAst<'a>)>,
    decl_scope: Scope<SymData>,
    loop_scope: Scope<LabelData>,
    modules: Vec<(InternedStr, ModuleRef)>,
}

impl<'a> InstrEmiter<'a> {
    pub fn emmit(mut self, meta: &ModuleMeta) -> Option<()> {
        let mut res = InstrEmiterRes::default();

        self.clean_funcs(meta);
        self.parse_modules(meta, &mut res);

        Some(())
    }

    fn clean_funcs(&mut self, meta: &ModuleMeta) {
        let preserved_funcs = ();
    }

    fn parse_modules(&mut self, meta: &ModuleMeta, res: &mut InstrEmiterRes) {}

    fn emmit_module<'arena>(
        self,
        items: &[ExprAst<'arena>],
        res: &mut InstrEmiterRes<'arena>,
    ) -> InstrModuleMetaSlice {
        let mut module_meta = self.funcs.module_meta.builder();

        Self::collect_module_items(
            self.modules.files(),
            self.interner,
            self.diags,
            items,
            res,
            &mut module_meta,
        );
        let frame = Self::prepare_scope(
            self.modules.files(),
            self.interner,
            self.diags,
            res,
            &mut module_meta,
        );

        while let Some((func_ref, func)) = res.function_stack.pop() {
            FuncBuilder {
                inner: self.funcs.func_meta.builder(),
                entities: &mut module_meta,
                modules: self.modules,
                interner: self.interner,
                diags: self.diags,
                res,
            }
            .build_func(func, func_ref);
        }

        res.decl_scope.end_frame(frame);

        module_meta.finish()
    }

    fn prepare_scope(
        files: &Files,
        interner: &StrInterner,
        diags: &mut Diagnostics,
        res: &mut InstrEmiterRes,
        entities: &mut InstrModuleMetaBuilder,
    ) -> ScopeFrame {
        let frame = res.decl_scope.start_frame();

        entities.items.sort_unstable_by_key(|item| item.name);

        for group in entities
            .items
            .group_by(|a, b| a.name == b.name)
            .filter(|g| g.len() > 1)
        {
            let mut builder = diags
                .builder(files, interner)
                .footer(Severty::Error, "duplicate declaration")
                .footer(Severty::Note, "module use name only once");

            for item in group {
                builder =
                    builder.annotation(Severty::Error, item.span, "found duplicate declaration");
            }
        }

        for &item in entities.items.iter() {
            res.decl_scope.push(instrs::SymData(item.name));
        }

        frame
    }

    fn collect_module_items<'arena>(
        files: &Files,
        interner: &StrInterner,
        diags: &mut Diagnostics,
        items: &[ExprAst<'arena>],
        res: &mut InstrEmiterRes<'arena>,
        entities: &mut InstrModuleMetaBuilder,
    ) {
        for &item in items {
            let NamedExprAst { name, expr } = match item {
                ExprAst::Unit(UnitAst::Decl(&n)) => n,
                expr => {
                    diags
                        .builder(files, interner)
                        .footer(Severty::Error, "expected declaration as top level item")
                        .annotation(Severty::Error, expr.span(), "found expression");
                    continue;
                }
            };

            let func = match expr {
                ExprAst::Unit(UnitAst::Func(&f)) => f,
                expr => {
                    diags
                        .builder(files, interner)
                        .footer(Severty::Error, "expected function as top level item")
                        .footer(Severty::Note, "static values are not yet supported")
                        .annotation(Severty::Error, expr.span(), "found expression");
                    continue;
                }
            };

            let func_ref = entities.funcs.push(Func::default());

            res.function_stack.push((func_ref, func));
            entities.items.push(InstrItem {
                name: name.ident,
                kind: InstrItemKind::Func(func_ref),
                span: name.span,
            });
        }
    }
}

struct FuncBuilder<'a, 'arena, 'ctx> {
    inner: FuncMetaBuilder<'a>,
    entities: &'a mut InstrModuleMetaBuilder<'ctx>,
    modules: &'a Modules,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
    res: &'a mut InstrEmiterRes<'arena>,
}

impl<'a, 'arena, 'ctx> FuncBuilder<'a, 'arena, 'ctx> {
    fn build_func(mut self, FuncAst { args, body, .. }: FuncAst<'arena>, into: FuncRef) {
        let scope = self.res.decl_scope.start_frame();

        for arg in args {
            self.res.decl_scope.push(instrs::SymData(arg.name.ident));
        }

        self.expr(body);

        self.res.decl_scope.end_frame(scope);

        let func = &mut self.entities.funcs[into];

        func.meta = self.inner.finish();
        func.arg_count = args.len().try_into().unwrap();
    }

    fn expr(&mut self, expr: ExprAst<'arena>) {
        match expr {
            ExprAst::Unit(u) => self.unit(u),
            ExprAst::Binary(&b) => self.binary(b),
        }
    }

    fn unit(&mut self, unit: UnitAst<'arena>) {
        match unit {
            UnitAst::Literal(l) => self.literal(l),
            UnitAst::Ident(i) => self.ident(i),
            UnitAst::Import(i) => self.import(i),
            UnitAst::Block(&b) => self.block(b),
            UnitAst::Unary(&u) => self.unary(u),
            UnitAst::Array { bracket, elems } => self.array(bracket, elems),
            UnitAst::FilledArray(&fa) => self.filled_array(fa),
            UnitAst::Tuple { keyword, values } => self.tuple(keyword, values),
            UnitAst::Struct { keyword, fields } => self.struct_(keyword, fields),
            UnitAst::Enum(&e) => self.enum_(e),
            UnitAst::Call(&c) => self.call(c),
            UnitAst::Func(&f) => self.func(f),
            UnitAst::Decl(&d) => self.decl(d),
            UnitAst::Loop(&l) => self.loop_(l),
            UnitAst::Index(&i) => self.index(i),
            UnitAst::ForLoop(fl) => todo!(),
            UnitAst::Break(&b) => self.break_(b),
            UnitAst::Continue(c) => self.continue_(c),
            UnitAst::Field(&f) => self.field(f),
            UnitAst::If(&i) => self.if_(i),
            UnitAst::Ret { value, .. } => self.ret(value.copied()),
            UnitAst::Paren(p) => self.expr(*p),
            UnitAst::Unknown(u) => self.unknown(u),
        }
    }

    fn literal(&mut self, LiteralAst { kind, .. }: LiteralAst) {
        let &mut lit = self
            .res
            .used_consts
            .entry(kind)
            .or_insert_with(|| self.inner.consts.push(kind));

        self.inner.instrs.push(Instr::Const(lit));
    }

    fn ident(&mut self, IdentAst { ident, span }: IdentAst) {
        let instr = match self.res.decl_scope.resolve(instrs::SymData(ident)) {
            Some(sym) => Instr::Ident(sym),
            None => {
                self.diags
                    .builder(self.modules.files(), self.interner)
                    .footer(Severty::Error, "symbol not found")
                    .annotation(Severty::Error, span, "requested here");
                Instr::Unkown
            }
        };

        self.inner.instrs.push(instr);
    }

    fn import(&mut self, IdentAst { ident, span }: IdentAst) {
        let module = self
            .res
            .modules
            .iter()
            .find_map(|&(name, module)| (name == ident).then_some(module));
        let instr = match module {
            Some(instr) => Instr::Import(instr),
            None => {
                self.diags
                    .builder(self.modules.files(), self.interner)
                    .footer(Severty::Error, "module not found")
                    .annotation(Severty::Error, span, "requested here");
                Instr::Unkown
            }
        };

        self.inner.instrs.push(instr);
    }

    fn block(
        &mut self,
        BlockAst {
            exprs,
            trailing_semi,
            ..
        }: BlockAst<'arena>,
    ) {
        let scope = self.res.decl_scope.start_frame();

        let expr_count = exprs.len().try_into().unwrap();
        let block_isntr = match trailing_semi {
            true => Instr::Block { expr_count },
            false => Instr::ExprBlock { expr_count },
        };
        self.inner.instrs.push(block_isntr);

        self.expr_list(exprs);

        self.res.decl_scope.end_frame(scope);
    }

    fn unary(&mut self, UnaryAst { op, expr }: UnaryAst<'arena>) {
        self.unit(expr);
        self.inner.instrs.push(Instr::Unary(op.kind));
    }

    fn array(&mut self, _bracket: Span, elems: &[ExprAst<'arena>]) {
        let item_count = elems.len().try_into().unwrap();
        self.inner.instrs.push(Instr::Array { item_count });

        self.expr_list(elems);
    }

    fn filled_array(&mut self, FilledArrayAst { expr, len, .. }: FilledArrayAst<'arena>) {
        self.expr(expr);
        self.expr(len);
        self.inner.instrs.push(Instr::FilledArray);
    }

    fn tuple(&mut self, _keyword: Span, values: &[ExprAst<'arena>]) {
        let item_count = values.len().try_into().unwrap();
        self.inner.instrs.push(Instr::Tuple { item_count });

        self.expr_list(values);
    }

    fn struct_(&mut self, _keyword: Span, fields: &[StructFieldAst<'arena>]) {
        let field_count = fields.len().try_into().unwrap();
        self.inner.instrs.push(Instr::Struct { field_count });

        for &field in fields {
            self.sturct_field(field);
        }
    }

    fn sturct_field(&mut self, field: StructFieldAst<'arena>) {
        match field {
            StructFieldAst::Decl(NamedExprAst { name, expr }) => {
                self.expr(expr);
                let name = self.intern_ident(name.ident);
                self.inner.instrs.push(Instr::StructField(name));
            }
            StructFieldAst::Inline(name) => {
                let name = self.intern_ident(name.ident);
                self.inner.instrs.push(Instr::InlinedField(name));
            }
            StructFieldAst::Embed(expr) => {
                self.expr(expr);
                self.inner.instrs.push(Instr::Embed);
            }
        }
    }

    fn enum_(&mut self, EnumAst { name, value }: EnumAst<'arena>) {
        let name = self.intern_ident(name.ident);
        let instr = if let Some(value) = value {
            self.expr(value);
            Instr::Enum(name)
        } else {
            Instr::UnitEnum(name)
        };
        self.inner.instrs.push(instr);
    }

    fn call(&mut self, CallAst { caller, args }: CallAst<'arena>) {
        self.unit(caller);
        self.expr_list(args);
        let arg_count = args.len().try_into().unwrap();
        self.inner.instrs.push(Instr::Call { arg_count });
    }

    fn func(&mut self, func: FuncAst<'arena>) {
        let func_ref = self.entities.funcs.push(Default::default());
        self.res.function_stack.push((func_ref, func));
        self.inner.instrs.push(Instr::Func(func_ref));
    }

    fn decl(&mut self, NamedExprAst { name, expr }: NamedExprAst<'arena>) {
        self.res.decl_scope.push(instrs::SymData(name.ident));
        self.expr(expr);
        self.inner.instrs.push(Instr::Decl);
    }

    fn loop_(&mut self, LoopAst { label, body, .. }: LoopAst<'arena>) {
        let label = label.map(|label| label.ident);

        let scope = self.res.loop_scope.start_frame();
        let label = self.res.loop_scope.push(instrs::LabelData(label));

        self.inner.instrs.push(Instr::Loop);
        self.expr(body);
        self.inner.instrs.push(Instr::Continue(label));

        self.res.loop_scope.end_frame(scope);
    }

    fn index(&mut self, IndexAst { expr, index }: IndexAst<'arena>) {
        self.unit(expr);
        self.expr(index);
        self.inner.instrs.push(Instr::Index);
    }

    fn break_(
        &mut self,
        BreakAst {
            label,
            expr,
            keyword,
        }: BreakAst<'arena>,
    ) {
        let Some(label) = self.find_loop(label, keyword) else {
            return;
        };

        let instr = if let Some(expr) = expr {
            self.expr(expr);
            Instr::Break(label)
        } else {
            Instr::ValuelessBreak(label)
        };
        self.inner.instrs.push(instr);
    }

    fn continue_(&mut self, ContinueAst { label, keyword }: ContinueAst) {
        let Some(label) = self.find_loop(label, keyword) else {
            return;
        };

        self.inner.instrs.push(Instr::Continue(label));
    }

    fn find_loop(&mut self, label: Option<IdentAst>, keyword: Span) -> Option<Label> {
        let label = label.map(|label| label.ident);
        let res = self.res.loop_scope.resolve(instrs::LabelData(label));

        if res.is_none() {
            self.diags
                .builder(self.modules.files(), self.interner)
                .footer(Severty::Error, "control flow does not have a matching loop")
                .annotation(Severty::Error, keyword, "this");
            self.inner.instrs.push(Instr::Unkown);
        }

        res
    }

    fn field(&mut self, FieldAst { expr, name }: FieldAst<'arena>) {
        self.unit(expr);
        let name = self.intern_ident(name.ident);
        self.inner.instrs.push(Instr::Field(name));
    }

    fn if_(
        &mut self,
        IfAst {
            cond, then, else_, ..
        }: IfAst<'arena>,
    ) {
        self.expr(cond);
        let if_ = self.inner.instrs.push(Instr::Unkown);

        let current_len = self.inner.instrs.len();
        self.expr(then);
        let mut then = self.inner.instrs.len() - current_len;

        if let Some(else_) = else_ {
            let else_instr = self.inner.instrs.push(Instr::Unkown);

            let current_len = self.inner.instrs.len();
            self.expr(else_);
            self.inner.instrs[else_instr] = Instr::Else {
                instr_count: (self.inner.instrs.len() - current_len).try_into().unwrap(),
            };

            then += 1;
        }

        self.inner.instrs[if_] = Instr::If {
            instr_count: then.try_into().unwrap(),
        };
    }

    fn ret(&mut self, expr: Option<ExprAst<'arena>>) {
        if let Some(expr) = expr {
            self.expr(expr);
        }
        self.inner.instrs.push(Instr::Ret {
            has_value: expr.is_some(),
        });
    }

    fn unknown(&mut self, _: Span) {
        self.inner.instrs.push(Instr::Unkown);
    }

    fn expr_list(&mut self, exprs: &[ExprAst<'arena>]) {
        for &expr in exprs {
            self.expr(expr);
        }
    }

    fn binary(&mut self, BinaryAst { op, lhs, rhs }: BinaryAst<'arena>) {
        self.expr(lhs);
        self.expr(rhs);
        self.inner.instrs.push(Instr::Binary(op.kind));
    }

    fn intern_ident(&mut self, ident: InternedStr) -> Ident {
        *self
            .res
            .used_idents
            .entry(ident)
            .or_insert_with(|| self.inner.idents.push(ident))
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, sym: T) -> Ref<T, InstrIndex> {
        self.vec.push(sym)
    }

    pub fn resolve(&self, name: T) -> Option<Ref<T, InstrIndex>>
    where
        T: Eq,
    {
        self.vec
            .iter()
            .rev()
            .find_map(|(id, sym)| (sym == &name).then_some(id))
    }

    pub fn start_frame(&self) -> ScopeFrame {
        ScopeFrame(self.vec.len())
    }

    pub fn end_frame(&mut self, frame: ScopeFrame) {
        self.vec.truncate(frame.0);
    }
}

struct ScopeFrame(usize);

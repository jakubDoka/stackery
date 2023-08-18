use mini_alloc::IdentStr;

use crate::{
    parser::expr::{CallAst, DeclAst, FuncAst, IdentAst, LitAst},
    ArgCount, BinaryAst, BlockAst, CtorAst, DefKind, DefRef, ExprAst, FieldAst, FrameSize,
    FuncArgAst, IfAst, ModItemAst, ModuleRef, OpAst, OpCode, RecordAst, RecordItemAst, ReturnAst,
    Severty, Span, Sym, UnaryAst, UnitAst,
};

use super::{
    CtorMode, CtxSym, Def, Instr, InstrBodyBuilder, InstrKind, InstrRef, InstrRepr,
    ModuleDeclBuilder, ScopeFrame,
};

pub enum DefAst<'arena> {
    Func(FuncAst<'arena>),
    Record(RecordAst<'arena>),
}

impl<'arena> DefAst<'arena> {
    fn keyword(&self) -> Span {
        match self {
            DefAst::Func(FuncAst { keyword, .. }) => *keyword,
            DefAst::Record(RecordAst { keyword, .. }) => *keyword,
        }
    }
}

#[derive(Default)]
struct Res<'arena> {
    module_ref: ModuleRef,
    body: InstrBodyBuilder,
    module: ModuleDeclBuilder,
    compile_queue: Vec<(DefRef, DefAst<'arena>)>,
    scope: Vec<IdentAst>,
}

impl<'ctx> super::InstrBuilder<'ctx> {
    pub fn build(mut self, module: ModuleRef, ast: &[ModItemAst]) -> Option<()> {
        let mut res = Res {
            module_ref: module,
            ..Default::default()
        };

        self.bind_decls(ast, &mut res);
        self.collect_decls(ast, &mut res);
        self.consume_queue(&mut res);

        self.instrs.modules[module] = self.instrs.decls.finish(&mut res.module);

        Some(())
    }

    fn bind_decls(&mut self, ast: &[ModItemAst], res: &mut Res) {
        for item in ast.iter() {
            match item {
                ModItemAst::Decl(DeclAst { name, .. }) => {
                    res.module.decls.push(name.ident.clone());
                    res.scope.push(name.clone());
                }
            }
        }
    }

    fn consume_queue(&mut self, res: &mut Res) {
        while let Some((id, def)) = res.compile_queue.pop() {
            let def = match def {
                DefAst::Func(ast) => self.build_func(ast, res),
                DefAst::Record(ast) => self.build_record(ast, res),
            };

            if let Some(def) = def {
                res.module.defs[id] = def;
            } else {
                res.body.clear();
            }
        }
    }

    fn build_signature<'arena>(
        &mut self,
        FuncAst {
            args, return_ty, ..
        }: &FuncAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<InstrRepr> {
        let frame = ScopeFrame::new(&mut res.scope);

        for FuncArgAst { name, ty, .. } in *args {
            self.build_expr(ty, res);
            res.push_instr(
                InstrKind::Decl {
                    mutable: false,
                    inited: true,
                    typed: false,
                },
                name.span,
            );
            res.scope.push(name.clone());
        }

        if let Some(return_ty) = return_ty {
            self.build_expr(return_ty, res);
        }

        frame.end(&mut res.scope);

        Some(args.len() as InstrRepr)
    }

    fn build_record<'arena>(
        &mut self,
        ast: RecordAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<Def> {
        let base_frame = ScopeFrame::new(&mut res.scope);

        let names = ast.fields.iter().map(|f| f.name.ident.clone());
        let fields = res.module.idents.extend(names);

        for RecordItemAst { name, value, .. } in ast.fields {
            self.build_expr(value, res);
            res.push_instr(
                InstrKind::Decl {
                    mutable: false,
                    inited: true,
                    typed: false,
                },
                name.span,
            );
        }

        base_frame.end(&mut res.scope);
        let body = self.instrs.bodies.finish(&mut res.body);

        Some(Def {
            kind: DefKind::Record {
                kind: ast.kind,
                fields,
            },
            body,
        })
    }

    fn build_func<'arena>(&mut self, ast: FuncAst<'arena>, res: &mut Res<'arena>) -> Option<Def> {
        let arg_count = self.build_signature(&ast, res)?;
        let signature_len = res.body.instrs.len() as InstrRepr;

        let base_frame = ScopeFrame::new(&mut res.scope);
        self.decl_params(ast.args, res);
        self.build_expr(&ast.body, res)?;
        base_frame.end(&mut res.scope);

        let body = self.instrs.bodies.finish(&mut res.body);

        Some(Def {
            kind: DefKind::Func {
                arg_count,
                signature_len,
            },
            body,
        })
    }

    fn decl_params(&mut self, arg: &[FuncArgAst], res: &mut Res) {
        for arg in arg.iter() {
            res.scope.push(arg.name.clone());
        }
    }

    fn build_expr<'arena>(&mut self, expr: &ExprAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        match expr {
            ExprAst::Unit(u) => self.build_unit(u, res),
            ExprAst::Binary(b) => self.build_binary(b, res),
        }
    }

    fn build_unit<'arena>(&mut self, unit: &UnitAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        match unit {
            UnitAst::Literal(l) => self.build_literal(l, res),
            UnitAst::Ident(i) => self.build_ident(i, res),
            UnitAst::Import(i) => self.build_import(i, res),
            UnitAst::If(i) => self.build_if(i, res),
            UnitAst::Block(b) => self.build_block(b.clone(), res),
            UnitAst::Unary(u) => self.build_unary(u, res),
            UnitAst::Call(c) => self.build_call(c, res),
            &UnitAst::Func(f) => self.queue(DefAst::Func(f.clone()), res),
            UnitAst::Decl(d) => self.build_decl(d, res),
            UnitAst::Paren(p) => self.build_expr(p, res),
            UnitAst::Field(f) => self.build_field(f, res),
            UnitAst::Rt(r) => self.build_rt(r, res),
            UnitAst::Return(r) => self.build_return(r, res),
            UnitAst::Record(r) => self.queue(DefAst::Record(r.clone()), res),
            UnitAst::Ctor(c) => self.build_ctor(c, res),
        }
    }

    fn build_ctor<'arena>(
        &mut self,
        CtorAst {
            brace: _,
            ty,
            fill,
            fields,
        }: &CtorAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        for field in fields.iter() {
            if let Some(expr) = &field.value {
                self.build_expr(expr, res)?;
            }
            let name = res.intern_ident(&field.name.ident);
            res.push_instr(
                InstrKind::CtorField(name, field.value.is_none()),
                field.name.span,
            );
        }

        if let Some(Some(expr)) = fill {
            self.build_expr(expr, res)?;
        }

        let fill_mode = CtorMode::from(fill);
        self.build_unit(ty, res)?;
        res.push_instr(
            InstrKind::Ctor(fields.len() as _, fill_mode, true),
            ty.span(),
        );

        Some(())
    }

    fn build_return<'arena>(&mut self, r: &ReturnAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        if let Some(expr) = &r.expr {
            self.build_expr(expr, res)?;
        }
        res.push_instr(InstrKind::Return(r.expr.is_some()), r.keyword);
        Some(())
    }

    fn build_rt<'arena>(
        &mut self,
        r: &crate::ExprAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        self.build_expr(r, res)?;
        res.push_instr(InstrKind::Rt, r.span());
        Some(())
    }

    fn build_literal(&mut self, lit: &LitAst, res: &mut Res) -> Option<()> {
        let kind = res.body.consts.find_or_push(&lit.kind);
        res.push_instr(InstrKind::Const(kind), lit.span);
        Some(())
    }

    fn build_ident(&mut self, ident: &IdentAst, res: &mut Res) -> Option<()> {
        let Some(sym) = res.lookup_sym(&ident.ident) else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "use of undeclared identifier")
                .annotation(Severty::Error, ident.span, "found here")
                .terminate()?;
        };

        res.push_instr(InstrKind::Sym(sym), ident.span);
        Some(())
    }

    fn build_import(&mut self, import: &IdentAst, res: &mut Res) -> Option<()> {
        let Some(module) = self.modules.find_dep(res.module_ref, &import.ident) else {
            let available = self
                .modules
                .deps_of(res.module_ref)
                .map(|dep| self.modules.name_of(dep).as_str())
                .intersperse(", ")
                .collect::<String>();
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "use of invalid module reference")
                .footer(
                    Severty::Note,
                    format_args!("available modules: {available}"),
                )
                .annotation(Severty::Error, import.span, "found here")
                .terminate()?;
        };

        res.push_instr(InstrKind::Module(module), import.span);
        Some(())
    }

    fn build_if<'arena>(
        &mut self,
        IfAst {
            keyword,
            cond,
            then,
            else_,
        }: &IfAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        self.build_expr(cond, res)?;
        let if_ref = res.push_instr(InstrKind::EndIf, *keyword);

        self.build_expr(then, res)?;

        let end_ref = res.push_instr(InstrKind::EndIf, *keyword);
        res.body.instrs[if_ref].kind = InstrKind::If(end_ref);

        if let Some(else_) = else_ {
            self.build_expr(else_, res)?;
            let final_ref = res.push_instr(InstrKind::EndIf, *keyword);
            res.body.instrs[end_ref].kind = InstrKind::Else(final_ref);
        }

        Some(())
    }

    fn build_block<'arena>(
        &mut self,
        block: BlockAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        let Some((last, rest)) = block.exprs.split_last() else {
            return Some(());
        };

        let frame = ScopeFrame::new(&mut res.scope);

        let needs_drop = |expr| {
            !matches!(
                expr,
                &ExprAst::Unit(
                    UnitAst::Decl(..)
                        | UnitAst::Block(BlockAst {
                            trailing_semi: true,
                            ..
                        })
                ) | ExprAst::Binary(BinaryAst {
                    op: OpAst {
                        kind: OpCode::Assign,
                        ..
                    },
                    ..
                })
            )
        };

        for expr in rest.iter() {
            self.build_expr(expr, res)?;
            if needs_drop(expr)
                && !matches!(
                    res.body.instrs.values().last(),
                    Some(Instr {
                        kind: InstrKind::DropScope(.., false),
                        ..
                    })
                )
            {
                res.push_instr(InstrKind::Drop, expr.span());
            }
        }

        self.build_expr(last, res)?;

        let frame_size = frame.size(&res.scope);
        if frame_size > 0 {
            res.push_instr(
                InstrKind::DropScope(
                    frame_size as FrameSize,
                    !block.trailing_semi && needs_drop(last),
                ),
                block.brace,
            );
        }

        frame.end(&mut res.scope);
        Some(())
    }

    fn build_unary<'arena>(
        &mut self,
        _unary: &'arena UnaryAst<'arena>,
        _res: &mut Res<'arena>,
    ) -> Option<()> {
        todo!()
    }

    fn build_call<'arena>(
        &mut self,
        CallAst { caller, args }: &'arena CallAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        for arg in args.iter() {
            self.build_expr(arg, res)?;
        }

        self.build_unit(caller, res)?;

        res.push_instr(InstrKind::Call(args.len() as ArgCount), caller.span());

        Some(())
    }

    fn queue<'arena>(&mut self, def: DefAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        let def_ref = res.module.defs.push(Def::default());
        res.push_instr(InstrKind::Def(def_ref), def.keyword());
        res.compile_queue.push((def_ref, def));
        Some(())
    }

    fn build_decl<'arena>(
        &mut self,
        DeclAst {
            keyword,
            mutable,
            name,
            ty,
            value,
        }: &DeclAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        if let Some(ty) = ty {
            self.build_expr(ty, res)?;
        }
        if let Some(value) = value {
            self.build_expr(value, res)?;
        }

        res.scope.push(name.clone());
        res.push_instr(
            InstrKind::Decl {
                mutable: mutable.is_some(),
                inited: value.is_some(),
                typed: ty.is_some(),
            },
            *keyword,
        );

        Some(())
    }

    fn build_field<'arena>(
        &mut self,
        FieldAst { on, dot: _, name }: &FieldAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        self.build_unit(on, res)?;
        let ident = res.intern_ident(&name.ident);
        res.push_instr(InstrKind::Field(ident), name.span);
        Some(())
    }

    fn build_binary<'arena>(
        &mut self,
        BinaryAst { lhs, rhs, op }: &BinaryAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        self.build_expr(lhs, res)?;
        self.build_expr(rhs, res)?;
        res.push_instr(InstrKind::BinOp(op.kind), op.span);
        Some(())
    }

    fn collect_decls<'arena>(&mut self, ast: &'arena [ModItemAst<'arena>], res: &mut Res<'arena>) {
        for _ in 0..ast.len() {
            res.module.defs.push(Def::default());
        }
        for (i, ast) in ast.iter().enumerate() {
            match ast {
                ModItemAst::Decl(decl) => {
                    if let Some(def) = self.collect_const(decl, res) {
                        res.module.defs[DefRef::from_repr(i as _)] = def;
                    }
                }
            }
        }
    }

    fn collect_const<'arena>(
        &mut self,
        DeclAst {
            keyword,
            mutable,
            name: _,
            ty,
            value,
        }: &'arena DeclAst,
        res: &mut Res<'arena>,
    ) -> Option<Def> {
        let Some(value) = value else {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "top level declarations need to be initialized",
                )
                .annotation(Severty::Error, *keyword, "declared here")
                .terminate()?;
        };

        if let &Some(span) = mutable {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "top level declarations cannot be mutable (yet)",
                )
                .annotation(Severty::Error, span, "declared here")
                .terminate()?;
        }

        if let Some(ty) = ty {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "top level declarations cannot have a type (yet)",
                )
                .annotation(Severty::Error, ty.span(), "declared here")
                .terminate()?;
        }

        let frame = ScopeFrame::new(&res.scope);
        self.build_expr(value, res)?;
        frame.end(&mut res.scope);

        let body = self.instrs.bodies.finish(&mut res.body);

        Some(Def {
            kind: DefKind::Const,
            body,
        })
    }
}

impl<'arena> Res<'arena> {
    fn push_instr(&mut self, kind: InstrKind, span: Span) -> InstrRef {
        self.body.instrs.push(Instr {
            kind,
            span: span.to_pos(),
        })
    }

    fn intern_ident(&mut self, ident: &IdentStr) -> CtxSym {
        self.module.idents.find_or_push(ident)
    }

    fn lookup_sym(&self, ident: &IdentStr) -> Option<Sym> {
        self.scope
            .iter()
            .rposition(|si| si.ident == *ident)
            .map(|s| s as Sym)
    }
}

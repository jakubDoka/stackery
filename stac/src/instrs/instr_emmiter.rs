use mini_alloc::IdentStr;

use crate::{
    parser::expr::{CallAst, DeclAst, FuncAst, IdentAst, LitAst},
    BinaryAst, BlockAst, BuiltInType, ExprAst, FieldAst, FrameSize, FuncArgAst, InstrBodyRef,
    ModItemAst, ModuleRef, Mutable, OpAst, OpCode, Resolved, Severty, Signature, Span, Sym, Type,
    UnaryAst, UnitAst,
};

use super::{
    Decl, Func, FuncRef, Instr, InstrBodyBuilder, InstrKind, InstrRef, ModuleDeclBuilder,
    ScopeFrame, TyResolverCtx,
};

struct Res<'arena> {
    module_ref: ModuleRef,
    body: InstrBodyBuilder,
    module: ModuleDeclBuilder,
    func_queue: Vec<(FuncRef, FuncAst<'arena>)>,
    scope: Vec<IdentAst>,
}

impl<'ctx> super::InstrBuilder<'ctx> {
    pub fn build(mut self, module: ModuleRef, ast: &[ModItemAst]) -> Option<()> {
        let mut res = Res {
            module_ref: module,
            body: Default::default(),
            module: Default::default(),
            func_queue: Default::default(),
            scope: Default::default(),
        };

        self.collect_decls(ast, &mut res);
        self.build_funcs(&mut res);

        self.finish_queue(&mut res);
        self.instrs.modules[module] = self.instrs.decls.finish(&mut res.module);

        Some(())
    }

    fn finish_queue(&mut self, res: &mut Res) {
        if !res.func_queue.is_empty() {
            todo!()
        }
    }

    fn build_funcs(&mut self, res: &mut Res) {
        for (func, ast) in std::mem::take(&mut res.func_queue) {
            if let Some(body) = self.build_func(ast, res) {
                res.module.funcs[func].body = body;
            }
        }
    }

    fn build_signature<'arena>(
        &mut self,
        FuncAst {
            args, return_ty, ..
        }: &FuncAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Signature {
        let args = args
            .iter()
            .map(|arg| self.resolve_ty(&arg.ty, res))
            .collect::<Vec<_>>();

        let ret = return_ty
            .as_ref()
            .map_or(Type::UNIT, |ty| self.resolve_ty(ty, res));

        Signature { args, ret }
    }

    fn resolve<'arena>(
        &mut self,
        expr: &ExprAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<Resolved> {
        self.build_expr(expr, res)?;

        let ctx = TyResolverCtx {
            arch: self.arch,
            module: res.module_ref,
            diags: &mut self.diags,
            instrs: &self.instrs,
            modules: &self.modules,
        };

        let resolved = self
            .resolver
            .resolve(ctx, res.module.view(), res.body.view(), expr.span());
        res.body.clear();
        resolved
    }

    fn resolve_ty<'arena>(&mut self, expr: &ExprAst<'arena>, res: &mut Res<'arena>) -> Type {
        match self.resolve(expr, res) {
            Some(Resolved::Type(ty)) => ty,
            Some(..) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "expected this to evaluate into type")
                    .annotation(Severty::Error, expr.span(), "this evalueates into TODO");
                Type::BuiltIn(BuiltInType::Unknown)
            }
            None => Type::BuiltIn(BuiltInType::Unknown),
        }
    }

    fn build_func<'arena>(
        &mut self,
        ast: FuncAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<InstrBodyRef> {
        let base_frame = ScopeFrame::new(&mut res.scope);
        self.decl_params(ast.args, res);

        self.build_expr(&ast.body, res)?;

        base_frame.end(&mut res.scope);
        Some(self.instrs.bodies.finish(&mut res.body))
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
            UnitAst::Block(b) => self.build_block(b, res),
            UnitAst::Unary(u) => self.build_unary(u, res),
            UnitAst::Call(c) => self.build_call(c, res),
            UnitAst::Func(f) => self.queue_func(f, res),
            UnitAst::Decl(d) => self.build_decl(d, res),
            UnitAst::Paren(p) => self.build_expr(p, res),
            UnitAst::Field(f) => self.build_field(f, res),
        }
    }

    fn build_literal(&mut self, lit: &LitAst, res: &mut Res) -> Option<()> {
        res.push_instr(Resolved::Const(lit.kind.clone()), lit.span);
        Some(())
    }

    fn build_ident(&mut self, ident: &IdentAst, res: &mut Res) -> Option<()> {
        let Some(sym) = res.lookup_sym(&ident.ident) else {
            if let Some(global_sym) = res.lookup_global_sym(&ident.ident) {
                res.push_instr(InstrKind::Res(global_sym), ident.span);
                return Some(());
            }

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

        res.push_instr(Resolved::Module(module), import.span);
        Some(())
    }

    fn build_block<'arena>(
        &mut self,
        block: &'arena BlockAst<'arena>,
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
        res.push_instr(
            InstrKind::DropScope(
                frame_size as FrameSize,
                !block.trailing_semi && needs_drop(last),
            ),
            block.brace,
        );

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
        _call: &'arena CallAst<'arena>,
        _res: &mut Res<'arena>,
    ) -> Option<()> {
        todo!()
    }

    fn queue_func<'arena>(&mut self, func: &FuncAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        let func_ref = res.add_to_compile_queue(func.clone());
        let signature = self.build_signature(func, res);
        res.module.funcs[func_ref].signature = signature;
        res.push_instr(
            Resolved::Func(crate::FuncId {
                module: res.module_ref,
                func: func_ref,
            }),
            func.keyword,
        );
        Some(())
    }

    fn build_decl<'arena>(
        &mut self,
        DeclAst {
            keyword,
            ct,
            mutable,
            name,
            ty,
            value,
        }: &DeclAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        if let Some(value) = value {
            self.build_expr(value, res)?;
        } else {
            let ty = ty
                .as_ref()
                .map_or(Type::BuiltIn(BuiltInType::Unknown), |ty| {
                    self.resolve_ty(ty, res)
                });
            res.push_instr(InstrKind::Uninit(ty), *keyword);
        }

        let mutable = match (ct, mutable) {
            (None, None) => Mutable::False,
            (None, Some(_)) => Mutable::True,
            (&Some(span), None) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(
                        Severty::Warning,
                        "nonmutable declaration does not need to be marked ct",
                    )
                    .annotation(Severty::Warning, span, "found here")
                    .terminate()?;
            }
            (Some(_), Some(_)) => Mutable::CtTrue,
        };

        res.scope.push(name.clone());
        res.push_instr(InstrKind::Decl(mutable), *keyword);

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
        for ast in ast {
            match ast {
                ModItemAst::Decl(decl) => {
                    if let Some(delc) = self.collect_decl(decl, res) {
                        res.module.decls.push(delc);
                    }
                }
            }
        }
    }

    fn collect_decl<'arena>(
        &mut self,
        DeclAst {
            keyword,
            ct,
            mutable,
            name,
            ty: _,
            value,
        }: &'arena DeclAst,
        res: &mut Res<'arena>,
    ) -> Option<Decl> {
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

        if let &Some(span) = ct {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Warning,
                    "top level declarations are always compile time (for now)",
                )
                .annotation(Severty::Warning, span, "declared here")
                .terminate()?;
        }

        let data = self.resolve(value, res)?;

        Some(Decl {
            name: name.clone(),
            data,
        })
    }
}

impl<'arena> Res<'arena> {
    fn add_to_compile_queue(&mut self, f: FuncAst<'arena>) -> FuncRef {
        let func = self.module.funcs.push(Func::default());
        self.func_queue.push((func, f));
        func
    }

    fn push_instr(&mut self, kind: impl Into<InstrKind>, span: Span) -> InstrRef {
        self.body.instrs.push(Instr {
            kind: kind.into(),
            span,
        })
    }

    fn intern_ident(&mut self, ident: &IdentStr) -> IdentStr {
        ident.clone()
    }

    fn lookup_sym(&self, ident: &IdentStr) -> Option<Sym> {
        self.scope
            .iter()
            .rposition(|si| si.ident == *ident)
            .map(|s| s as Sym)
    }

    fn lookup_global_sym(&self, ident: &IdentStr) -> Option<Resolved> {
        self.module
            .decls
            .values()
            .find_map(|decl| (decl.name.ident == *ident).then(|| decl.data.clone()))
    }
}

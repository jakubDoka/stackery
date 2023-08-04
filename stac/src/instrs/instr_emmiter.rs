use mini_alloc::IdentStr;

use crate::{
    parser::expr::{CallAst, DeclAst, FuncAst, IdentAst, LitAst},
    BinaryAst, BlockAst, BuiltInType, ExprAst, FieldAst, FuncArgAst, InstrBodyRef, ModItemAst,
    ModuleRef, Resolved, Severty, Signature, Span, Type, UnaryAst, UnitAst,
};

use super::{
    Decl, Func, FuncRef, Instr, InstrBodyBuilder, InstrKind, InstrRef, InternedIdent,
    ModuleDeclBuilder, ScopeFrame, TyResolverCtx,
};

struct Res<'arena> {
    module_ref: ModuleRef,
    body: InstrBodyBuilder,
    module: ModuleDeclBuilder,
    func_queue: Vec<(FuncRef, FuncAst<'arena>)>,
}

impl<'ctx> super::InstrBuilder<'ctx> {
    pub fn build(mut self, module: ModuleRef, ast: &[ModItemAst]) -> Option<()> {
        let mut res = Res {
            module_ref: module,
            body: Default::default(),
            module: Default::default(),
            func_queue: Default::default(),
        };

        self.collect_decls(ast, &mut res);
        self.build_signatures(&mut res);
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
            if let Some(body) = self.build_func(func, ast, res) {
                res.module.funcs[func].body = body;
            }
        }
    }

    fn build_signatures(&mut self, res: &mut Res) {
        let queue = std::mem::take(&mut res.func_queue);
        for &(func, ref ast) in queue.iter() {
            let signature = self.build_signature(ast, res);
            res.module.funcs[func].signature = signature;
        }
        res.func_queue = queue;
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
            module: res.module_ref,
            scope: &mut res.module.decls,
            diags: &mut self.diags,
            instrs: &self.instrs,
            modules: &self.modules,
        };

        let resolved = self.resolver.resolve(ctx, res.body.view(), expr.span());
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
        func: FuncRef,
        ast: FuncAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<InstrBodyRef> {
        let base_frame = ScopeFrame::new(&mut res.module.decls);
        self.decl_params(func, ast.args, res);

        self.build_expr(&ast.body, res)?;

        base_frame.end(&mut res.module.decls);
        Some(self.instrs.bodies.finish(&mut res.body))
    }

    fn decl_params(&mut self, func: FuncRef, arg: &[FuncArgAst], res: &mut Res) {
        let signature = &res.module.funcs[func].signature;
        for (arg, ty) in arg.iter().zip(signature.args.iter()) {
            let decl = Decl {
                name: arg.name.clone(),
                data: Resolved::Type(ty.clone()),
            };
            res.module.decls.push(decl);
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
        let kind = res.body.consts.push(lit.kind.clone());
        res.push_instr(InstrKind::Const(kind), lit.span);
        Some(())
    }

    fn build_ident(&mut self, ident: &IdentAst, res: &mut Res) -> Option<()> {
        let Some(sym) = super::lookup_sym(&mut res.module.decls, &ident.ident) else {
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
        let Some(module) = self
            .modules
            .deps_of(res.module_ref)
            .find(|&m| self.modules.name_of(m) == &import.ident)
        else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "use of invalid module reference")
                .annotation(Severty::Error, import.span, "found here")
                .terminate()?;
        };

        res.push_instr(InstrKind::Module(module), import.span);
        Some(())
    }

    fn build_block<'arena>(
        &mut self,
        block: &'arena BlockAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        todo!()
    }

    fn build_unary<'arena>(
        &mut self,
        unary: &'arena UnaryAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        todo!()
    }

    fn build_call<'arena>(
        &mut self,
        call: &'arena CallAst<'arena>,
        res: &mut Res<'arena>,
    ) -> Option<()> {
        todo!()
    }

    fn queue_func<'arena>(&mut self, func: &FuncAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        let func_ref = res.add_to_compile_queue(func.clone());
        let signature = self.build_signature(func, res);
        res.module.funcs[func_ref].signature = signature;
        res.push_instr(InstrKind::Func(func_ref), func.keyword);
        Some(())
    }

    fn build_decl<'arena>(&mut self, decl: &DeclAst<'arena>, res: &mut Res<'arena>) -> Option<()> {
        todo!()
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

    fn push_instr(&mut self, kind: InstrKind, _span: Span) -> InstrRef {
        self.body.instrs.push(Instr { kind, offset: 0 })
    }

    fn intern_ident(&mut self, ident: &IdentStr) -> InternedIdent {
        let existing = self
            .body
            .idents
            .iter()
            .find_map(|(i, s)| (s == ident).then_some(i));

        existing.unwrap_or_else(|| self.body.idents.push(ident.clone()))
    }
}

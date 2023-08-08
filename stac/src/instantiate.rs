mod interp_impl;
mod layout;

use crate::{
    Diagnostics, FuncRef, InstrKind, Instrs, ModuleRef, Modules, Resolved, Resolver, Type,
};

pub use self::layout::Layout;

#[derive(Clone)]
pub enum Local {
    Ct(Resolved),
    Rt(Type),
}

#[derive(Default)]
pub struct Ir {
    pub instrs: Vec<InstrKind>,
}

pub struct Interpreter<'ctx> {
    diags: &'ctx mut Diagnostics,
    modules: &'ctx Modules,
    instrs: &'ctx Instrs,
}

pub struct Entry {
    pub module: ModuleRef,
    pub func: FuncRef,
    pub inputs: Vec<Local>,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(diags: &'ctx mut Diagnostics, modules: &'ctx Modules, instrs: &'ctx Instrs) -> Self {
        Self {
            diags,
            modules,
            instrs,
        }
    }
}

pub fn resolver() -> impl Resolver {
    struct ResolverImpl;

    impl Resolver for ResolverImpl {
        fn resolve(
            &mut self,
            ctx: crate::TyResolverCtx,
            body: crate::InstrBody,
            span: crate::Span,
        ) -> Option<Resolved> {
            let mut interp = Interpreter::new(ctx.diags, ctx.modules, ctx.instrs);
            let decls = ctx.instrs.decls_of(ctx.module);
            interp.ct_eval(span, body, decls)
        }
    }

    ResolverImpl
}

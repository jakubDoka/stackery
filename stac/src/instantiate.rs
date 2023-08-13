mod interp_impl;
mod layout;

use crate::{
    Diagnostics, FrameSize, FuncId, InstrKind, Instrs, LitKindAst, Modules, Mutable, OpCode, Ref,
    Resolved, Resolver, Returns, Sym, Type, VecStore,
};

pub use self::layout::Layout;

type SubsRepr = u16;
pub type SubsRef = Ref<Type, SubsRepr>;
pub type IrTypes = VecStore<Type, SubsRepr>;
pub type FuncInst = u32;
pub type FieldIndex = u16;

#[derive(Clone)]
pub enum Local {
    Ct(Resolved),
    Rt(Type),
}

#[derive(Clone)]
pub enum FinstKind {
    Const(LitKindAst),
    Sym(Sym),
    Call(FuncInst),
    BinOp(OpCode),
    Decl(Mutable),
    Field(FieldIndex),

    Drop,
    Uninit,
    DropScope(FrameSize, Returns),
}

#[derive(Default)]
pub struct Ir {
    pub instrs: Vec<(InstrKind, SubsRef)>,
    pub types: IrTypes,
}

pub struct Interpreter<'ctx> {
    arch: Layout,
    diags: &'ctx mut Diagnostics,
    modules: &'ctx Modules,
    instrs: &'ctx Instrs,
}

pub struct Entry {
    pub id: FuncId,
    pub inputs: Vec<Local>,
    pub ret: Type,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(
        arch: Layout,
        diags: &'ctx mut Diagnostics,
        modules: &'ctx Modules,
        instrs: &'ctx Instrs,
    ) -> Self {
        Self {
            arch,
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
            decls: crate::ModuleDecl,
            body: crate::InstrBody,
            span: crate::Span,
        ) -> Option<Resolved> {
            let mut interp = Interpreter::new(ctx.arch, ctx.diags, ctx.modules, ctx.instrs);
            interp.ct_eval(span, ctx.module, body, decls)
        }
    }

    ResolverImpl
}

#[cfg(test)]
mod test {
    use crate::{
        print_cases, BuiltInType, Diagnostics, FuncId, Instrs, Interpreter, Layout, LoaderMock,
        Modules, Type,
    };

    fn perform_test(source: &str, ctx: &mut String) -> Option<()> {
        let mut mods = Modules::new();
        let loader = LoaderMock::new(source);
        let meta = crate::load_modules(&mut mods, loader, ctx)?;

        let mut instrs = Instrs::new();
        let resolver = crate::resolver();
        crate::parse_modules(&mods, &meta, &mut instrs, resolver, ctx);

        let Some(entry) = instrs.entry_of(meta.root()) else {
            ctx.push_str("no entry point\n");
            return None;
        };

        let mut diags = Diagnostics::default();
        let mut interp = Interpreter::new(Layout::ARCH_64, &mut diags, &mods, &instrs);
        let ir = interp.eval(
            &crate::Entry {
                id: FuncId {
                    module: meta.root(),
                    func: entry,
                },
                inputs: vec![],
                ret: Type::BuiltIn(BuiltInType::I32),
            },
            &mut Vec::new(),
        );

        if !diags.view().is_empty() {
            ctx.push_str("diagnostics:\n");
            ctx.push_str(diags.view());
        }

        for (instr, ty) in ir.instrs.iter() {
            crate::display_instr(instr, ctx);
            ctx.push_str(" : ");
            ctx.push_str(ir.types[*ty].to_string().as_str());
            ctx.push('\n');
        }

        Some(())
    }

    print_cases! { perform_test:
        simple_main "
            let bi = :{bi}
            let main = fn(): bi.i32 0
        ";
        simple_fold "
            let bi = :{bi}
            let main = fn(): bi.i32.f 1 + 1
        ";
        int_size "
            let bi = :{bi}
            let main = fn(): bi.i32 bi.i64.size + 1
        ";
        local_immutable_var "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                let x = 1
                x + 1
            }
        ";
        local_mutable_var "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                let ct mut x = 1
                x += 1
                x
            }
        ";
        multiple_locals "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                let ct mut x = 1
                let ct mut y = 2
                { let x = 3 y += x }
                x += y + x
                y += x + y
                x + y
            }
        ";
        no_compile_time_mutation "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                let mut x = 1
                x += 1
                x
            }
        ";
        function_call "
            let bi = :{bi}
            let fun = fn(): bi.i32 1
            let main = fn(): bi.i32 fun()
        ";
    }
}

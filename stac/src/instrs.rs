use mini_alloc::IdentStr;

use crate::{
    gen_storage_group, loader::ModuleShadowStore, parser::expr::IdentAst, Diagnostics, IntType,
    LitKindAst, ModuleRef, Modules, OpCode, Ref, Signature, Span, Type, VecStore,
};

mod instr_emmiter;

type InstrRepr = u16;
pub type InstrRef = Ref<Instr, InstrRepr>;
pub type Const = Ref<LitKindAst, InstrRepr>;
pub type Sym = Ref<Decl, InstrRepr>;
pub type FuncRef = Ref<Func, InstrRepr>;
type Scope = VecStore<Decl, InstrRepr>;
type InternedIdent = Ref<IdentStr, InstrRepr>;

pub type SourceOffset = i16;

#[derive(Copy, Clone)]
pub enum InstrKind {
    Uninit,
    Const(Const),
    Sym(Sym),
    Func(FuncRef),
    Module(ModuleRef),
    BinOp(OpCode),
    Field(InternedIdent),
}

#[derive(Copy, Clone)]
pub struct Instr {
    kind: InstrKind,
    offset: SourceOffset,
}

#[derive(Clone)]
pub struct Decl {
    name: IdentAst,
    data: Resolved,
}

gen_storage_group! {
    InstrBodyes InstrBodyRef InstrBody InstrBodyBuilder 'a {
        instrs: Instr,
        consts: LitKindAst,
        idents: IdentStr,
    }
}

gen_storage_group! {
    ModuleDecls ModuleDeclRef ModuleDecl ModuleDeclBuilder 'a {
        decls: Decl,
        funcs: Func,
    }
}

#[derive(Clone, Default)]
pub struct Func {
    signature: Signature,
    body: InstrBodyRef,
}

#[derive(Default)]
pub struct Instrs {
    bodies: InstrBodyes,
    decls: ModuleDecls,
    modules: ModuleShadowStore<ModuleDeclRef>,
}

impl Instrs {
    pub fn new() -> Self {
        Self::default().init()
    }

    fn init(mut self) -> Self {
        let mut builder = ModuleDeclBuilder::default();
        builder.mount_integers();
        self.modules[Modules::BUILTIN] = self.decls.finish(&mut builder);

        self
    }
}

impl ModuleDeclBuilder {
    fn mount_integers(&mut self) {
        for (&ty, &nm) in IntType::INT_TYPES.iter().zip(IntType::INT_TYPE_NAMES) {
            self.decls.push(Decl {
                name: IdentAst {
                    ident: IdentStr::from_str(nm),
                    span: Span::new(0, 0, Modules::BUILTIN),
                },
                data: Resolved::Type(Type::BuiltIn(crate::BuiltInType::Int(ty))),
            });
        }
    }
}

pub struct TyResolverCtx<'ctx> {
    pub module: ModuleRef,
    pub instrs: &'ctx Instrs,
    pub scope: &'ctx mut Scope,
    pub diags: &'ctx mut Diagnostics,
    pub modules: &'ctx Modules,
}

impl<'ctx> TyResolverCtx<'ctx> {
    pub fn stack_borrow(&mut self) -> TyResolverCtx {
        TyResolverCtx {
            module: self.module,
            instrs: self.instrs,
            scope: self.scope,
            diags: self.diags,
            modules: self.modules,
        }
    }
}

pub fn lookup_sym(s: &Scope, name: &str) -> Option<Sym> {
    s.iter()
        .find_map(|(id, decl)| (decl.name.ident.as_str() == name).then_some(id))
}

#[must_use]
pub struct ScopeFrame(usize);

impl ScopeFrame {
    pub fn new(s: &mut Scope) -> Self {
        Self(s.len())
    }

    pub fn end(self, s: &mut Scope) {
        s.truncate(self.0);
    }
}

pub trait Resolver {
    fn resolve(&mut self, ctx: TyResolverCtx, body: InstrBody, span: Span) -> Option<Resolved>;
}

#[derive(Clone)]
pub enum Resolved {
    Type(Type),
    Func(FuncRef),
    Module(ModuleRef),
    Const(LitKindAst),
}

pub struct InstrBuilder<'ctx> {
    instrs: &'ctx mut Instrs,
    modules: &'ctx Modules,
    diags: &'ctx mut Diagnostics,
    resolver: &'ctx mut dyn Resolver,
}

impl<'ctx> InstrBuilder<'ctx> {
    pub fn new(
        instrs: &'ctx mut Instrs,
        modules: &'ctx Modules,
        diags: &'ctx mut Diagnostics,
        resolver: &'ctx mut dyn Resolver,
    ) -> Self {
        Self {
            instrs,
            modules,
            diags,
            resolver,
        }
    }
}

#[cfg(test)]
mod test {
    use mini_alloc::{ArenaBase, DiverBase, IdentStr};
    use pollster::FutureExt;

    use crate::{
        instrs::InstrKind, print_cases, Instr, InstrBody, Instrs, ModuleRef, Modules, Parser,
        Resolved, Resolver,
    };

    #[derive(Default)]
    struct TyResolverImpl {}

    impl Resolver for TyResolverImpl {
        fn resolve(
            &mut self,
            ctx: crate::TyResolverCtx,
            body: crate::InstrBody,
            _span: crate::Span,
        ) -> Option<super::Resolved> {
            let mut stack = Vec::<Resolved>::new();

            for &instr in body.instrs {
                let value = match instr.kind {
                    InstrKind::Uninit => todo!(),
                    InstrKind::Const(c) => Resolved::Const(body.consts[c].clone()),
                    InstrKind::Sym(sym) => ctx.scope[sym].data.clone(),
                    InstrKind::Module(module) => Resolved::Module(module),
                    InstrKind::BinOp(_) => todo!(),
                    InstrKind::Field(i) => match stack.pop().unwrap() {
                        Resolved::Const(_) => todo!(),
                        Resolved::Type(_) => todo!(),
                        Resolved::Func(_) => todo!(),
                        Resolved::Module(m) => {
                            let view = ctx.instrs.decls.view(ctx.instrs.modules[m]);
                            view.decls
                                .iter()
                                .find(|d| d.name.ident == body.idents[i])
                                .unwrap()
                                .data
                                .clone()
                        }
                    },
                    InstrKind::Func(i) => Resolved::Func(i),
                };

                stack.push(value);
            }

            Some(stack.pop().unwrap())
        }
    }

    fn display_id(prefix: &str, id: usize, ctx: &mut String) {
        ctx.push_str(prefix);
        ctx.push_str(id.to_string().as_str());
    }

    fn display_instr(instr: Instr, body: &InstrBody, ctx: &mut String) {
        match instr.kind {
            InstrKind::Uninit => ctx.push_str("uninit"),
            InstrKind::Const(c) => {
                ctx.push_str("const ");
                ctx.push_str(&body.consts[c].to_string());
            }
            InstrKind::Sym(s) => display_id("sym", s.index(), ctx),
            InstrKind::Module(m) => display_id("mod", m.index(), ctx),
            InstrKind::BinOp(op) => ctx.push_str(op.name()),
            InstrKind::Field(f) => {
                ctx.push_str(". ");
                ctx.push_str(body.idents[f].as_str());
            }
            InstrKind::Func(f) => display_id("func", f.index(), ctx),
        }
    }

    fn display_module(module: ModuleRef, modules: &Modules, instrs: &Instrs, ctx: &mut String) {
        let name = modules.name_of(module);
        let view = instrs.decls.view(instrs.modules[module]);

        ctx.push_str("module ");
        ctx.push_str(name.as_str());
        ctx.push_str(":\n");

        for decl in view.decls.iter() {
            ctx.push_str("  ");
            ctx.push_str(decl.name.ident.as_str());
            ctx.push_str(" = ");

            let func = match decl.data.clone() {
                Resolved::Func(func) => func,
                Resolved::Type(ty) => {
                    ctx.push_str(&ty.to_string());
                    ctx.push('\n');
                    continue;
                }
                Resolved::Module(m) => {
                    ctx.push_str(":{");
                    ctx.push_str(modules.name_of(m).as_str());
                    ctx.push_str("}\n");
                    continue;
                }
                Resolved::Const(c) => {
                    ctx.push_str(&c.to_string());
                    ctx.push('\n');
                    continue;
                }
            };

            let func = &view.funcs[func];
            let scope_offset = view.decls.len();

            ctx.push('(');
            for (i, arg) in func.signature.args.iter().enumerate() {
                if i != 0 {
                    ctx.push_str(", ");
                }
                display_id("sim", scope_offset + i, ctx);
                ctx.push_str(": ");
                ctx.push_str(&arg.to_string());
            }
            ctx.push_str("): ");
            ctx.push_str(&func.signature.ret.to_string());
            ctx.push_str(":\n");

            let body_view = instrs.bodies.view(func.body);
            for &instr in body_view.instrs.iter() {
                ctx.push_str("    ");
                display_instr(instr, &body_view, ctx);
                ctx.push('\n');
            }
        }
    }

    fn prefrom_test(source: &str, ctx: &mut String) {
        let mut diags = crate::Diagnostics::default();
        let mut modules = crate::Modules::new();
        let mut loader = crate::LoaderMock::new(source);

        macro bail($message:literal $control:ident) {
            ctx.push_str($message);
            ctx.push('\n');
            ctx.push_str(diags.diagnostic_view());
            diags.clear();
            $control;
        }

        let mut arena = ArenaBase::new(1000);
        let scope = arena.scope();
        let mut diver = DiverBase::new(1000);

        let root = IdentStr::from_str("root");
        let loader_ctx = crate::ModuleLoader::new(&mut loader, &mut modules, &mut diags);
        let Some(meta) = loader_ctx.update(root).block_on() else {
            bail!("failed to load modules" return);
        };

        let mut instrs = crate::Instrs::new();
        let mut resolver = TyResolverImpl::default();

        for &module in meta.order() {
            let parser = Parser::new(modules.files(), module, &mut diags, &scope);
            let Some(items) = parser.parse(diver.dive()) else {
                bail!("failed to parse module" continue);
            };

            let builder =
                crate::InstrBuilder::new(&mut instrs, &modules, &mut diags, &mut resolver);

            if builder.build(module, items).is_none() || !diags.diagnostic_view().is_empty() {
                bail!("failed to build module" continue);
            }

            display_module(module, &modules, &instrs, ctx);
        }
    }

    print_cases! { prefrom_test:
        function "let main = fn(): :{bi}.i32 0";
        arguments "let main = fn(a: :{bi}.i32): :{bi}.i32 a * 2";
        import "let bi = :{bi} let drop = fn(): bi.i32 42";
    }
}

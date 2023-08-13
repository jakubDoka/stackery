use mini_alloc::IdentStr;

use crate::{
    gen_storage_group, loader::ModuleShadowStore, parser::expr::IdentAst, Diagnostics, IntType,
    Layout, LitKindAst, ModuleRef, Modules, OpCode, Ref, Signature, Span, Type,
};

mod instr_emmiter;

type InstrRepr = u16;
pub type ArgCount = u16;
pub type Returns = bool;
pub type FrameSize = u16;
pub type Sym = InstrRepr;
pub type InstrRef = Ref<Instr, InstrRepr>;
pub type Const = LitKindAst;
pub type FuncRef = Ref<Func, InstrRepr>;
pub type TypeRef = Type;
type CtxSym = IdentStr;

pub type SourceOffset = Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mutable {
    False,
    True,
    CtTrue,
}

#[derive(Clone, Debug)]
pub enum InstrKind {
    Uninit(TypeRef),
    Sym(Sym),
    Res(Resolved),
    BinOp(OpCode),
    Call(ArgCount),
    Field(CtxSym),
    Decl(Mutable),
    Drop,
    DropScope(FrameSize, Returns),
}

impl From<Resolved> for InstrKind {
    fn from(res: Resolved) -> Self {
        Self::Res(res)
    }
}

#[derive(Clone)]
pub struct Instr {
    pub kind: InstrKind,
    pub span: SourceOffset,
}

#[derive(Clone)]
pub struct Decl {
    pub name: IdentAst,
    pub data: Resolved,
}

gen_storage_group! {
    InstrBodyes InstrBodyRef InstrBody InstrBodyBuilder 'a {
        instrs: Instr,
        consts: LitKindAst,
        idents: IdentStr,
        types: Type,
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
    pub signature: Signature,
    pub body: InstrBodyRef,
}

#[derive(Default)]
pub struct Instrs {
    bodies: InstrBodyes,
    decls: ModuleDecls,
    modules: ModuleShadowStore<ModuleDeclRef>,
}

impl Instrs {
    const ENTRY_NAME: &'static str = "main";

    pub fn new() -> Self {
        Self::default().init()
    }

    fn init(mut self) -> Self {
        let mut builder = ModuleDeclBuilder::default();
        builder.mount_integers();
        self.modules[Modules::BUILTIN] = self.decls.finish(&mut builder);

        self
    }

    pub fn body_of(&self, id: FuncId) -> InstrBody {
        let module_view = self.decls.view(self.modules[id.module]);
        self.bodies.view(module_view.funcs[id.func].body)
    }

    pub fn decls_of(&self, module: ModuleRef) -> ModuleDecl {
        self.decls.view(self.modules[module])
    }

    pub fn entry_of(&self, root: ModuleRef) -> Option<FuncRef> {
        self.decls_of(root)
            .decls
            .iter()
            .find_map(|decl| match decl.data {
                Resolved::Func(id) if decl.name.ident.as_str() == Self::ENTRY_NAME => Some(id.func),
                _ => None,
            })
    }

    pub(crate) fn sig_of(&self, f: FuncId) -> &Signature {
        &self.decls.view(self.modules[f.module]).funcs[f.func].signature
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
    pub arch: Layout,
    pub instrs: &'ctx Instrs,
    pub diags: &'ctx mut Diagnostics,
    pub modules: &'ctx Modules,
}

impl<'ctx> TyResolverCtx<'ctx> {
    pub fn stack_borrow(&mut self) -> TyResolverCtx {
        TyResolverCtx {
            module: self.module,
            arch: self.arch,
            instrs: self.instrs,
            diags: self.diags,
            modules: self.modules,
        }
    }
}

#[must_use]
pub struct ScopeFrame(usize);

impl ScopeFrame {
    pub fn new<T>(s: &mut Vec<T>) -> Self {
        Self(s.len())
    }

    pub fn size<T>(&self, s: &Vec<T>) -> FrameSize {
        (s.len() - self.0) as FrameSize
    }

    pub fn end<T>(self, s: &mut Vec<T>) {
        s.truncate(self.0);
    }
}

pub trait Resolver {
    fn resolve(
        &mut self,
        ctx: TyResolverCtx,
        decls: ModuleDecl,
        body: InstrBody,
        span: Span,
    ) -> Option<Resolved>;
}

#[derive(Clone, Debug)]
pub enum Resolved {
    Type(Type),
    Func(FuncId),
    Module(ModuleRef),
    Const(LitKindAst),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FuncId {
    pub module: ModuleRef,
    pub func: FuncRef,
}

pub struct InstrBuilder<'ctx> {
    arch: Layout,
    instrs: &'ctx mut Instrs,
    modules: &'ctx Modules,
    diags: &'ctx mut Diagnostics,
    resolver: &'ctx mut dyn Resolver,
}

impl<'ctx> InstrBuilder<'ctx> {
    pub fn new(
        arch: Layout,
        instrs: &'ctx mut Instrs,
        modules: &'ctx Modules,
        diags: &'ctx mut Diagnostics,
        resolver: &'ctx mut dyn Resolver,
    ) -> Self {
        Self {
            arch,
            instrs,
            modules,
            diags,
            resolver,
        }
    }
}

#[cfg(test)]
pub mod instr_test_util {
    use mini_alloc::{ArenaBase, DiverBase};

    use crate::{
        bail, instrs::InstrKind, Instrs, Layout, ModuleMeta, ModuleRef, Modules, Mutable, Parser,
        Resolved, Resolver,
    };

    #[derive(Default)]
    pub struct TyResolverMock {}

    impl Resolver for TyResolverMock {
        fn resolve(
            &mut self,
            ctx: crate::TyResolverCtx,
            _: crate::ModuleDecl,
            body: crate::InstrBody,
            _span: crate::Span,
        ) -> Option<super::Resolved> {
            let mut stack = Vec::<Resolved>::new();

            for instr in body.instrs {
                let value = match &instr.kind {
                    InstrKind::Field(i) => match stack.pop().unwrap() {
                        Resolved::Module(m) => {
                            let view = ctx.instrs.decls.view(ctx.instrs.modules[m]);
                            view.decls
                                .iter()
                                .find(|d| d.name.ident == *i)
                                .unwrap()
                                .data
                                .clone()
                        }
                        _ => todo!(),
                    },
                    InstrKind::Res(r) => r.clone(),
                    _ => todo!(),
                };

                stack.push(value);
            }

            Some(stack.pop().unwrap())
        }
    }

    pub fn display_id(prefix: &str, id: usize, ctx: &mut String) {
        ctx.push_str(prefix);
        ctx.push_str(id.to_string().as_str());
    }

    pub fn display_instr(instr: &InstrKind, ctx: &mut String) {
        match instr {
            InstrKind::Uninit(ty) => {
                ctx.push_str("uninit ");
                ctx.push_str(&ty.to_string());
            }
            &InstrKind::Sym(s) => display_id("sym", s as usize, ctx),
            InstrKind::BinOp(op) => ctx.push_str(op.name()),
            InstrKind::Field(f) => {
                ctx.push_str(". ");
                ctx.push_str(f.as_str());
            }
            InstrKind::Res(r) => match r {
                Resolved::Type(t) => ctx.push_str(&t.to_string()),
                Resolved::Func(f) => display_id("func", f.func.index(), ctx),
                Resolved::Module(m) => display_id("mod", m.index(), ctx),
                Resolved::Const(c) => ctx.push_str(&c.to_string()),
            },
            InstrKind::Decl(Mutable::False) => ctx.push_str("decl"),
            InstrKind::Decl(Mutable::True) => ctx.push_str("decl mut"),
            InstrKind::Decl(Mutable::CtTrue) => ctx.push_str("decl ct mut"),
            InstrKind::Drop => ctx.push_str("drop"),
            InstrKind::DropScope(s, r) => {
                ctx.push_str("drop scope ");
                ctx.push_str(s.to_string().as_str());
                ctx.push_str(if *r { " returns" } else { "" });
            }
            InstrKind::Call(ac) => {
                ctx.push_str("call ");
                ctx.push_str(ac.to_string().as_str());
            }
        }
    }

    pub fn display_module(module: ModuleRef, modules: &Modules, instrs: &Instrs, ctx: &mut String) {
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

            let func = &view.funcs[func.func];

            ctx.push('(');
            for (i, arg) in func.signature.args.iter().enumerate() {
                if i != 0 {
                    ctx.push_str(", ");
                }
                display_id("sim", i, ctx);
                ctx.push_str(": ");
                ctx.push_str(&arg.to_string());
            }
            ctx.push_str("): ");
            ctx.push_str(&func.signature.ret.to_string());
            ctx.push_str(":\n");

            let body_view = instrs.bodies.view(func.body);
            for instr in body_view.instrs.iter() {
                ctx.push_str("    ");
                display_instr(&instr.kind, ctx);
                ctx.push('\n');
            }
        }
    }

    pub fn parse_modules(
        mods: &Modules,
        meta: &ModuleMeta,
        instrs: &mut Instrs,
        mut resolver: impl Resolver,
        ctx: &mut String,
    ) {
        let mut diags = crate::Diagnostics::default();

        let mut arena = ArenaBase::new(1000);
        let scope = arena.scope();
        let mut diver = DiverBase::new(1000);

        for &module in meta.order() {
            let parser = Parser::new(mods.files(), module, &mut diags, &scope);
            let Some(items) = parser.parse(diver.dive()) else {
                bail!("failed to parse module" diags ctx continue);
            };

            let builder =
                crate::InstrBuilder::new(Layout::ARCH_64, instrs, mods, &mut diags, &mut resolver);

            if builder.build(module, items).is_none() || !diags.view().is_empty() {
                bail!("failed to build module" diags ctx continue);
            }

            display_module(module, &mods, &instrs, ctx);
        }
    }
}

#[cfg(test)]
mod test {

    use crate::{load_modules, parse_modules, print_cases, TyResolverMock};

    fn prefrom_test(source: &str, ctx: &mut String) {
        let mut mods = crate::Modules::new();
        let loader = crate::LoaderMock::new(source);
        let Some(meta) = load_modules(&mut mods, loader, ctx) else {
            return;
        };

        let mut instrs = crate::Instrs::new();
        let resolver = TyResolverMock::default();
        parse_modules(&mods, &meta, &mut instrs, resolver, ctx);
    }

    print_cases! { prefrom_test:
        function "let main = fn(): :{bi}.i32 0";
        arguments "let main = fn(a: :{bi}.i32): :{bi}.i32 a * 2";
        import "let bi = :{bi} let drop = fn(): bi.i32 42";
    }
}

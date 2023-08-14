use std::ops::IndexMut;

use mini_alloc::IdentStr;

use crate::{
    gen_storage_group, loader::ModuleShadowStore, parser::expr::IdentAst, BuiltInType, Diagnostics,
    FuncType, Layout, LitKindAst, ModuleRef, Modules, OpCode, Pos, Ref, Span, Type, VecStore,
};

mod instr_emmiter;

type InstrRepr = u16;

pub type ArgCount = InstrRepr;
pub type Returns = bool;
pub type Scoped = bool;
pub type FrameSize = InstrRepr;
pub type Sym = InstrRepr;

pub type InstrRef = Ref<Instr, InstrRepr>;
pub type Const = LitKindAst;
pub type ConstRef = Ref<Const, InstrRepr>;
pub type FuncRef = Ref<Func, InstrRepr>;
pub type TypeRef = Ref<Type, InstrRepr>;
pub type CtxSym = Ref<IdentStr, InstrRepr>;
pub type SourceOffset = Pos;
pub type IrTypes = VecStore<Type, InstrRepr>;

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
    Module(ModuleRef),
    Const(ConstRef),
    Type(TypeRef),
    BinOp(OpCode),
    Call(ArgCount),
    Field(CtxSym),
    Decl(Mutable),
    Drop,
    DropScope(FrameSize, Returns),
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

impl InstrBodyBuilder {
    pub fn intern_ty(&mut self, ty: &Type) -> TypeRef {
        match ty {
            &Type::BuiltIn(b) => TypeRef::from_repr(b as InstrRepr),
            other => {
                let r = self.types.find_or_push(other).index();
                TypeRef::from_repr((r + BuiltInType::COUNT) as InstrRepr)
            }
        }
    }
}

impl<'a> InstrBody<'a> {
    pub fn get_ty(&self, ty: TypeRef) -> Type {
        self.types.get_ty(ty)
    }
}

pub trait TypeRefIndex: IndexMut<TypeRef, Output = Type> {
    fn get_ty(&self, ty: TypeRef) -> Type {
        match BuiltInType::from_index(ty.index()) {
            Ok(b) => Type::BuiltIn(b),
            Err(index) => self[TypeRef::from_repr(index as InstrRepr)].clone(),
        }
    }
}

impl<T: IndexMut<TypeRef, Output = Type> + ?Sized> TypeRefIndex for T {}

gen_storage_group! {
    ModuleDecls ModuleDeclRef ModuleDecl ModuleDeclBuilder 'a {
        decls: Decl,
        funcs: Func,
    }
}

#[derive(Clone, Default)]
pub struct Func {
    pub param_count: ArgCount,
    /// Measured in instructions,
    pub signature_len: InstrRepr,
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
                Resolved::Type(Type::Func(FuncType::Static(id)))
                    if decl.name.ident.as_str() == Self::ENTRY_NAME =>
                {
                    Some(id.func)
                }
                _ => None,
            })
    }
}

impl ModuleDeclBuilder {
    fn mount_integers(&mut self) {
        for ty in BuiltInType::ALL {
            self.decls.push(Decl {
                name: IdentAst {
                    ident: IdentStr::from_str(ty.name()),
                    span: Span::new(0, 0, Modules::BUILTIN),
                },
                data: Resolved::Type(Type::BuiltIn(ty)),
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
        bail, instrs::InstrKind, FuncType, InstrBody, Instrs, Layout, ModuleMeta, ModuleRef,
        Modules, Mutable, Parser, Resolved, Resolver, Type,
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
                                .find(|d| d.name.ident == body.idents[*i])
                                .unwrap()
                                .data
                                .clone()
                        }
                        _ => todo!(),
                    },
                    &InstrKind::Module(module) => Resolved::Module(module),
                    &InstrKind::Const(c) => Resolved::Const(body.consts[c].clone()),
                    &InstrKind::Type(ty) => Resolved::Type(body.get_ty(ty)),
                    &InstrKind::Drop => stack.pop().unwrap(),
                    i => todo!("{:?}", i),
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

    pub fn display_instr(instr: &InstrKind, body: &InstrBody, ctx: &mut String) {
        match instr.clone() {
            InstrKind::Uninit(ty) => {
                ctx.push_str("uninit ");
                ctx.push_str(&body.types[ty].to_string());
            }
            InstrKind::Sym(s) => display_id("sym", s as usize, ctx),
            InstrKind::BinOp(op) => ctx.push_str(op.name()),
            InstrKind::Field(f) => {
                ctx.push_str(". ");
                ctx.push_str(&body.idents[f].to_string());
            }
            InstrKind::Type(t) => ctx.push_str(&body.get_ty(t).to_string()),
            InstrKind::Module(m) => display_id("mod", m.index(), ctx),
            InstrKind::Const(c) => ctx.push_str(&body.consts[c].to_string()),
            InstrKind::Decl(Mutable::False) => ctx.push_str("decl"),
            InstrKind::Decl(Mutable::True) => ctx.push_str("decl mut"),
            InstrKind::Decl(Mutable::CtTrue) => ctx.push_str("decl ct mut"),
            InstrKind::Drop => ctx.push_str("drop"),
            InstrKind::DropScope(s, r) => {
                ctx.push_str("drop scope ");
                ctx.push_str(s.to_string().as_str());
                ctx.push_str(if r { " returns" } else { "" });
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
                Resolved::Type(Type::Func(FuncType::Static(func))) => func.func,
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

            let body_view = instrs.bodies.view(func.body);

            ctx.push_str("sig:\n");
            for instr in body_view.instrs.iter().take(func.signature_len as usize) {
                ctx.push_str("    ");
                display_instr(&instr.kind, &body_view, ctx);
                ctx.push('\n');
            }

            ctx.push_str("  body:\n");
            for instr in body_view.instrs.iter().skip(func.signature_len as usize) {
                ctx.push_str("    ");
                display_instr(&instr.kind, &body_view, ctx);
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

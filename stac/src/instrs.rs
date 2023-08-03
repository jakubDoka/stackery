use mini_alloc::IdentStr;

use crate::{
    gen_storage_group, loader::ModuleShadowStore, parser::expr::IdentAst, Diagnostics, IntType,
    LitKindAst, ModuleRef, Modules, OpCode, Ref, Signature, Span, Type, VecStore,
};

mod instr_emmiter;

type InstrRepr = u16;
type InstrRef = Ref<Instr, InstrRepr>;
type Const = Ref<LitKindAst, InstrRepr>;
type Sym = Ref<Decl, InstrRepr>;
type FuncRef = Ref<Func, InstrRepr>;
type Scope = VecStore<Decl, InstrRepr>;
type InternedIdent = Ref<IdentStr, InstrRepr>;

pub type SourceOffset = i16;

#[derive(Copy, Clone)]
enum InstrKind {
    Uninit,
    Const(Const),
    Sym(Sym),
    Module(ModuleRef),
    Op(OpCode),
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
    data: DeclData,
}

#[derive(Clone)]
enum DeclData {
    Func(FuncRef),
    Type(Type),
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
                data: DeclData::Type(Type::BuiltIn(crate::BuiltInType::Int(ty))),
            });
        }
    }
}

pub struct TyResolverCtx<'ctx> {
    pub module: ModuleRef,
    pub instrs: &'ctx Instrs,
    pub scope: &'ctx mut Scope,
    pub diags: &'ctx mut Diagnostics,
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

pub trait TyResolver {
    fn resolve(&mut self, ctx: TyResolverCtx, body: InstrBody) -> Type;
}

pub struct InstrBuilder<'ctx> {
    instrs: &'ctx mut Instrs,
    modules: &'ctx Modules,
    diags: &'ctx mut Diagnostics,
    resolver: &'ctx mut dyn TyResolver,
}

impl<'ctx> InstrBuilder<'ctx> {
    pub fn new(
        instrs: &'ctx mut Instrs,
        modules: &'ctx Modules,
        diags: &'ctx mut Diagnostics,
        resolver: &'ctx mut dyn TyResolver,
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
        instrs::{DeclData, InstrKind},
        lookup_sym, print_cases, Instr, InstrBody, Instrs, LitKindAst, ModuleDecl, ModuleRef,
        Modules, Parser, TyResolver, Type,
    };

    #[derive(Default)]
    struct TyResolverImpl {}

    impl TyResolver for TyResolverImpl {
        fn resolve(&mut self, ctx: crate::TyResolverCtx, body: crate::InstrBody) -> crate::Type {
            #[derive(Debug)]
            enum StackValue {
                Const(LitKindAst),
                Type(Type),
                Mod(ModuleRef),
            }

            let mut stack = Vec::<StackValue>::new();

            for &instr in body.instrs {
                let value = match instr.kind {
                    InstrKind::Uninit => todo!(),
                    InstrKind::Const(c) => StackValue::Const(body.consts[c].clone()),
                    InstrKind::Sym(sym) => match ctx.scope[sym].data.clone() {
                        DeclData::Func(_) => todo!(),
                        DeclData::Type(ty) => StackValue::Type(ty),
                    },
                    InstrKind::Module(module) => StackValue::Mod(module),
                    InstrKind::Op(_) => todo!(),
                    InstrKind::Field(i) => match stack.pop().unwrap() {
                        StackValue::Const(_) => todo!(),
                        StackValue::Type(_) => todo!(),
                        StackValue::Mod(m) => {
                            let view = ctx.instrs.decls.view(ctx.instrs.modules[m]);
                            let delc = view
                                .decls
                                .iter()
                                .find(|d| d.name.ident == body.idents[i])
                                .unwrap()
                                .data
                                .clone();
                            match delc {
                                DeclData::Func(_) => todo!(),
                                DeclData::Type(ty) => StackValue::Type(ty),
                            }
                        }
                    },
                };

                stack.push(value);
            }

            match stack.pop() {
                Some(StackValue::Type(ty)) => ty,
                other => panic!("Expected type, got {:?}", other),
            }
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
            InstrKind::Op(op) => ctx.push_str(op.name()),
            InstrKind::Field(f) => {
                ctx.push_str(". ");
                ctx.push_str(body.idents[f].as_str());
            }
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
                DeclData::Func(func) => func,
                DeclData::Type(_) => {
                    todo!()
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

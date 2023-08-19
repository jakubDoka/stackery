use core::fmt;

use mini_alloc::IdentStr;

use crate::{
    gen_storage_group, loader::ModuleShadowStore, Diagnostics, ExprAst, LitKindAst, ModuleRef,
    Modules, OpCode, Pos, RecordKind, Ref, Slice,
};

mod instr_emmiter;

type InstrRepr = u16;
type DefaultRef<T> = Ref<T, InstrRepr>;

pub type ArgCount = InstrRepr;
pub type Returns = bool;
pub type Scoped = bool;
pub type HasValue = bool;
pub type FrameSize = InstrRepr;
pub type Sym = InstrRepr;
pub type FieldCount = InstrRepr;
pub type HasType = bool;

pub type InstrRef = DefaultRef<Instr>;
pub type Const = LitKindAst;
pub type ConstRef = DefaultRef<Const>;
pub type DefRef = DefaultRef<Def>;
pub type CtxSym = DefaultRef<IdentStr>;
pub type SourceOffset = Pos;
pub type Mutable = bool;
pub type Fields = Slice<IdentStr, InstrRepr>;

#[derive(Clone, Debug)]
pub enum InstrKind {
    Module(ModuleRef),
    Const(ConstRef),
    Def(DefRef),

    Sym(Sym),
    Decl {
        mutable: bool,
        inited: bool,
        typed: bool,
    },
    BinOp(OpCode),
    Call(ArgCount),
    Field(CtxSym),

    If(InstrRef),
    Else(InstrRef),
    EndIf,
    Return(Returns),

    Rt,

    Drop,
    DropScope(FrameSize, Returns),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CtorMode {
    Filled,
    Complete,
    Defaulted,
}

impl From<&Option<Option<ExprAst<'_>>>> for CtorMode {
    fn from(expr: &Option<Option<ExprAst<'_>>>) -> Self {
        match expr {
            Some(Some(_)) => Self::Filled,
            Some(None) => Self::Defaulted,
            None => Self::Complete,
        }
    }
}

impl fmt::Display for CtorMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Filled => write!(f, "filled"),
            Self::Complete => write!(f, "complete"),
            Self::Defaulted => write!(f, "defaulted"),
        }
    }
}

#[derive(Clone)]
pub struct Instr {
    pub kind: InstrKind,
    pub span: SourceOffset,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub enum DefKind {
    Record {
        kind: RecordKind,
        fields: Fields,
    },
    Func {
        arg_count: ArgCount,
        signature_len: ArgCount,
    },
    #[default]
    Const,
}
impl DefKind {
    fn initial_ip(self) -> InstrRepr {
        match self {
            Self::Record { .. } => 0,
            Self::Func { signature_len, .. } => signature_len,
            Self::Const => 0,
        }
    }
}

#[derive(Clone, Default)]
pub struct Def {
    pub kind: DefKind,
    pub body: InstrBodyRef,
}

gen_storage_group! {
    InstrBodyes InstrBodyRef InstrBody InstrBodyBuilder 'a {
        instrs: Instr,
        consts: LitKindAst,
    }
}

gen_storage_group! {
    ModuleDecls ModuleDeclRef ModuleDecl ModuleDeclBuilder 'a {
        defs: Def,
        idents: IdentStr,
        decls: IdentStr,
    }
}

impl<'a> ModuleDecl<'a> {
    pub fn lookup_def(&self, name: &str) -> Option<DefRef> {
        self.decls
            .iter()
            .position(|i| i.as_str() == name)
            .map(|i| DefRef::from_repr(i as _))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefId {
    pub ns: ModuleRef,
    pub index: DefRef,
}

#[derive(Default)]
pub struct Instrs {
    bodies: InstrBodyes,
    decls: ModuleDecls,
    modules: ModuleShadowStore<ModuleDeclRef>,
}

impl Instrs {
    const ENTRY_NAME: &'static str = "main";

    pub fn new<'a>(builtin_names: impl IntoIterator<Item = &'a str>) -> Self {
        let mut s = Self::default();
        let mut mb = ModuleDeclBuilder::default();
        mb.decls
            .extend(builtin_names.into_iter().map(IdentStr::from));
        s.modules[Modules::BUILTIN] = s.decls.finish(&mut mb);
        s
    }

    pub fn body_of(&self, id: DefId) -> InstrBody {
        let module_view = self.decls.view(self.modules[id.ns]);
        self.bodies.view(module_view.defs[id.index].body)
    }

    pub fn decls_of(&self, module: ModuleRef) -> ModuleDecl {
        self.decls.view(self.modules[module])
    }

    pub fn entry_of(&self, root: ModuleRef) -> Option<DefRef> {
        self.decls_of(root)
            .decls
            .iter()
            .position(|d| d.as_str() == Self::ENTRY_NAME)
            .map(|i| DefRef::from_repr(i as _))
    }

    pub fn get_def(&self, f: DefId) -> Def {
        let module_view = self.decls_of(f.ns);
        module_view.defs[f.index].clone()
    }

    pub fn initial_ip_for(&self, id: DefId) -> InstrRepr {
        self.get_def(id).kind.initial_ip()
    }

    pub fn get_fields(&self, inside: ModuleRef, fields: Fields) -> &[IdentStr] {
        &self.decls_of(inside).idents[fields]
    }
}

#[must_use]
pub struct ScopeFrame(usize);

impl ScopeFrame {
    pub fn new<T>(s: &Vec<T>) -> Self {
        Self(s.len())
    }

    pub fn size<T>(&self, s: &Vec<T>) -> FrameSize {
        (s.len() - self.0) as FrameSize
    }

    pub fn end<T>(self, s: &mut Vec<T>) {
        s.truncate(self.0);
    }
}

pub struct InstrBuilder<'ctx> {
    instrs: &'ctx mut Instrs,
    modules: &'ctx Modules,
    diags: &'ctx mut Diagnostics,
}

impl<'ctx> InstrBuilder<'ctx> {
    pub fn new(
        instrs: &'ctx mut Instrs,
        modules: &'ctx Modules,
        diags: &'ctx mut Diagnostics,
    ) -> Self {
        Self {
            instrs,
            modules,
            diags,
        }
    }
}

#[cfg(test)]
pub mod instr_test_util {
    use mini_alloc::{ArenaBase, DiverBase};

    use crate::{
        bail, instrs::InstrKind, Def, InstrBody, Instrs, ModuleDecl, ModuleMeta, ModuleRef,
        Modules, Parser,
    };

    pub fn display_id(prefix: &str, id: usize, ctx: &mut String) {
        ctx.push_str(prefix);
        ctx.push_str(id.to_string().as_str());
    }

    pub fn display_instr(
        instr: &InstrKind,
        instrs: &Instrs,
        body: InstrBody,
        module: ModuleDecl,
        indent: usize,
        ctx: &mut String,
    ) {
        match instr.clone() {
            InstrKind::Sym(s) => display_id("sym", s as usize, ctx),
            InstrKind::BinOp(op) => ctx.push_str(op.name()),
            InstrKind::Field(f) => {
                ctx.push_str(". ");
                ctx.push_str(&module.idents[f].as_str());
            }
            InstrKind::Module(m) => display_id("mod", m.index(), ctx),
            InstrKind::Const(c) => ctx.push_str(&body.consts[c].to_string()),
            InstrKind::Decl {
                mutable,
                inited,
                typed,
            } => {
                if mutable {
                    ctx.push_str("mut ");
                }
                if inited {
                    ctx.push_str("inited ");
                }
                if typed {
                    ctx.push_str("typed ");
                }
                ctx.push_str("decl");
            }
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
            InstrKind::If(s) => {
                ctx.push_str("if ");
                ctx.push_str(s.index().to_string().as_str());
            }
            InstrKind::Else(label) => {
                ctx.push_str("else ");
                ctx.push_str(label.index().to_string().as_str());
            }
            InstrKind::EndIf => ctx.push_str("endif"),
            InstrKind::Rt => ctx.push_str("rt"),
            InstrKind::Return(true) => ctx.push_str("return value"),
            InstrKind::Return(false) => ctx.push_str("return"),
            InstrKind::Def(def) => display_def(&module.defs[def], instrs, module, indent + 1, ctx),
        }
    }

    pub fn display_module(module: ModuleRef, modules: &Modules, instrs: &Instrs, ctx: &mut String) {
        let name = modules.name_of(module);
        let view = instrs.decls.view(instrs.modules[module]);

        ctx.push_str(name.as_str());
        ctx.push_str(":\n");

        for (decl, def) in view.decls.iter().zip(view.defs.iter()) {
            ctx.push_str("  ");
            ctx.push_str(decl.as_str());
            ctx.push_str(" = ");

            display_def(def, instrs, view, 2, ctx);
        }
    }

    pub fn display_def(
        def: &Def,
        instrs: &Instrs,
        view: ModuleDecl,
        indent: usize,
        ctx: &mut String,
    ) {
        let prefix = match def.kind {
            crate::DefKind::Record { kind, .. } => kind.name(),
            crate::DefKind::Func { .. } => "func",
            crate::DefKind::Const => "const",
        };

        ctx.push_str(prefix);
        ctx.push_str(":\n");

        let body = instrs.bodies.view(def.body);

        for instr in body.instrs.iter() {
            for _ in 0..indent {
                ctx.push_str("  ");
            }
            display_instr(&instr.kind, instrs, body, view, indent, ctx);
            ctx.push('\n');
        }
    }

    pub fn parse_modules(mods: &Modules, meta: &ModuleMeta, instrs: &mut Instrs, ctx: &mut String) {
        let mut diags = crate::Diagnostics::default();

        let mut arena = ArenaBase::new(1000);
        let scope = arena.scope();
        let mut diver = DiverBase::new(1000);

        for &module in meta.order() {
            let parser = Parser::new(mods.files(), module, &mut diags, &scope);
            let Some(items) = parser.parse(diver.dive()) else {
                bail!("failed to parse module" diags ctx continue);
            };

            let builder = crate::InstrBuilder::new(instrs, mods, &mut diags);

            if builder.build(module, items).is_none() || !diags.view().is_empty() {
                bail!("failed to build module" diags ctx continue);
            }

            display_module(module, &mods, &instrs, ctx);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{load_modules, parse_modules, print_cases};

    fn prefrom_test(_: &str, source: &str, ctx: &mut String) {
        let mut mods = crate::Modules::new();
        let loader = crate::LoaderMock::new(source);
        let Some(meta) = load_modules(&mut mods, loader, ctx) else {
            return;
        };

        let mut instrs = crate::Instrs::new([]);
        parse_modules(&mods, &meta, &mut instrs, ctx);
    }

    print_cases! { prefrom_test:
        function "let main = fn(): :{bi}.i32 0";
        arguments "let main = fn(a: :{bi}.i32): :{bi}.i32 a * 2";
        import "let bi = :{bi} let drop = fn(): bi.i32 42";
        if_stmt "let foo = fn(): :{bi}.i32 if true 1 else 2";
        structs "
            let foo = struct {
                a: :{bi}.i32
                b: :{bi}.i32
                c: union {
                    c: :{bi}.i32
                    d: :{bi}.i32
                }
                d: enum {
                    b: :{bi}.i32
                }
            }
        ";
        symbol_numbering "
            let foo = {
                let goo = {
                    let loo = 3
                    loo
                    2
                }
                let moo = 3
                moo
                foo
                0
            }
            let soo = 1
        ";
    }
}

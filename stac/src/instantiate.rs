mod interp_impl;

use crate::{
    BuiltInType, Def, DefId, DefRef, Diagnostics, FrameSize, Instrs, Layout, LitKindAst, ModuleRef,
    ModuleShadowStore, Modules, OpCode, Ref, Returns, ShadowStore, Span, Sym, Type, Types,
    VecStore,
};

use std::fmt;

type SubsRepr = u16;
pub type TypeRef = Ref<Type, SubsRepr>;
pub type FuncInst = u32;
pub type FieldIndex = u16;
pub type InstrIindex = u16;
pub type IrTypes = VecStore<Type, SubsRepr>;

pub enum EvalResult {
    Rt(Ir),
    Const(Resolved),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Resolved {
    Const(LitKindAst),
    Type(Type),
    Module(ModuleRef),
    Def(DefId),
}

impl Resolved {
    pub fn ty(&self) -> BuiltInType {
        match self {
            Resolved::Type(_) => BuiltInType::Type,
            Resolved::Module(_) => BuiltInType::Module,
            Resolved::Const(c) => BuiltInType::from_const(c),
            Resolved::Def(_) => BuiltInType::Def,
        }
    }
}

impl fmt::Display for Resolved {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Resolved::Const(c) => c.fmt(f),
            Resolved::Type(t) => t.fmt(f),
            Resolved::Module(m) => write!(f, "module{}", m.index()),
            Resolved::Def(d) => write!(f, "def{}#{}", d.ns.index(), d.index.index()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Local {
    Ct(Resolved),
    Rt(Type),
}

#[derive(Clone)]
pub enum FinstKind {
    Uninit,
    Const(LitKindAst),
    Sym(Sym),
    Call(FuncInst),
    BinOp(OpCode),
    Decl(bool),
    Field(FieldIndex),

    If,
    Else,
    EndIf,
    Return(Returns),

    Drop,
    DropScope(FrameSize, Returns),
}

#[derive(Clone)]
pub struct Finst {
    pub kind: FinstKind,
    pub ty: TypeRef,
    pub span: Span,
}

#[derive(Default)]
pub struct Ir {
    pub instrs: Vec<Finst>,
    pub types: IrTypes,
}

impl Ir {
    pub fn get_ty(&self, ty: TypeRef) -> Type {
        match BuiltInType::from_index(ty.index()) {
            Ok(b) => Type::BuiltIn(b),
            Err(r) => self.types[TypeRef::from_repr(r as _)].clone(),
        }
    }

    fn intern_ty(&mut self, t: &Type) -> TypeRef {
        match t {
            &Type::BuiltIn(b) => TypeRef::from_repr(b as _),
            other => {
                let r = self.types.find_or_push(other).index();
                TypeRef::from_repr((r + BuiltInType::COUNT) as _)
            }
        }
    }
}

#[derive(Default)]
pub struct DeclCache {
    pub computed: ShadowStore<Def, SubsRepr, Option<Resolved>>,
}

#[derive(Default)]
pub struct Instances {
    lookup: Vec<Entry>,
    progress: usize,
    recent: usize,
    decls: ModuleShadowStore<DeclCache>,
}

impl Instances {
    pub fn new() -> Self {
        let mut s = Self::default();
        for (i, ty) in BuiltInType::ALL.into_iter().enumerate() {
            s.decls[Modules::BUILTIN].computed[DefRef::from_repr(i as _)] =
                Some(Resolved::Type(Type::BuiltIn(ty)));
        }
        s
    }

    pub fn next_entry(&mut self) -> Option<(FuncInst, Entry)> {
        self.lookup.get(self.progress).map(|e| {
            self.progress += 1;
            ((self.progress - 1) as _, e.clone())
        })
    }

    pub fn project_entry(&mut self, value: Entry) -> FuncInst {
        if let Some(pos) = self.lookup.iter().position(|e| *e == value) {
            return pos as _;
        }
        let id = self.lookup.len() as _;
        self.lookup.push(value);
        id
    }

    pub fn recent_entries(&mut self) -> impl IntoIterator<Item = (FuncInst, &Entry)> {
        let prev = self.recent;
        self.recent = self.lookup.len();
        (prev as FuncInst..).zip(&self.lookup[prev..])
    }
}

pub struct Interpreter<'ctx> {
    arch: Layout,
    instances: &'ctx mut Instances,
    diags: &'ctx mut Diagnostics,
    modules: &'ctx Modules,
    instrs: &'ctx Instrs,
    types: &'ctx mut Types,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Entry {
    pub id: DefId,
    pub ip: u16,
    pub inputs: Vec<Local>,
    pub ret: Type,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(
        arch: Layout,
        instances: &'ctx mut Instances,
        diags: &'ctx mut Diagnostics,
        modules: &'ctx Modules,
        instrs: &'ctx Instrs,
        types: &'ctx mut Types,
    ) -> Self {
        Self {
            arch,
            instances,
            diags,
            modules,
            instrs,
            types,
        }
    }
}

#[cfg(test)]
mod test {

    use crate::{
        print_cases, BuiltInType, DefId, Diagnostics, EvalResult, Finst, Instrs, Interpreter,
        Layout, LoaderMock, Modules, Resolved, Type, Types,
    };

    fn perform_test(_: &str, source: &str, ctx: &mut String) -> Option<()> {
        let mut mods = Modules::new();
        let loader = LoaderMock::new(source);
        let meta = crate::load_modules(&mut mods, loader, ctx)?;

        let mut instrs = Instrs::new(BuiltInType::ALL.map(|t| t.name()));
        crate::parse_modules(&mods, &meta, &mut instrs, ctx);

        let Some(entry) = instrs.entry_of(meta.root()) else {
            ctx.push_str("no entry point\n");
            return None;
        };

        let mut diags = Diagnostics::default();
        let mut instances = crate::Instances::new();
        let mut types = Types::default();
        let mut interp = Interpreter::new(
            Layout::ARCH_64,
            &mut instances,
            &mut diags,
            &mods,
            &instrs,
            &mut types,
        );
        let id = DefId {
            ns: meta.root(),
            index: entry,
        };

        let EvalResult::Const(Resolved::Def(id)) = interp.eval(&crate::Entry {
            id,
            ip: instrs.initial_ip_for(id),
            inputs: vec![],
            ret: Type::BuiltIn(BuiltInType::I32),
        }) else {
            ctx.push_str("entry point is not a function\n");
            return None;
        };

        let ir = match interp.eval(&crate::Entry {
            id,
            ip: instrs.initial_ip_for(id),
            inputs: vec![],
            ret: Type::BuiltIn(BuiltInType::I32),
        }) {
            EvalResult::Rt(ir) => ir,
            EvalResult::Const(res) => {
                ctx.push_str("function fully folded\n");
                ctx.push_str(format!("result: {res}\n").as_str());
                return Some(());
            }
        };

        if !diags.view().is_empty() {
            ctx.push_str("diagnostics:\n");
            ctx.push_str(diags.view());
        }

        for Finst { kind, ty, .. } in ir.instrs.iter() {
            use crate::FinstKind::*;
            match kind {
                Const(lit) => {
                    ctx.push_str("const ");
                    ctx.push_str(lit.to_string().as_str());
                }
                Sym(sym) => {
                    ctx.push_str("sym");
                    ctx.push_str(sym.to_string().as_str());
                }
                Call(ac) => {
                    ctx.push_str("call ");
                    ctx.push_str(ac.to_string().as_str());
                }
                BinOp(op) => {
                    ctx.push_str("binop ");
                    ctx.push_str(op.name());
                }
                Decl(false) => ctx.push_str("decl"),
                Decl(true) => ctx.push_str("decl mut"),
                Field(f) => {
                    ctx.push_str(". ");
                    ctx.push_str(f.to_string().as_str());
                }
                Drop => ctx.push_str("drop"),
                DropScope(fs, r) => {
                    ctx.push_str("drop scope ");
                    ctx.push_str(fs.to_string().as_str());
                    ctx.push_str(" ");
                    ctx.push_str(r.to_string().as_str());
                }
                If => ctx.push_str("if"),
                Else => ctx.push_str("else"),
                EndIf => ctx.push_str("endif"),
                Return(false) => ctx.push_str("return"),
                Return(true) => ctx.push_str("return value"),
                Uninit => ctx.push_str("uninit"),
            }
            ctx.push_str(" : ");
            ctx.push_str(ir.get_ty(*ty).to_string().as_str());
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
                let mut x = 1
                x += 1
                x
            }
        ";
        multiple_locals "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                let mut x = 1
                let mut y = 2
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
        is_statement "
            let bi = :{bi}
            let main = fn(): bi.i32
                if true if false 0 else 1 else 2
        ";
        runtime_if "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                if rt true if rt true 0 else 1 else 2
            }
        ";
        early_return "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                if true return 0
                1
            }
        ";
        dead_branch "
            let bi = :{bi}
            let main = fn(): bi.i32 {
                if true {}
                1
            }
        ";
        records "
            let bi = :{bi}
            let point = struct {
                x: bi.i32
                y: bi.i32
            }
            let optuion = enum {
                none: bi.unit
                some: bi.i32
            }
            let field_alias = union {
                x: bi.i32
                y: bi.i32
            }
            let nested = struct {
                y: enum {
                    field: union {
                        x: bi.i32
                        y: bi.i32
                    }
                }
            }
        ";
        struct_ctor "
            let i32 = :{bi}.i32
            let main = fn(): i32 {
                let Point = struct {
                    x: i32
                    y: i32
                }

                let x = 1
                let p = Point.{ x, y: 2 }
                let p = Point.{ x, ..p }
                let p = Point.{..}
                p.x
            }
        ";
    }
}

mod interp_impl;

use crate::{
    BuiltInType, Diagnostics, FrameSize, FuncId, Instrs, IrTypes, Layout, LitKindAst, Modules,
    OpCode, Ref, Resolved, Resolver, Returns, Span, Sym, Type, TypeRef, TypeRefIndex,
};

type SubsRepr = u16;
pub type SubsRef = Ref<Type, SubsRepr>;
pub type FuncInst = u32;
pub type FieldIndex = u16;
pub type InstrIindex = u16;
pub type LabelRefCount = u16;

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Decl(bool),
    Field(FieldIndex),

    If,
    Else,
    EndIf,

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
        self.types.get_ty(ty)
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
pub struct FuncInstances {
    lookup: Vec<Entry>,
    progress: usize,
    recent: usize,
}

impl FuncInstances {
    pub fn next(&mut self) -> Option<(FuncInst, Entry)> {
        self.lookup.get(self.progress).map(|e| {
            self.progress += 1;
            ((self.progress - 1) as _, e.clone())
        })
    }

    pub fn project(&mut self, value: Entry) -> FuncInst {
        if let Some(pos) = self.lookup.iter().position(|e| *e == value) {
            return pos as _;
        }
        let id = self.lookup.len() as _;
        self.lookup.push(value);
        id
    }

    pub fn recent(&mut self) -> impl IntoIterator<Item = (FuncInst, &Entry)> {
        let prev = self.recent;
        self.recent = self.lookup.len();
        (prev as FuncInst..).zip(&self.lookup[prev..])
    }
}

pub struct Interpreter<'ctx> {
    arch: Layout,
    instances: &'ctx mut FuncInstances,
    diags: &'ctx mut Diagnostics,
    modules: &'ctx Modules,
    instrs: &'ctx Instrs,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Entry {
    pub id: FuncId,
    pub ip: u16,
    pub inputs: Vec<Local>,
    pub ret: Type,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(
        arch: Layout,
        instances: &'ctx mut FuncInstances,
        diags: &'ctx mut Diagnostics,
        modules: &'ctx Modules,
        instrs: &'ctx Instrs,
    ) -> Self {
        Self {
            arch,
            instances,
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
            let mut instances = crate::FuncInstances::default();
            let mut interp =
                Interpreter::new(ctx.arch, &mut instances, ctx.diags, ctx.modules, ctx.instrs);
            interp.ct_eval(span, ctx.module, body, decls)
        }
    }

    ResolverImpl
}

#[cfg(test)]
mod test {
    use crate::{
        print_cases, BuiltInType, Diagnostics, Finst, FuncId, Instrs, Interpreter, Layout,
        LoaderMock, Modules, Type,
    };

    fn perform_test(_: &str, source: &str, ctx: &mut String) -> Option<()> {
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
        let mut instances = crate::FuncInstances::default();
        let mut interp =
            Interpreter::new(Layout::ARCH_64, &mut instances, &mut diags, &mods, &instrs);
        let id = FuncId {
            module: meta.root(),
            func: entry,
        };
        let ir = interp.eval(&crate::Entry {
            id,
            ip: instrs.func(id).signature_len,
            inputs: vec![],
            ret: Type::BuiltIn(BuiltInType::I32),
        });

        if !diags.view().is_empty() {
            ctx.push_str("diagnostics:\n");
            ctx.push_str(diags.view());
        }

        for Finst { kind, ty, .. } in ir.instrs.iter() {
            match kind {
                crate::FinstKind::Const(lit) => {
                    ctx.push_str("const ");
                    ctx.push_str(lit.to_string().as_str());
                }
                crate::FinstKind::Sym(sym) => {
                    ctx.push_str("sym");
                    ctx.push_str(sym.to_string().as_str());
                }
                crate::FinstKind::Call(ac) => {
                    ctx.push_str("call ");
                    ctx.push_str(ac.to_string().as_str());
                }
                crate::FinstKind::BinOp(op) => {
                    ctx.push_str("binop ");
                    ctx.push_str(op.name());
                }
                crate::FinstKind::Decl(false) => ctx.push_str("decl"),
                crate::FinstKind::Decl(true) => ctx.push_str("decl mut"),
                crate::FinstKind::Field(f) => {
                    ctx.push_str(". ");
                    ctx.push_str(f.to_string().as_str());
                }
                crate::FinstKind::Drop => ctx.push_str("drop"),
                crate::FinstKind::DropScope(fs, r) => {
                    ctx.push_str("drop scope ");
                    ctx.push_str(fs.to_string().as_str());
                    ctx.push_str(" ");
                    ctx.push_str(r.to_string().as_str());
                }
                crate::FinstKind::If => ctx.push_str("if"),
                crate::FinstKind::Else => ctx.push_str("else"),
                crate::FinstKind::EndIf => ctx.push_str("endif"),
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
    }
}

use mini_alloc::InternedStr;

use crate::{
    gen_storage_group, Diagnostics, ExprAst, LitKindAst, Module, ModuleRef, ModuleRefRepr, Modules,
    OpCode, Ref, ShadowStore, Slice, Span,
};

mod emiter_impl;
pub mod fmt;

pub type InstrIndex = u16;
pub type InstrSourceOffset = i16;
pub type FuncIndex = u32;
pub type Const = Ref<LitKindAst, InstrIndex>;
pub type Sym = Ref<SymData, InstrIndex>;
pub type Loop = Ref<LoopData, InstrIndex>;
pub type Import = ModuleRef;
pub type Ident = Ref<InternedStr, InstrIndex>;
pub type FuncRef = Ref<Func, InstrIndex>;
pub type InstrItems = Slice<InstrItem, FuncIndex>;
pub type InstrRef = Ref<Instr, InstrIndex>;
pub type InstrTypeRef = Ref<InstrType, InstrIndex>;

pub struct ConstFoldCtx<'a> {
    pub diags: &'a mut Diagnostics,
}

impl<'a> ConstFoldCtx<'a> {
    pub fn stack_borrow(&mut self) -> ConstFoldCtx {
        ConstFoldCtx { diags: self.diags }
    }
}

pub trait ArrayLenFolder {
    fn fold(&mut self, expr: &ExprAst, ctx: ConstFoldCtx) -> usize;
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncId {
    pub module: ModuleRef,
    pub func: FuncRef,
}

pub struct InstrEmiter<'ctx> {
    instrs: &'ctx mut Instrs,
    modules: &'ctx Modules,
    diags: &'ctx mut Diagnostics,
    array_len_folder: &'ctx mut (dyn ArrayLenFolder + 'ctx),
}

impl<'ctx> InstrEmiter<'ctx> {
    pub fn new(
        instrs: &'ctx mut Instrs,
        modules: &'ctx Modules,
        diags: &'ctx mut Diagnostics,
        array_const_folder: &'ctx mut (dyn ArrayLenFolder + 'ctx),
    ) -> Self {
        Self {
            instrs,
            modules,
            diags,
            array_len_folder: array_const_folder,
        }
    }
}

gen_storage_group! {
    FuncMeta FuncMetaSlice FuncMetaView FuncMetaBuilder
    'a {
        instrs: Instr,
        consts: LitKindAst,
        idents: InternedStr,
        called_funcs: FuncRef,
    }
}

gen_storage_group! {
    InstrModuleMeta InstrModuleMetaSlice InstrModuleMetaView InstrModuleMetaBuilder
    'a {
        items: InstrItem,
        funcs: Func,
    }
}

#[derive(Default)]
pub struct Instrs {
    modules: ShadowStore<Module, ModuleRefRepr, InstrModule>,
    module_meta: InstrModuleMeta,
    func_meta: FuncMeta,
}

#[allow(dead_code)]
impl Instrs {
    pub(crate) fn view_of(&self, func: FuncId) -> FuncMetaView {
        let module = &self.modules[func.module];
        let func = &self.module_meta.funcs[module.meta.funcs][func.func];
        self.func_meta.view(func.meta)
    }

    pub(crate) fn items_of(&self, module: ModuleRef) -> &[InstrItem] {
        &self.module_meta.items[self.modules[module].meta.items]
    }
}

#[derive(Copy, Clone, Default)]
pub struct InstrModule {
    meta: InstrModuleMetaSlice,
}

#[derive(Clone)]
pub struct InstrItem {
    pub name: InternedStr,
    pub kind: InstrItemKind,
    pub span: Span,
}

#[derive(Copy, Clone)]
pub enum InstrItemKind {
    Func(FuncRef),
    Import(ModuleRef),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FuncName {
    pub module: ModuleRef,
    pub name: InternedStr,
}

#[derive(Clone, PartialEq, Eq)]
pub struct SymData {
    name: InternedStr,
}

impl SymData {
    fn new(name: InternedStr) -> Self {
        Self { name }
    }
}

pub struct LoopData {
    label: InternedStr,
    offset: InstrIndex,
    break_frame: usize,
    dest: InstrRef,
    scope: emiter_impl::ScopeFrame,
}

#[derive(Copy, Clone, Default)]
pub struct Func {
    pub arg_count: InstrIndex,
    pub meta: FuncMetaSlice,
}

#[derive(Copy, Clone)]
pub struct Instr {
    pub kind: InstrKind,
    pub offset: InstrSourceOffset,
}

impl Instr {
    pub fn new(kind: InstrKind, offset: InstrSourceOffset) -> Self {
        Self { kind, offset }
    }
}

#[derive(Copy, Clone)]
pub enum InstrKind {
    Const(Const),
    Sym(Sym),
    Mod(Import),
    Array { item_count: InstrIndex },
    FilledArray { len_const: Const },
    Tuple { item_count: InstrIndex },
    Struct { field_count: InstrIndex },
    StructField { name: Ident, has_value: bool },
    Embed,
    Enum { name: Ident, has_value: bool },
    Call { arg_count: InstrIndex },
    Func(FuncRef),
    Unary(OpCode),
    Binary(OpCode),
    Index,
    Decl,
    Drop,
    DropDecl { decl_count: InstrIndex },
    BackJumpDest { used: bool },
    Jump { to: InstrIndex, conditional: bool },
    Field { name: Ident, is_meta: bool },
    Unkown,
    Self_,
    Error,
}

pub struct InstrType;

use mini_alloc::{InternedStr, StrInterner};

use crate::{
    gen_storage_group, Diagnostics, Enum, LiteralKindAst, Module, ModuleRef, ModuleRefRepr,
    Modules, OpCode, Ref, ShadowStore, Slice, Span, Struct,
};

mod emiter_impl;
pub mod fmt;

pub type InstrIndex = u16;
pub type FuncIndex = u32;
pub type Const = Ref<LiteralKindAst, InstrIndex>;
pub type Sym = Ref<SymData, InstrIndex>;
pub type Loop = Ref<LoopData, InstrIndex>;
pub type Import = ModuleRef;
pub type Ident = Ref<InternedStr, InstrIndex>;
pub type FuncRef = Ref<Func, InstrIndex>;
pub type InstrItems = Slice<InstrItem, FuncIndex>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncId {
    pub module: ModuleRef,
    pub func: FuncRef,
}

pub struct InstrEmiter<'a> {
    instrs: &'a mut Instrs,
    modules: &'a Modules,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
}

impl<'a> InstrEmiter<'a> {
    pub fn new(
        instrs: &'a mut Instrs,
        modules: &'a Modules,
        interner: &'a StrInterner,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            instrs,
            modules,
            interner,
            diags: diagnostics,
        }
    }
}

gen_storage_group! {
    FuncMeta FuncMetaSlice FuncMetaView FuncMetaBuilder
    'a {
        instrs: Instr,
        consts: LiteralKindAst,
        idents: InternedStr,
        labels: LoopData,
        syms: SymData,
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

#[derive(Copy, Clone)]
pub struct InstrItem {
    pub name: InternedStr,
    pub kind: InstrItemKind,
    pub span: Span,
}

#[derive(Copy, Clone)]
pub enum InstrItemKind {
    Func(FuncRef),
    Import(ModuleRef),
    Struct(Struct),
    Enum(Enum),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncName {
    pub module: ModuleRef,
    pub name: InternedStr,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SymData(InternedStr);

pub struct LoopData {
    label: InternedStr,
    offset: InstrIndex,
    break_frame: usize,
    dest: Ref<Instr, InstrIndex>,
    scope: emiter_impl::ScopeFrame,
}

#[derive(Copy, Clone, Default)]
pub struct Func {
    pub arg_count: InstrIndex,
    pub meta: FuncMetaSlice,
}

#[derive(Copy, Clone)]
pub enum Instr {
    Const(Const),
    Sym(Sym),
    Mod(Import),
    Array { item_count: InstrIndex },
    FilledArray,
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
    Error,
}

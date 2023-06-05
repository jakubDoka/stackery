use mini_alloc::*;

use crate::*;

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

macro_rules! gen_func_meta {
    (
        $name:ident $slice_name:ident $view_name:ident $builder_name:ident
        $lt:lifetime {$(
            $field:ident: $field_ty:ty,
        )*}
    ) => {
        #[derive(Default)]
        pub struct $name {$(
            $field: VecStore<$field_ty, FuncIndex>,
        )*}

        impl $name {
            pub fn new() -> Self {
                Self::default()
            }

            pub fn builder(&mut self) -> $builder_name {
                $builder_name {$(
                    $field: self.$field.builder(),
                )*}
            }

            pub fn view(&self, slice: $slice_name) -> $view_name {
                $view_name {$(
                    $field: &self.$field[slice.$field],
                )*}
            }

            pub fn clear(&mut self) {
                $(
                    self.$field.clear();
                )*
            }

            pub fn preserve_ranges(&mut self, ranges: &mut [&mut $slice_name]) {
                $(
                    ranges.sort_unstable_by_key(|range| range.$field.start());
                    self.$field.preserve_chunks_by_slice(ranges.iter_mut().map(|range| &mut range.$field));
                )*
            }
        }

        pub struct $builder_name<$lt> {$(
            pub $field: VecStoreBuilder<$lt, $field_ty, InstrIndex, FuncIndex>,
        )*}

        impl<$lt> $builder_name<$lt> {
            pub fn finish(self) -> $slice_name {
                $slice_name {$(
                    $field: self.$field.finish(),
                )*}
            }

            pub fn view(&self, slice: $slice_name) -> $view_name {
                $view_name {$(
                    $field: &self.$field[slice.$field],
                )*}
            }
        }

        #[derive(Clone, Copy, Default)]
        pub struct $slice_name {$(
            $field: VecSlice<$field_ty, InstrIndex, FuncIndex>,
        )*}

        pub struct $view_name<'a> {$(
            pub $field: &'a VecSliceView<$field_ty, InstrIndex>,
        )*}
    };
}

gen_func_meta! {
    FuncMeta FuncMetaSlice FuncMetaView FuncMetaBuilder
    'a {
        instrs: Instr,
        consts: LiteralKindAst,
        idents: InternedStr,
        labels: LoopData,
        syms: SymData,
    }
}

gen_func_meta! {
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

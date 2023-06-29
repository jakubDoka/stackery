#![allow(incomplete_features)]
#![feature(
    async_fn_in_trait,
    let_chains,
    allocator_api,
    type_alias_impl_trait,
    return_position_impl_trait_in_trait,
    never_type,
    int_roundings,
    if_let_guard,
    inline_const,
    iter_intersperse,
    iter_collect_into,
    trusted_len,
    slice_group_by,
    array_windows,
    core_intrinsics,
    slice_partition_dedup,
    decl_macro,
    slice_from_ptr_range,
    impl_trait_in_assoc_type,
    iter_next_chunk,
    iter_advance_by
)]

mod algorithms;
mod diagnostics;
mod graphs;
mod instrs;
mod lexer;
mod loader;
mod parser;
mod source;
mod storage;
mod temp_mem;
mod types;

#[cfg(test)]
pub use loader::test_util::{DelayedLoaderMock, LoaderMock};

pub use {
    diagnostics::{Diagnostic, DiagnosticConfig, Diagnostics, Severty},
    graphs::{CycleDetector, CycleFinder, Graph},
    instrs::{
        fmt::format_instrs, ArrayLenFolder, Const, ConstFoldCtx, Func, FuncId, FuncIndex, FuncMeta,
        FuncMetaBuilder, FuncMetaSlice, FuncMetaView, FuncRef, Ident, Import, Instr, InstrEmiter,
        InstrIndex, InstrItem, InstrItemKind, InstrItems, InstrKind, InstrModule, InstrModuleMeta,
        InstrModuleMetaBuilder, InstrModuleMetaSlice, InstrModuleMetaView, InstrRef, InstrType,
        InstrTypeRef, Instrs, Loop, LoopData, Sym, SymData,
    },
    lexer::{ImportLexer, Lexer, OpCode, Token, TokenKind},
    loader::{
        is_visible, Loader, LoaderCtx, Module, ModuleDeps, ModuleLoader, ModuleMeta, ModuleRef,
        ModuleRefRepr, Modules, Vis, VisError,
    },
    parser::{
        expr::{
            BinaryAst, BlockAst, BreakAst, CallAst, ContinueAst, EnumAst, ExprAst, FieldAst,
            FieldIdentAst, FilledArrayAst, ForLoopAst, FuncArgAst, FuncAst, IdentAst, IfAst,
            IndexAst, IntLit, LitAst, LitKindAst, LoopAst, NamedExprAst, OpAst, StructFieldAst,
            UnaryAst, UnitAst,
        },
        fmt::format_ast,
        Parser, StringParseError, StringParser, TransposeOpt,
    },
    source::{File, FileRef, Files, Span},
    storage::{
        refs::{Ref, RefRepr, Slice},
        BitSet, PoolStore, ShadowStore, SubsTable, VecSlice, VecSliceView, VecStore,
        VecStoreBuilder,
    },
    temp_mem::{TempMem, TempMemBase},
    types::{
        AsocType, AsocTypeDef, BuiltInType, DefinedType, InstanceType, IntType, InternedType,
        ParamType, PointerType, Signature, Spec, SpecBase, SpecBaseId, SpecBaseRef, SpecInstance,
        Struct, StructId, StructRef, TyRef, TySlice, Type, TypeRefRepr,
    },
};

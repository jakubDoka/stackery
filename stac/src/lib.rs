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
    drain_filter,
    decl_macro,
    slice_from_ptr_range
)]

mod algorithms;
mod diagnostics;
mod instrs;
mod lexer;
mod loader;
mod parser;
mod source;
mod storage;
mod temp_mem;
mod types;

#[cfg(test)]
pub use loader::test_util::LoaderMock;

pub use {
    algorithms::detect_cycles::{CycleDetector, Graph},
    diagnostics::{Diagnostic, DiagnosticConfig, Diagnostics, Severty},
    instrs::{
        fmt::format_instrs, Const, Func, FuncId, FuncIndex, FuncMeta, FuncMetaBuilder,
        FuncMetaSlice, FuncMetaView, FuncRef, Ident, Import, Instr, InstrEmiter, InstrIndex,
        InstrItem, InstrItemKind, InstrItems, InstrModule, InstrModuleMeta, InstrModuleMetaBuilder,
        InstrModuleMetaSlice, InstrModuleMetaView, Instrs, Loop, LoopData, Sym, SymData,
    },
    lexer::{ImportLexer, Lexer, OpCode, Token, TokenKind},
    loader::{
        Loader, LoaderCtx, Module, ModuleDeps, ModuleLoader, ModuleMeta, ModuleRef, ModuleRefRepr,
        Modules,
    },
    parser::{
        expr::{
            BinaryAst, BlockAst, BreakAst, CallAst, ContinueAst, EnumAst, ExprAst, FieldAst,
            FieldIdentAst, FilledArrayAst, ForLoopAst, FuncArgAst, FuncAst, IdentAst, IfAst,
            IndexAst, LiteralAst, LiteralKindAst, LoopAst, NamedExprAst, OpAst, StructFieldAst,
            UnaryAst, UnitAst,
        },
        fmt::format_ast,
        Parser, StringParseError, StringParser, TransposeOpt,
    },
    source::{File, FileRef, Files, Span},
    storage::{
        refs::{Ref, RefRepr, Slice},
        BitSet, PoolStore, ShadowStore, VecSlice, VecSliceView, VecStore, VecStoreBuilder,
    },
    temp_mem::{TempMem, TempMemBase},
    types::{
        ArrayType, BuiltinType, EnumType, FuncInstId, FuncType, Generator, GeneratorArgs,
        GeneratorCtx, IntType, IntWidth, StructType, Type, Types,
    },
};

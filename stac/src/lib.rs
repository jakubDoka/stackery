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
    drain_filter
)]

mod algorithms;
mod cache;
mod diagnostics;
mod lexer;
mod parser;
mod source;
mod temp_mem;
mod types;

pub use {
    algorithms::detect_cycles::{CycleDetector, Graph},
    cache::{
        ast,
        scope::{Scope, ScopeCache, ScopeFrame, ScopeRange, ScopeView, Sym},
        storage::{
            BitSet, CachePool, CacheRef, CacheSlice, CacheVec, CacheVecPush, CacheVecRange,
            CacheVecView,
        },
        Cache, CacheLoader, CacheParser, CachePush, CacheRange, CacheView, Loader, LoaderCtx,
        Module, ModuleMeta, ModuleRef, Modules, ParsedModule,
    },
    diagnostics::{Diagnostic, DiagnosticConfig, Diagnostics, Severty},
    lexer::{ImportLexer, Lexer, OpCode, Token, TokenKind},
    parser::{
        expr::{
            BinaryAst, BlockAst, BreakAst, CallAst, ContinueAst, EnumAst, ExprAst, FieldAst,
            FilledArrayAst, ForLoopAst, FuncArgAst, FuncAst, IdentAst, IfAst, IndexAst, LiteralAst,
            LiteralKindAst, LoopAst, OpAst, StructFieldAst, UnaryAst, UnitAst,
        },
        fmt::format_ast,
        Parser, StringParseError, StringParser, TransposeOpt,
    },
    source::{File, FileRef, Files, Span},
    temp_mem::{TempMem, TempMemBase},
    types::{
        ArrayType, BuiltinType, EnumType, FuncType, StructType, Type, Types, UnknownType, Unknowns,
    },
};

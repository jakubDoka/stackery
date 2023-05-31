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
    slice_partition_dedup
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
        scope::{Scope, ScopeCache, ScopeFrame, ScopeRange, ScopeView, Sym},
        storage::{
            BitSet, CachePool, CacheRef, CacheSlice, CacheVec, CacheVecPush, CacheVecRange,
            CacheVecView,
        },
        Cache, CacheLoader, CachePush, CacheRange, CacheView, Loader, LoaderCtx, Module, ModuleRef,
        Modules,
    },
    diagnostics::{Diagnostic, DiagnosticConfig, Diagnostics, Severty},
    lexer::{ImportLexer, Lexer, OpCode, Token, TokenKind},
    parser::{
        expr::{
            BinaryAst, BlockAst, BreakAst, CallAst, ContinueAst, EnumAst, ExprAst, FieldAccessAst,
            FilledArrayAst, ForLoopAst, FuncArgAst, FuncAst, IdentAst, IfAst, LiteralAst,
            LiteralKindAst, LoopAst, OpAst, StructField, UnaryAst, UnitAst,
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

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
    slice_group_by
)]

mod diagnostics;
mod lexer;
mod parser;
mod source;

pub use {
    diagnostics::{Diagnostic, DiagnosticConfig, Diagnostics, Severty},
    lexer::{Lexer, Token, TokenKind},
    parser::{
        expr::{
            BinaryAst, BlockAst, BreakAst, CallAst, ContinueAst, EnumAst, ExprAst, FieldAccessAst,
            FilledArrayAst, ForLoopAst, FuncArgAst, FuncAst, IdentAst, IfAst, LiteralAst,
            LiteralKindAst, LoopAst, MethodCallAst, OpAst, StructField, UnaryAst, UnitAst,
        },
        Parser, TransposeOpt,
    },
    source::{File, FileRef, Files, Span},
};

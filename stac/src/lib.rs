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
    inline_const
)]

mod codegen;
mod errors;
mod lexer;
mod loader;
mod parser;

pub use {
    errors::{Error, ErrorBuilder, Errors},
    lexer::Token,
    loader::{LoaderSource, RootSource, Source, SourceId},
    scoped_arena,
};

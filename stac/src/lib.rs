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
mod instantiate;
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
    diagnostics::*, graphs::*, instrs::*, lexer::*, loader::*, parser::expr::*, parser::fmt::*,
    parser::*, source::*, storage::refs::*, storage::*, temp_mem::*, types::*,
};

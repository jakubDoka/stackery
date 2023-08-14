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
    iter_advance_by,
    trait_alias
)]

mod algorithms;
mod diagnostics;
mod graphs;
mod instantiate;
mod instrs;
mod layout;
mod lexer;
mod loader;
mod parser;
mod source;
mod storage;
mod temp_mem;
mod types;

#[cfg(test)]
pub use {instrs::instr_test_util::*, loader::load_test_util::*};

pub use {
    diagnostics::*, graphs::*, instantiate::*, instrs::*, layout::*, lexer::*, loader::*,
    mini_alloc, parser::expr::*, parser::fmt::*, parser::*, print_test, source::*,
    storage::refs::*, storage::*, temp_mem::*, types::*,
};

#![cfg_attr(not(test), no_std)]
#![feature(
    allocator_api,
    slice_ptr_get,
    core_intrinsics,
    unchecked_math,
    pointer_is_aligned,
    trusted_len,
    ptr_metadata,
    inline_const,
    auto_traits,
    negative_impls,
    slice_ptr_len,
    impl_trait_in_assoc_type
)]

extern crate alloc;

mod arena;
mod bump;
mod diver;
mod interner;
// mod parallel;

pub use {
    arena::{ArenaBase, ArenaProxi, ArenaScope},
    diver::{Diver, DiverBase},
    hashbrown,
    interner::{
        FnvBuildHasher, FnvHashMap, FnvHashSet, FnvHasher, Internable, Interned, InternedSlice,
        InternedStr, Interner, InternerObj, InternerSlice, StrInterner,
    },
};

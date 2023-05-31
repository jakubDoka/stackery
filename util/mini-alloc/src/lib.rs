#![no_std]
#![feature(
    allocator_api,
    slice_ptr_get,
    core_intrinsics,
    unchecked_math,
    pointer_is_aligned,
    trusted_len,
    ptr_metadata,
    inline_const
)]

extern crate alloc;

mod arena;
mod bump;
mod diver;
mod interner;

pub use {
    arena::{ArenaBase, ArenaProxi, ArenaScope},
    diver::{Diver, DiverBase},
    interner::{
        FnvBuildHasher, FnvHashMap, FnvHasher, Internable, Interned, InternedSlice, InternedStr,
        Interner, InternerObj, InternerSlice, StrInterner,
    },
};

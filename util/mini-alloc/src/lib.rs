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
    impl_trait_in_assoc_type,
    const_slice_ptr_len
)]

extern crate alloc;

mod arena;
mod bump;
mod diver;
mod ident;
//mod interner;
mod seg_vec;

pub use {
    arena::{ArenaBase, ArenaProxi, ArenaScope},
    diver::{Diver, DiverBase},
    hashbrown,
    ident::IdentStr,
    seg_vec::{SegIndexRepr, SegSlice, SegSliceMut, SegVec},
    //interner::{Internable, Interned, InternedSlice, Interner, InternerObj, InternerSlice},
};

pub type FnvBuildHasher = core::hash::BuildHasherDefault<FnvHasher>;
pub type FnvHashMap<K, V> = hashbrown::HashMap<K, V, FnvBuildHasher>;
pub type FnvHashSet<K> = hashbrown::HashSet<K, FnvBuildHasher>;

pub struct FnvHasher {
    hash: u64,
}

impl core::hash::Hasher for FnvHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.hash
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.hash = self.hash.wrapping_mul(0x100000001b3);
            self.hash ^= byte as u64;
        }
    }
}

impl Default for FnvHasher {
    #[inline]
    fn default() -> Self {
        Self {
            hash: 0xcbf29ce484222325,
        }
    }
}

#![feature(
    allocator_api,
    slice_ptr_get,
    core_intrinsics,
    unchecked_math,
    pointer_is_aligned
)]

mod arena;
mod bump;
mod diver;

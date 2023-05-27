use core::{
    alloc::Allocator,
    cell::UnsafeCell,
    iter::TrustedLen,
    mem::{self, MaybeUninit},
    ptr,
};

use alloc::{alloc::Global, slice};

use crate::bump::{AllocList, Bump};

pub struct ArenaBase<A: Allocator = Global> {
    alloc: AllocList<A>,
}

impl ArenaBase {
    pub fn new(base_size: usize) -> Self {
        Self::new_in(base_size, Global)
    }
}

impl<A: Allocator> ArenaBase<A> {
    pub fn new_in(base_size: usize, alloc: A) -> Self {
        Self {
            alloc: AllocList::new_in(
                (base_size + AllocList::MIN_BASE_SIZE).next_power_of_two(),
                alloc,
            ),
        }
    }

    pub fn scope(&mut self) -> ArenaScope<A> {
        ArenaScope::new(Default::default(), self)
    }
}

unsafe fn alloc<'a, T, A>(arena: &impl Arena<A>, value: T) -> &'a mut T
where
    A: Allocator,
{
    const { assert!(!mem::needs_drop::<T>()) }
    let ptr = {
        let (bump, base) = arena.state();
        base.alloc.alloc_to_bump::<T>(bump)
    };

    ptr.as_ptr().write(value);
    &mut *ptr.as_ptr()
}

unsafe fn alloc_slice<'a, T, A>(ctx: &impl Arena<A>, slice: &[T]) -> &'a mut [T]
where
    T: Clone,
    A: Allocator,
{
    alloc_iter(ctx, slice.iter().cloned())
}

unsafe fn alloc_iter<'a, T, I, A>(arena: &impl Arena<A>, iterator: I) -> &'a mut [T]
where
    I: IntoIterator<Item = T>,
    I::IntoIter: TrustedLen + ExactSizeIterator,
    A: Allocator,
{
    const { assert!(!mem::needs_drop::<T>()) }
    let iter = iterator.into_iter();
    let len = iter.len();

    let ptr = {
        let (bump, base) = arena.state();
        base.alloc.alloc_slice_to_bump::<T>(bump, len)
    };

    {
        let mut ptr = ptr.as_ptr();
        for item in iter {
            ptr.write(item);
            ptr = ptr.add(1);
        }
    }
    slice::from_raw_parts_mut(ptr.as_ptr(), len)
}

unsafe fn alloc_rev_iter<'a, T, I, A>(arena: &impl Arena<A>, iterator: I) -> &'a mut [T]
where
    I: IntoIterator<Item = T>,
    I::IntoIter: TrustedLen + ExactSizeIterator,
    A: Allocator,
{
    const { assert!(!mem::needs_drop::<T>()) }
    let iter = iterator.into_iter();
    let len = iter.len();

    let ptr = {
        let (bump, base) = arena.state();
        base.alloc.alloc_slice_to_bump::<T>(bump, len)
    };

    {
        let mut ptr = ptr.as_ptr().add(len);
        for item in iter {
            ptr = ptr.sub(1);
            ptr.write(item);
        }
    }

    slice::from_raw_parts_mut(ptr.as_ptr(), len)
}

pub struct ArenaScope<'a, A: Allocator = Global> {
    bump: UnsafeCell<Bump>,
    base: UnsafeCell<&'a mut ArenaBase<A>>,
}

impl<'a, A: Allocator> ArenaScope<'a, A> {
    fn new(bump: Bump, base: &'a mut ArenaBase<A>) -> Self {
        Self {
            bump: bump.into(),
            base: base.into(),
        }
    }

    pub fn alloc<T>(&self, value: T) -> &mut T {
        unsafe { alloc(self, value) }
    }

    pub fn alloc_slice<T: Clone>(&self, slice: &[T]) -> &mut [T] {
        unsafe { alloc_slice(self, slice) }
    }

    pub fn alloc_iter<T, I>(&self, iterator: I) -> &mut [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
    {
        unsafe { alloc_iter(self, iterator) }
    }

    pub fn alloc_rev_iter<T, I>(&self, iterator: I) -> &mut [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
    {
        unsafe { alloc_rev_iter(self, iterator) }
    }

    pub fn proxi(&self) -> ArenaProxi<A> {
        let (bump, base) = unsafe { self.state() };
        ArenaProxi {
            bump: bump.into(),
            base: base.into(),
        }
    }

    pub fn object<'b, T: ArenaObject<'b>>(
        &'b mut self,
        factory: impl FnOnce(&'b Self) -> T,
    ) -> ScopedArenaObject<'b, T> {
        let old_bump = unsafe { *self.state().0 };

        let object = factory(self);

        let (bump, _) = unsafe { self.state() };
        ScopedArenaObject {
            object,
            old_bump,
            bump,
        }
    }
}

pub unsafe trait ArenaObject<'a> {}

#[macro_export]
macro_rules! impl_arena_object {
    ($(($lt:lifetime $(, $first_generic:ident $($other:tt)*)?) => $ty:ty;)*) => {
        $(
            unsafe impl<$lt $(, $first_generic $($other)*)?> $crate::arena::ArenaObject<$lt> for $ty {}
        )*
    };

    ($($ty:ty),* $(,)?) => {
        $(
            unsafe impl<'a> $crate::arena::ArenaObject<'a> for $ty {}
        )*
    };
}

impl_arena_object! {
    ('a, T: ArenaObject<'a>) => &'a [T];
    ('a, T: ArenaObject<'a>) => &'a T;
}

impl_arena_object! {
    u8, u16, u32, u64, u128, usize,
    i8, i16, i32, i64, i128, isize,
    f32, f64,
    (),
}

pub struct ScopedArenaObject<'a, T> {
    object: T,
    old_bump: Bump,
    bump: &'a mut Bump,
}

impl<'a, T> ScopedArenaObject<'a, T> {
    pub fn get<'b>(&'b self) -> &'b T
    where
        T: ArenaObject<'b>,
    {
        &self.object
    }

    pub fn into_inner(self) -> T {
        let consumed = MaybeUninit::new(self);
        unsafe { ptr::read(&consumed.assume_init_ref().object) }
    }
}

impl<'a, T> Drop for ScopedArenaObject<'a, T> {
    fn drop(&mut self) {
        *self.bump = self.old_bump;
    }
}

pub struct ArenaProxi<'a, A: Allocator = Global> {
    bump: UnsafeCell<&'a mut Bump>,
    base: UnsafeCell<&'a mut ArenaBase<A>>,
}

impl<'a, A: Allocator> ArenaProxi<'a, A> {
    unsafe fn state(&self) -> (&'a mut Bump, &'a mut ArenaBase<A>) {
        (&mut *self.bump.get(), &mut *self.base.get())
    }

    pub fn alloc<T>(&self, value: T) -> &'a mut T {
        unsafe { alloc(self, value) }
    }

    pub fn alloc_slice<T: Clone>(&self, slice: &[T]) -> &'a mut [T] {
        unsafe { alloc_slice(self, slice) }
    }

    pub fn alloc_iter<T, I>(&self, iterator: I) -> &'a mut [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
    {
        unsafe { alloc_iter(self, iterator) }
    }

    pub fn alloc_rev_iter<T, I>(&self, iterator: I) -> &'a mut [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
    {
        unsafe { alloc_rev_iter(self, iterator) }
    }

    pub fn scope(&self) -> ArenaScope<A> {
        let (&mut bump, base) = unsafe { self.state() };
        ArenaScope {
            bump: bump.into(),
            base: base.into(),
        }
    }
}

impl<A: Allocator> Arena<A> for ArenaProxi<'_, A> {
    unsafe fn state(&self) -> (&mut Bump, &mut ArenaBase<A>) {
        (&mut *self.bump.get(), *self.base.get())
    }
}

impl<A: Allocator> Arena<A> for ArenaScope<'_, A> {
    unsafe fn state(&self) -> (&mut Bump, &mut ArenaBase<A>) {
        (&mut *self.bump.get(), &mut *self.base.get())
    }
}

trait Arena<A: Allocator> {
    unsafe fn state(&self) -> (&mut Bump, &mut ArenaBase<A>);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scoped_object() {
        let mut base = ArenaBase::new(0);
        let mut scope = base.scope();

        let object = scope.object(|scope| {
            &*scope.alloc_slice(&[&*scope.alloc(0u32), &*scope.alloc(1), &*scope.alloc(2)])
        });

        let o = *object.get();
        assert_eq!(o, &[&0, &1, &2]);

        drop(object);
    }

    #[test]
    fn alloc_within_iter() {
        let mut base = ArenaBase::new(0);
        let scope = base.scope();

        let array = scope.alloc_iter((0..10).map(|i| scope.alloc(i)));

        assert_eq!(array, &[&0, &1, &2, &3, &4, &5, &6, &7, &8, &9]);
    }
}

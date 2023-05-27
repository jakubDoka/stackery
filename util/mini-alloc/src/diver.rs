use core::{alloc::Allocator, iter::TrustedLen, marker::PhantomData, mem, ptr::NonNull};

use alloc::alloc::Global;

use crate::bump::{AllocList, Bump};

pub struct DiverBase<A: Allocator = Global> {
    list: AllocList<A>,
}

impl DiverBase {
    pub fn new(base_size: usize) -> Self {
        Self::new_in(base_size, Global)
    }
}

impl<A: Allocator> DiverBase<A> {
    pub fn new_in(base_size: usize, alloc: A) -> Self {
        Self {
            list: AllocList::new_in(
                (base_size + AllocList::MIN_BASE_SIZE).next_power_of_two(),
                alloc,
            ),
        }
    }

    pub fn dive<T>(&mut self) -> Diver<T, A> {
        Diver::new(Default::default(), self)
    }

    pub fn untyped_dive(&mut self) -> Diver<(), A> {
        self.dive()
    }
}

pub struct Diver<'a, T = (), A: Allocator = Global> {
    bump: Bump,
    alloc: &'a mut DiverBase<A>,
    len: usize,
    _marker: PhantomData<T>,
}

impl<'a, T, A: Allocator> Diver<'a, T, A> {
    fn new(bump: Bump, alloc: &'a mut DiverBase<A>) -> Self {
        

        Diver {
            bump,
            alloc,
            len: 0,
            _marker: PhantomData,
        }
    }

    pub fn push(&mut self, value: T) -> &mut T {
        let ptr = self.alloc();

        unsafe {
            ptr.as_ptr().write(value);
        }

        self.len += 1;

        unsafe { &mut *ptr.as_ptr() }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.len = self.len.checked_sub(1)?;

        unsafe {
            let ptr = self.dealloc();
            Some(ptr.as_ptr().read())
        }
    }

    fn alloc(&mut self) -> NonNull<T> {
        self.alloc.list.alloc_to_bump(&mut self.bump)
    }

    fn dealloc(&mut self) -> NonNull<T> {
        self.alloc
            .list
            .dealloc_from_bump(&mut self.bump)
            .expect("calls to this should be protected by len counter")
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn dive<D>(&mut self) -> Diver<D, A> {
        Diver::new(self.bump, self.alloc)
    }

    pub fn untyped_dive(&mut self) -> Diver<(), A> {
        self.dive()
    }
}

impl<'a, T, A: Allocator> Drop for Diver<'a, T, A> {
    fn drop(&mut self) {
        if mem::needs_drop::<T>() {
            while self.pop().is_some() {}
        }
    }
}

pub struct DiverIntoIter<'a, T, A: Allocator = Global> {
    diver: Diver<'a, T, A>,
}

impl<'a, T, A: Allocator> Iterator for DiverIntoIter<'a, T, A> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.diver.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.diver.len(), Some(self.diver.len()))
    }
}

impl<'a, T, A: Allocator> IntoIterator for Diver<'a, T, A> {
    type Item = T;
    type IntoIter = DiverIntoIter<'a, T, A>;

    fn into_iter(self) -> Self::IntoIter {
        DiverIntoIter { diver: self }
    }
}

impl<'a, T, A: Allocator> ExactSizeIterator for DiverIntoIter<'a, T, A> {
    fn len(&self) -> usize {
        self.diver.len()
    }
}

unsafe impl<'a, T, A: Allocator> TrustedLen for DiverIntoIter<'a, T, A> {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn push_some() {
        let mut diver = DiverBase::new(0);

        let mut diver_i = diver.dive::<u32>();

        for i in 0..10 {
            diver_i.push(i);
        }

        for (i, j) in diver_i.into_iter().zip((0..10).rev()) {
            assert_eq!(i, j);
        }
    }

    #[test]
    fn stacked_pushes() {
        let mut diver = DiverBase::new(0);

        let mut diver_i = diver.dive::<u32>();

        for i in 0..10 {
            diver_i.push(i);
            let mut diver_j = diver_i.dive::<u32>();

            for j in 0..10 {
                diver_j.push(j * 1000);
            }
        }

        for (i, j) in diver_i.into_iter().zip((0..10).rev()) {
            assert_eq!(i, j);
        }
    }
}

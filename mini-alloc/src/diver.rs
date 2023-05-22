use std::{
    alloc::{Allocator, Global},
    marker::PhantomData,
    ptr::NonNull,
};

use crate::bump::{AllocList, Bump};

pub struct DiverAlloc<A: Allocator = Global> {
    list: AllocList<A>,
}

impl DiverAlloc {
    pub fn new(base_size: usize) -> Self {
        Self {
            list: AllocList::new(base_size, Global),
        }
    }
}

impl<A: Allocator> DiverAlloc<A> {
    pub fn new_in(base_size: usize, alloc: A) -> Self {
        Self {
            list: AllocList::new(base_size, alloc),
        }
    }

    pub fn dive<T>(&mut self) -> Diver<T, A> {
        Diver::new(Default::default(), self)
    }
}

pub struct Diver<'a, T, A: Allocator = Global> {
    bump: Bump,
    alloc: &'a mut DiverAlloc<A>,
    len: usize,
    _marker: PhantomData<T>,
}

impl<'a, T, A: Allocator> Diver<'a, T, A> {
    fn new(bump: Bump, alloc: &'a mut DiverAlloc<A>) -> Self {
        let s = Diver {
            bump,
            alloc,
            len: 0,
            _marker: PhantomData,
        };

        s
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

    unsafe fn dealloc(&mut self) -> NonNull<T> {
        self.alloc.list.dealloc_from_bump(&mut self.bump)
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl<'a, T, A: Allocator> Drop for Diver<'a, T, A> {
    fn drop(&mut self) {
        while self.pop().is_some() {}
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
}

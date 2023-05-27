use core::{
    alloc::{Allocator, Layout},
    intrinsics::unlikely,
    mem,
    ops::Range,
    ptr::{self, NonNull},
};

use alloc::alloc::Global;

const ALLOC_HEADER_SIZE: usize = mem::size_of::<AllocHeader>();
const ALLOC_HEADER_ALIGN: usize = mem::align_of::<AllocHeader>();

pub(crate) struct AllocList<A: Allocator = Global> {
    alloc: A,
    head: Option<Alloc>,
    base_size: usize,
}

impl AllocList {
    pub(crate) const MIN_BASE_SIZE: usize = ALLOC_HEADER_SIZE;

    #[cfg(test)]
    pub(crate) fn new(base_size: usize) -> Self {
        Self::new_in(base_size, Global)
    }
}

impl<A: Allocator> AllocList<A> {
    pub(crate) fn new_in(base_size: usize, alloc: A) -> Self {
        assert!(base_size >= AllocList::MIN_BASE_SIZE);
        Self {
            alloc,
            head: None,
            base_size,
        }
    }

    #[inline]
    pub(crate) fn alloc_to_bump<T>(&mut self, bump: &mut Bump) -> NonNull<T> {
        loop {
            match bump.alloc(Layout::new::<T>()) {
                Some(ptr) => return ptr.cast(),
                None => self.expand(bump),
            }
        }
    }

    pub fn alloc_slice_to_bump<T>(&mut self, bump: &mut Bump, len: usize) -> NonNull<T> {
        let layout = Layout::array::<T>(len).unwrap();

        loop {
            match bump.alloc(layout) {
                Some(ptr) => return ptr.cast(),
                None => self.expand(bump),
            }
        }
    }

    #[inline]
    pub(crate) fn dealloc_from_bump<T>(&mut self, bump: &mut Bump) -> Option<NonNull<T>> {
        loop {
            match bump.dealloc(mem::size_of::<T>()) {
                Some(ptr) => return Some(ptr.cast()),
                None if *bump == Default::default() => return None,
                None => self.shrink(bump),
            }
        }
    }

    #[inline(never)]
    #[cold]
    fn expand(&mut self, bump: &mut Bump) {
        let Some(head) = self.head else {
            let head = unsafe {
                Alloc::new(
                    self.base_size,
                    AllocHeader {
                        next: None,
                        prev: None,
                        meta: AllocHeaderMeta::initial(),
                    },
                    &self.alloc,
                )
            };

            self.head = Some(head);

            *bump = unsafe { head.empty_bump(self.base_size) };

            return;
        };

        if *bump == Default::default() {
            *bump = unsafe { head.empty_bump(self.base_size) };
            return;
        }

        let header = unsafe { bump.header() };
        let next = if let Some(alloc) = header.next {
            alloc
        } else {
            let next = unsafe {
                Alloc::new(
                    self.base_size,
                    AllocHeader {
                        next: None,
                        prev: Some(bump.source()),
                        meta: header.meta.next(),
                    },
                    &self.alloc,
                )
            };

            header.next = Some(next);

            next
        };

        header.meta = header.meta.with_end_offset(bump.free_space());

        *bump = unsafe { next.empty_bump(self.base_size) };
    }

    #[inline(never)]
    #[cold]
    fn shrink(&mut self, bump: &mut Bump) {
        if *bump == Default::default() {
            return;
        }

        let Some(prev) = (unsafe { bump.header().prev }) else {
            *bump = Default::default();
            return;
        };

        *bump = unsafe { prev.full_bump(self.base_size) };
        bump.cursor = unsafe { bump.cursor.sub(bump.header().meta.end_offset()) };
    }
}

impl<A: Allocator> Drop for AllocList<A> {
    fn drop(&mut self) {
        let mut current = self.head;

        while let Some(alloc) = current {
            current = unsafe { alloc.drop(self.base_size, &self.alloc).next };
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) struct Bump {
    cursor: *mut u8,
    end: *mut u8,
    start: *mut u8,
}

impl Bump {
    #[inline]
    unsafe fn new(start: *mut u8, end: *mut u8, cursor: *mut u8) -> Self {
        debug_assert!(start <= cursor);
        debug_assert!(cursor <= end);
        debug_assert!(start.is_aligned_to(mem::align_of::<NonNull<Alloc>>()));

        Self { cursor, end, start }
    }

    #[inline]
    fn alloc(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        let padding = self.cursor.align_offset(layout.align());
        let size = layout.size();

        if unlikely(self.free_space() < padding + size) {
            return None;
        }

        unsafe { Some(self.alloc_low(padding, size)) }
    }

    #[inline]
    unsafe fn alloc_low(&mut self, padding: usize, size: usize) -> NonNull<u8> {
        self.cursor = self.cursor.add(padding);
        let ptr = self.cursor;
        self.cursor = self.cursor.add(size);
        NonNull::new_unchecked(ptr)
    }

    #[inline]
    fn free_space(&self) -> usize {
        self.end as usize - self.cursor as usize
    }

    #[inline]
    fn taken_space(&self) -> usize {
        self.cursor as usize - self.start as usize
    }

    fn dealloc(&mut self, size: usize) -> Option<NonNull<u8>> {
        if unlikely(self.taken_space() < size) {
            return None;
        }

        self.cursor = unsafe { self.cursor.sub(size) };
        NonNull::new(self.cursor)
    }

    unsafe fn header<'a>(&mut self) -> &'a mut AllocHeader {
        unsafe { &mut *self.end.cast::<AllocHeader>() }
    }

    unsafe fn source(&self) -> Alloc {
        Alloc {
            end: NonNull::new(self.end.add(ALLOC_HEADER_SIZE)).unwrap(),
        }
    }
}

impl Default for Bump {
    fn default() -> Self {
        Self {
            cursor: ptr::null_mut(),
            end: ptr::null_mut(),
            start: ptr::null_mut(),
        }
    }
}

#[derive(Clone, Copy)]
struct Alloc {
    end: NonNull<u8>,
}

impl Alloc {
    unsafe fn new(base_size: usize, header: AllocHeader, alloc: impl Allocator) -> Self {
        debug_assert!(base_size >= ALLOC_HEADER_SIZE);

        let size = base_size << header.meta.pow();
        let layout = Layout::from_size_align_unchecked(size, ALLOC_HEADER_ALIGN);
        let end = unsafe { alloc.allocate(layout).unwrap().as_mut_ptr().add(size) };

        end.cast::<AllocHeader>().sub(1).write(header);

        Self {
            end: NonNull::new_unchecked(end),
        }
    }

    unsafe fn header<'a>(self) -> &'a mut AllocHeader {
        unsafe { &mut *self.end.as_ptr().cast::<AllocHeader>().sub(1) }
    }

    unsafe fn drop(self, base_size: usize, alloc: impl Allocator) -> AllocHeader {
        let header = unsafe { ptr::read(self.header()) };
        let size = base_size << header.meta.pow();
        let layout = Layout::from_size_align_unchecked(size, ALLOC_HEADER_ALIGN);
        let alloc_ptr = unsafe { NonNull::new_unchecked(self.end.as_ptr().sub(size)) };
        unsafe { alloc.deallocate(alloc_ptr, layout) }
        header
    }

    unsafe fn bump_range(self, base_size: usize) -> Range<*mut u8> {
        let size = base_size << self.header().meta.pow();
        let start = unsafe { self.end.as_ptr().sub(size) };
        let end = self.end.as_ptr().sub(ALLOC_HEADER_SIZE);

        start..end
    }

    unsafe fn full_bump(self, base_size: usize) -> Bump {
        let Range { start, end } = self.bump_range(base_size);
        unsafe { Bump::new(start, end, end) }
    }

    unsafe fn empty_bump(self, base_size: usize) -> Bump {
        let Range { start, end } = self.bump_range(base_size);
        unsafe { Bump::new(start, end, start) }
    }
}

struct AllocHeader {
    next: Option<Alloc>,
    prev: Option<Alloc>,
    meta: AllocHeaderMeta,
}

#[derive(Clone, Copy)]
struct AllocHeaderMeta(usize);

impl AllocHeaderMeta {
    const POW_WIDTH: usize = 6;
    const END_OFFSET_WIDTH: usize = mem::size_of::<usize>() * 8 - Self::POW_WIDTH;
    const END_OFFSET_MASK: usize = (1 << Self::END_OFFSET_WIDTH) - 1;

    fn new(pow: usize, end_offset: usize) -> Self {
        debug_assert!(pow < 1 << Self::POW_WIDTH);
        debug_assert!(end_offset < 1 << Self::END_OFFSET_WIDTH);
        Self((pow << Self::END_OFFSET_WIDTH) | end_offset)
    }

    fn initial() -> Self {
        Self::new(0, 0)
    }

    fn pow(self) -> usize {
        self.0 >> Self::END_OFFSET_WIDTH
    }

    fn end_offset(self) -> usize {
        self.0 & Self::END_OFFSET_MASK
    }

    fn next(self) -> AllocHeaderMeta {
        Self::new(self.pow() + 1, 0)
    }

    fn with_end_offset(self, free_space: usize) -> AllocHeaderMeta {
        Self::new(self.pow(), free_space)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn expand() {
        let mut alloc = AllocList::new(AllocList::MIN_BASE_SIZE);
        let mut bump = Bump::default();

        for _ in 0..10 {
            alloc.expand(&mut bump);
        }
    }

    #[test]
    fn allocate_deallocate() {
        let mut alloc = AllocList::new(AllocList::MIN_BASE_SIZE);
        let mut bump = Bump::default();

        let mut ptrs = (0..100)
            .map(|_| alloc.alloc_to_bump::<usize>(&mut bump))
            .collect::<Vec<_>>();

        for ptr in ptrs.drain(..).rev() {
            let poped = alloc.dealloc_from_bump::<usize>(&mut bump);
            assert_eq!(poped, Some(ptr));
        }

        assert_eq!(alloc.dealloc_from_bump::<usize>(&mut bump), None);

        assert_eq!(bump.cursor, bump.start);
    }
}

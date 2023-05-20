use std::{
    alloc::{Allocator, Global, Layout},
    intrinsics::unlikely,
    mem,
    ptr::NonNull,
};

pub type NextAlloc = Option<Alloc>;
pub const NEXT_ALLOC_SIZE: usize = mem::size_of::<NextAlloc>();
pub const NEXT_ALLOC_ALIGN: usize = mem::align_of::<NextAlloc>();

pub struct AllocList<A: Allocator = Global> {
    alloc: A,
    head: Option<Alloc>,
    base_size: usize,
    len: usize,
}

impl<A: Allocator> AllocList<A> {
    pub fn new(base_size: usize, alloc: A) -> Self {
        assert!(base_size >= NEXT_ALLOC_SIZE);
        Self {
            alloc,
            head: None,
            base_size,
            len: 0,
        }
    }

    pub fn current_chunk_size(&self) -> usize {
        self.base_size << self.len
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline(never)]
    pub fn alloc(&mut self) -> Bump {
        let alloc = if let Some(mut current) = self.head {
            let mut countdown = self.len;

            loop {
                let Some(next) = countdown.checked_sub(1) else {
                    break current;
                };
                countdown = next;

                let next_address =
                    unsafe { current.end.as_ptr().cast::<NextAlloc>().sub(1).read() };

                if let Some(next) = next_address {
                    current = next;
                    continue;
                }

                let next = unsafe { Alloc::new(self.current_chunk_size(), &self.alloc) };

                unsafe {
                    current
                        .end
                        .as_ptr()
                        .cast::<NextAlloc>()
                        .sub(1)
                        .write(Some(next));
                    next.end.as_ptr().cast::<NextAlloc>().write(None);
                }

                break next;
            }
        } else {
            let next = unsafe { Alloc::new(self.current_chunk_size(), &self.alloc) };
            unsafe {
                next.end.as_ptr().cast::<NextAlloc>().write(None);
            }
            self.head = Some(next);
            next
        };

        self.len += 1;

        let start = unsafe { alloc.end.as_ptr().sub(self.current_chunk_size()) };
        let end = unsafe { alloc.end.as_ptr().sub(NEXT_ALLOC_SIZE) };
        unsafe { Bump::new(start, end) }
    }

    pub fn assume_cleared(&mut self) {
        self.len = 0;
    }
}

impl<A: Allocator> Drop for AllocList<A> {
    fn drop(&mut self) {
        let Some(mut current) = self.head else {
            return;
        };

        for i in 0..self.len {
            let next = unsafe { current.end.as_ptr().cast::<NextAlloc>().sub(1).read() };
            unsafe {
                self.alloc.deallocate(
                    NonNull::new_unchecked(current.end.as_ptr().sub(self.base_size << i)),
                    Layout::from_size_align_unchecked(self.current_chunk_size(), NEXT_ALLOC_ALIGN),
                );
            }
            current = next.unwrap();
        }
    }
}

pub struct Bump {
    cursor: *mut u8,
    end: *mut u8,
}

impl Bump {
    pub unsafe fn new(start: *mut u8, end: *mut u8) -> Self {
        debug_assert!(start <= end);
        debug_assert!(start.is_aligned_to(mem::align_of::<NonNull<Alloc>>()));

        Self { cursor: start, end }
    }

    pub unsafe fn alloc_unchecked(&mut self, layout: Layout) -> NonNull<u8> {
        if cfg!(debug_assertions) {
            self.alloc(layout).expect("Bump allocator out of memory")
        } else {
            self.alloc_low(self.cursor.align_offset(layout.align()), layout.size())
        }
    }

    unsafe fn alloc_low(&mut self, padding: usize, size: usize) -> NonNull<u8> {
        self.cursor = self.cursor.add(padding);
        let ptr = self.cursor;
        self.cursor = self.cursor.add(size);
        NonNull::new_unchecked(ptr)
    }

    pub fn alloc(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        let padding = self.cursor.align_offset(layout.align());
        let size = layout.size();

        if unlikely((self.end as usize - self.cursor as usize) < padding + size) {
            return None;
        }

        unsafe { Some(self.alloc_low(padding, size)) }
    }
}

#[derive(Clone, Copy)]
pub struct Alloc {
    end: NonNull<u8>,
}

impl Alloc {
    pub unsafe fn new(size: usize, alloc: impl Allocator) -> Self {
        debug_assert!(size >= NEXT_ALLOC_SIZE);
        let layout = Layout::from_size_align_unchecked(size, NEXT_ALLOC_ALIGN);
        let ptr = alloc.allocate(layout).unwrap();
        Self {
            end: NonNull::new_unchecked(ptr.as_mut_ptr().add(size)),
        }
    }
}

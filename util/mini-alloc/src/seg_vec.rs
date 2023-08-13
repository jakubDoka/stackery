//! A datastructure implemented according to `https://github.com/ziglang/zig/blob/master/lib/std/segmented_list.zig`
use std::{
    alloc::Allocator,
    intrinsics::unlikely,
    ops::{Index, IndexMut, Range},
    ptr::NonNull,
};

pub type SegIndexRepr = u32;

const SKIPPED_SEGMENTS: SegIndexRepr = 4;
const MAX_SEGMENTS: usize = std::mem::size_of::<SegIndexRepr>() * 8 - SKIPPED_SEGMENTS as usize;
const INDEX_BASE: SegIndexRepr = 1 << SKIPPED_SEGMENTS;

fn to_segment_and_index(index: SegIndexRepr) -> (u8, usize) {
    let segment = (index + INDEX_BASE).ilog2() - SKIPPED_SEGMENTS;
    let index = index + INDEX_BASE - (1 << (segment + SKIPPED_SEGMENTS));
    (segment as u8, index as usize)
}

fn segment_size(segment: u8) -> usize {
    1 << (segment + SKIPPED_SEGMENTS as u8)
}

pub struct SegVec<T, A: Allocator = std::alloc::Global> {
    len: SegIndexRepr,
    segments: [Segment<T>; MAX_SEGMENTS],
    alloc: A,
}

impl<T> SegVec<T> {
    pub fn new() -> Self {
        Self::new_in(std::alloc::Global)
    }
}

impl<T, A: Allocator> SegVec<T, A> {
    pub fn new_in(alloc: A) -> Self {
        Self {
            len: 0,
            segments: Default::default(),
            alloc,
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, value: T) -> &mut T {
        let (segment_index, index) = to_segment_and_index(self.len);
        let segment = &mut self.segments[segment_index as usize];

        if unlikely(segment.is_dangling()) {
            *segment = Segment::new(segment_index, &self.alloc);
        }

        let ptr = unsafe { segment.index(index).as_ptr() };
        unsafe { ptr.write(value) };

        self.len += 1;

        unsafe { &mut *ptr }
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
        let mut iter = iter.into_iter();
        loop {
            let (segment_index, index) = to_segment_and_index(self.len);
            let segment = &mut self.segments[segment_index as usize];

            if unlikely(segment.is_dangling()) {
                *segment = Segment::new(segment_index, &self.alloc);
            }

            let current_size = segment_size(segment_index);

            let mut ptr = unsafe { segment.index(index).as_ptr() };

            let written = iter
                .by_ref()
                .take(current_size - index)
                .map(|value| unsafe {
                    ptr.write(value);
                    ptr = ptr.add(1);
                    // we bump like this since iter can panic and we would end up dropping
                    // ininitialized slots
                    self.len += 1;
                })
                .count();

            if written < current_size - index {
                break;
            }
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        self.len -= 1;

        let (segment_index, index) = to_segment_and_index(self.len);
        let segment = &mut self.segments[segment_index as usize];

        Some(unsafe { segment.index(index).as_ptr().read() })
    }

    pub fn clear(&mut self) {
        dbg!(self.len);
        let (segment_index, index) = to_segment_and_index(self.len);

        for (i, segment) in self.segments[..segment_index as usize].iter().enumerate() {
            unsafe { segment.drop_contents(segment_size(i as u8)) }
        }

        unsafe { self.segments[segment_index as usize].drop_contents(index) }

        self.len = 0;
    }

    pub unsafe fn get_unchecked(&self, index: SegIndexRepr) -> &T {
        let (segment_index, index) = to_segment_and_index(index);
        let segment = &self.segments[segment_index as usize];

        &*segment.index(index).as_ptr()
    }

    pub unsafe fn get_unchecked_mut(&mut self, index: SegIndexRepr) -> &mut T {
        let (segment_index, index) = to_segment_and_index(index);
        let segment = &mut self.segments[segment_index as usize];

        &mut *segment.index(index).as_ptr()
    }

    pub fn slice(&self, range: Range<SegIndexRepr>) -> SegSlice<T> {
        assert!(range.end <= self.len);
        assert!(range.start <= range.end);
        SegSlice {
            start: range.start,
            end: range.end,
            list: &self.segments,
        }
    }

    pub fn slice_mut(&mut self, range: Range<SegIndexRepr>) -> SegSliceMut<T> {
        assert!(range.end <= self.len);
        assert!(range.start <= range.end);
        SegSliceMut {
            start: range.start,
            end: range.end,
            list: &mut self.segments,
        }
    }
}

impl<T, A: Allocator> Index<SegIndexRepr> for SegVec<T, A> {
    type Output = T;

    fn index(&self, index: SegIndexRepr) -> &Self::Output {
        assert!(index < self.len);
        unsafe { self.get_unchecked(index) }
    }
}

impl<T, A: Allocator> IndexMut<SegIndexRepr> for SegVec<T, A> {
    fn index_mut(&mut self, index: SegIndexRepr) -> &mut Self::Output {
        assert!(index < self.len);
        unsafe { self.get_unchecked_mut(index) }
    }
}

impl<T, A: Allocator> Drop for SegVec<T, A> {
    fn drop(&mut self) {
        self.clear();
        self.segments
            .iter_mut()
            .map(std::mem::take)
            .enumerate()
            .for_each(|(i, segment)| {
                segment.drop(i as u8, &self.alloc);
            });
    }
}

pub struct SegSlice<'a, T> {
    start: SegIndexRepr,
    end: SegIndexRepr,
    list: &'a [Segment<T>; MAX_SEGMENTS],
}

impl<'a, T> Clone for SegSlice<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T> Copy for SegSlice<'a, T> {}

impl<'a, T> SegSlice<'a, T> {
    pub fn len(&self) -> usize {
        self.end as usize - self.start as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub unsafe fn get_unchecked(&self, index: SegIndexRepr) -> &T {
        let (segment_index, index) = to_segment_and_index(index + self.start);
        let segment = &self.list[segment_index as usize];

        &*segment.index(index).as_ptr()
    }
}

impl<'a, T> Index<SegIndexRepr> for SegSlice<'a, T> {
    type Output = T;

    fn index(&self, index: SegIndexRepr) -> &Self::Output {
        assert!(index < self.len() as SegIndexRepr);
        unsafe { self.get_unchecked(index) }
    }
}

pub struct SegSliceMut<'a, T> {
    start: SegIndexRepr,
    end: SegIndexRepr,
    list: &'a mut [Segment<T>; MAX_SEGMENTS],
}

impl<'a, T> SegSliceMut<'a, T> {
    pub fn len(&self) -> usize {
        self.end as usize - self.start as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub unsafe fn get_unchecked(&self, index: SegIndexRepr) -> &T {
        let (segment_index, index) = to_segment_and_index(index + self.start);
        let segment = &self.list[segment_index as usize];

        &*segment.index(index).as_ptr()
    }

    pub unsafe fn get_unchecked_mut(&mut self, index: SegIndexRepr) -> &mut T {
        let (segment_index, index) = to_segment_and_index(index + self.start);
        let segment = &mut self.list[segment_index as usize];

        &mut *segment.index(index).as_ptr()
    }
}

impl<'a, T> Index<SegIndexRepr> for SegSliceMut<'a, T> {
    type Output = T;

    fn index(&self, index: SegIndexRepr) -> &Self::Output {
        assert!(index < self.len() as SegIndexRepr);
        unsafe { self.get_unchecked(index) }
    }
}

impl<'a, T> IndexMut<SegIndexRepr> for SegSliceMut<'a, T> {
    fn index_mut(&mut self, index: SegIndexRepr) -> &mut Self::Output {
        assert!(index < self.len() as SegIndexRepr);
        unsafe { self.get_unchecked_mut(index) }
    }
}

struct Segment<T> {
    alloc: NonNull<T>,
}

impl<T> Segment<T> {
    fn new(index: u8, alloc: &impl Allocator) -> Self {
        let size = segment_size(index);
        let layout = std::alloc::Layout::array::<T>(size).unwrap();
        let ptr = alloc.allocate(layout).unwrap();
        Self { alloc: ptr.cast() }
    }

    fn drop(self, index: u8, alloc: &impl Allocator) {
        if self.is_dangling() {
            return;
        }

        let size = segment_size(index);

        let layout = std::alloc::Layout::array::<T>(size).unwrap();
        // SAFETY: we assume all instances are constructed with `Self::new` or are default
        unsafe { alloc.deallocate(self.alloc.cast(), layout) }
    }

    unsafe fn index(&self, index: usize) -> NonNull<T> {
        NonNull::new_unchecked(self.alloc.as_ptr().add(index))
    }

    fn is_dangling(&self) -> bool {
        self.alloc == NonNull::dangling()
    }

    unsafe fn drop_contents(&self, size: usize) {
        std::ptr::slice_from_raw_parts_mut(self.alloc.as_ptr(), size).drop_in_place();
    }
}

impl<T> Default for Segment<T> {
    fn default() -> Self {
        Self {
            alloc: NonNull::dangling(),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn push_some() {
        let push_count = 1000;
        let mut list = super::SegVec::new();
        for i in 0..push_count {
            list.push(i);
        }
        for i in 0..push_count {
            assert_eq!(list[i], i);
        }
    }

    #[test]
    fn extend_some() {
        let push_count = 1000;
        let mut list = super::SegVec::new();
        list.extend(0..push_count);
        for i in 0..push_count {
            assert_eq!(list[i], i);
        }
    }

    #[test]
    fn pop_some() {
        let push_count = 1000;
        let mut list = super::SegVec::new();
        for i in 0..push_count {
            list.push(i);
        }
        for i in (0..push_count).rev() {
            assert_eq!(list.pop(), Some(i));
        }
    }

    #[test]
    fn propper_dropping() {
        use std::cell::Cell;

        struct DropCounter<'a>(&'a Cell<usize>);

        impl<'a> Drop for DropCounter<'a> {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }

        let push_count = 1000;

        let counter = Cell::new(0);
        let mut list = super::SegVec::new();
        for _ in 0..push_count {
            list.push(DropCounter(&counter));
        }
        list.pop();
        drop(list);
        assert_eq!(counter.get(), push_count);
    }
}

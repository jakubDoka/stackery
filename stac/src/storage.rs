use std::{
    intrinsics::unlikely,
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut, Index, IndexMut, Range},
    ptr, slice, cmp::Ordering, hash,
};

use crate::*;

pub mod refs;
// pub mod scope;

pub struct ShadowStore<K, R, V> {
    data: Vec<V>,
    default: V,
    _phantom: PhantomData<(K, R)>,
}

impl<K, R: RefRepr, V: Default> ShadowStore<K, R, V> {
    pub fn multiple_mut(
        &mut self,
        preserved_modules: impl IntoIterator<Item = Ref<K, R>>,
    ) -> impl Iterator<Item = &mut V> {
        let mut last_index = 0;
        preserved_modules.into_iter().map(move |module| unsafe {
            assert!(module.index() > last_index);
            last_index = module.index();
            mem::transmute(&mut self[module])
        })
    }
}

impl<K, R, V: Default> Default for ShadowStore<K, R, V> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            default: V::default(),
            _phantom: PhantomData,
        }
    }
}

impl<K, R: RefRepr, V> Index<Ref<K, R>> for ShadowStore<K, R, V> {
    type Output = V;

    fn index(&self, index: Ref<K, R>) -> &Self::Output {
        &self.data.get(index.index()).unwrap_or(&self.default)
    }
}

impl<K, R: RefRepr, V: Default> IndexMut<Ref<K, R>> for ShadowStore<K, R, V> {
    fn index_mut(&mut self, index: Ref<K, R>) -> &mut Self::Output {
        if unlikely(index.index() >= self.data.len()) {
            self.data.resize_with(index.index() + 1, V::default);
        }

        &mut self.data[index.index()]
    }
}

pub struct PoolStore<T, R> {
    data: Vec<Option<T>>,
    free: Vec<Ref<T, R>>,
}

impl<T, R> Default for PoolStore<T, R> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            free: Vec::new(),
        }
    }
}

impl<T, R: RefRepr> PoolStore<T, R> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, index: Ref<T, R>) -> Option<&T> {
        self.data.get(index.index()).and_then(Option::as_ref)
    }

    pub fn get_mut(&mut self, index: Ref<T, R>) -> Option<&mut T> {
        self.data.get_mut(index.index()).and_then(Option::as_mut)
    }

    pub fn push(&mut self, value: T) -> Ref<T, R> {
        let index = if let Some(index) = self.free.pop() {
            index
        } else {
            let index = self.data.len();
            self.data.push(None);
            Ref::new(index)
        };
        self.data[index.index()] = Some(value);
        index
    }

    pub fn remove(&mut self, index: Ref<T, R>) -> T {
        let value = self.data[index.index()]
            .take()
            .expect("cache entry already removed");
        self.free.push(index);
        value
    }

    pub fn unfiltered_values(&self) -> impl Iterator<Item = Option<&T>> {
        self.data.iter().map(Option::as_ref)
    }

    pub fn by_index(s: &Self, index: usize) -> &T {
        s.data
            .get(index)
            .and_then(Option::as_ref)
            .expect("invalid raw index")
    }

    pub fn index_to_ref(s: &Self, index: usize) -> Ref<T, R> {
        Self::by_index(s, index);
        Ref::new(index)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.unfiltered_values().flatten()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Ref<T, R>, &mut T)> {
        self.data
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| v.as_mut().map(|v| (Ref::new(i), v)))
    }

    pub fn retain(&mut self, mut pred: impl FnMut(Ref<T, R>, &mut T) -> bool) {
        for (i, v) in self.data.iter_mut().enumerate() {
            let id = Ref::new(i);
            if let Some(ref mut val) = v && !pred(id, val) {
                *v = None;
                self.free.push(id); 
            }
        }
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data.iter_mut().filter_map(Option::as_mut)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Ref<T, R>, &T)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (Ref::new(i), v)))
    }
}

impl<T, R: RefRepr> Index<Ref<T, R>> for PoolStore<T, R> {
    type Output = T;

    fn index(&self, index: Ref<T, R>) -> &Self::Output {
        self.data[index.index()]
            .as_ref()
            .expect("accessed removed cache entry")
    }
}

impl<T, R: RefRepr> IndexMut<Ref<T, R>> for PoolStore<T, R> {
    fn index_mut(&mut self, index: Ref<T, R>) -> &mut Self::Output {
        self.data[index.index()]
            .as_mut()
            .expect("accessed removed cache entry")
    }
}

pub struct VecStore<T, R> {
    data: Vec<T>,
    _marker: PhantomData<R>,
}

impl<T, R: RefRepr> VecStore<T, R> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, value: T) -> Ref<T, R> {
        let id = self.data.len();
        self.data.push(value);
        Ref::new(id)
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) -> Slice<T, R> {
        let start = self.data.len();
        self.data.extend(iter);
        let end = self.data.len();
        Slice::new(start, end)
    }

    pub fn builder<P>(&mut self) -> VecStoreBuilder<T, P, R> {
        VecStoreBuilder {
            start: self.data.len(),
            within: self,
            _marker: PhantomData,
        }
    }

    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }

    pub fn preserve_chunks_by_slice<'a, P>(
        &mut self,
        chunks: impl IntoIterator<Item = &'a mut VecSlice<T, P, R>>,
    ) where
        T: 'a,
        P: 'a,
    {
        self.preserve_chunks(chunks.into_iter().map(|c| &mut c.slice))
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Ref<T, R>, &T)> + ExactSizeIterator {
        self.keys().zip(self.values())
    }

    pub fn keys(&self) -> impl DoubleEndedIterator<Item = Ref<T, R>> + ExactSizeIterator {
        Slice::new(0, self.data.len()).keys()
    }

    pub fn values(&self) -> impl DoubleEndedIterator<Item = &T> + ExactSizeIterator {
        self.data.iter()
    }

    pub fn preserve_chunks<'a>(&mut self, chunks: impl IntoIterator<Item = &'a mut Slice<T, R>>)
    where
        T: 'a,
    {
        let len = self.data.len();
        let base = self.data.as_mut_ptr();
        // we should not expose dropped memory to T
        unsafe { self.data.set_len(0) };

        let mut writer = 0;
        let mut prev_end = 0;
        let mut iter = chunks.into_iter().peekable();

        while let Some(chunk) = iter.next() {
            let Range { start, end } = chunk.range();
            assert!(start <= end, "invalid chunk range");
            assert!(start >= prev_end, "invalid chunk order");
            prev_end = end;

            let mut copy_len = end - start;
            while let Some(next) = iter.peek_mut() && let range = next.range() && range.start == start + copy_len {
                **next = Slice::new(writer + copy_len, writer + copy_len + range.len());
                copy_len += range.len();
                iter.next();
            }

            unsafe {
                let drop_len = start - writer;
                ptr::slice_from_raw_parts_mut(base.add(writer as usize), drop_len as usize)
                    .drop_in_place();
            }

            unsafe {
                let src = base.add(start as usize);
                let dst = base.add(writer as usize);
                if src != dst {
                    ptr::copy(src, dst, copy_len);
                }
            }

            *chunk = Slice::new(writer, writer + end - start);
            writer += copy_len;
        }

        unsafe {
            let drop_len = len - writer as usize;
            ptr::slice_from_raw_parts_mut(base.add(writer as usize), drop_len as usize)
                .drop_in_place();

            self.data.set_len(writer as usize);
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn values_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> + ExactSizeIterator {
        self.data.iter_mut()
    }
}

impl<T, R: RefRepr> Index<Slice<T, R>> for VecStore<T, R> {
    type Output = [T];

    fn index(&self, index: Slice<T, R>) -> &Self::Output {
        &self.data[index.range()]
    }
}

impl<T, R: RefRepr> IndexMut<Slice<T, R>> for VecStore<T, R> {
    fn index_mut(&mut self, index: Slice<T, R>) -> &mut Self::Output {
        &mut self.data[index.range()]
    }
}

impl<T, R: RefRepr> Index<Ref<T, R>> for VecStore<T, R> {
    type Output = T;

    fn index(&self, index: Ref<T, R>) -> &Self::Output {
        &self.data[index.index()]
    }
}

impl<T, R: RefRepr> IndexMut<Ref<T, R>> for VecStore<T, R> {
    fn index_mut(&mut self, index: Ref<T, R>) -> &mut Self::Output {
        &mut self.data[index.index()]
    }
}

impl<T, R, P: RefRepr> Index<VecSlice<T, R, P>> for VecStore<T, P> {
    type Output = VecSliceView<T, R>;

    fn index(&self, index: VecSlice<T, R, P>) -> &Self::Output {
        unsafe { mem::transmute(&self.data[index.slice.range()]) }
    }
}

impl<T, R, P: RefRepr> IndexMut<VecSlice<T, R, P>> for VecStore<T, P> {
    fn index_mut(&mut self, index: VecSlice<T, R, P>) -> &mut Self::Output {
        unsafe { mem::transmute(&mut self.data[index.slice.range()]) }
    }
}

impl<T, R: RefRepr> Default for VecStore<T, R> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct VecStoreBuilder<'a, T, R, P> {
    within: &'a mut VecStore<T, P>,
    start: usize,
    _marker: PhantomData<R>,
}

impl<'a, T, R: RefRepr, P: RefRepr> VecStoreBuilder<'a, T, R, P> {
    pub fn push(&mut self, value: T) -> Ref<T, R> {
        let id = self.len();
        self.within.data.push(value);
        Ref::new(id)
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) -> Slice<T, R> {
        let start = self.len();
        self.within.data.extend(iter);
        let end = self.len();
        Slice::new(start, end)
    }

    pub fn finish(self) -> VecSlice<T, R, P> {
        let end = self.within.data.len();
        VecSlice {
            slice: Slice::new(self.start, end),
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.within.data.len() - self.start
    }
}

impl<'a, T, R: RefRepr, P> Index<Slice<T, R>> for VecStoreBuilder<'a, T, R, P> {
    type Output = [T];

    fn index(&self, index: Slice<T, R>) -> &Self::Output {
        &self.within.data[index.shifted_range(self.start)]
    }
}

impl<'a, T, R: RefRepr, P> IndexMut<Slice<T, R>> for VecStoreBuilder<'a, T, R, P> {
    fn index_mut(&mut self, index: Slice<T, R>) -> &mut Self::Output {
        &mut self.within.data[index.shifted_range(self.start)]
    }
}

impl<'a, T, R: RefRepr, P> Index<Ref<T, R>> for VecStoreBuilder<'a, T, R, P> {
    type Output = T;

    fn index(&self, index: Ref<T, R>) -> &Self::Output {
        &self.within.data[index.index() - self.start]
    }
}

impl<'a, T, R: RefRepr, P> IndexMut<Ref<T, R>> for VecStoreBuilder<'a, T, R, P> {
    fn index_mut(&mut self, index: Ref<T, R>) -> &mut Self::Output {
        &mut self.within.data[index.index() - self.start]
    }
}

impl<'a, T, R, P: RefRepr> Index<VecSlice<T, R, P>> for VecStoreBuilder<'a, T, R, P> {
    type Output = VecSliceView<T, R>;

    fn index(&self, index: VecSlice<T, R, P>) -> &Self::Output {
        unsafe { mem::transmute(&self.within.data[index.slice.shifted_range(self.start)]) }
    }
}

impl<'a, T, R, P> Deref for VecStoreBuilder<'a, T, R, P> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.within.data[self.start..]
    }
}

impl<'a, T, R, P> DerefMut for VecStoreBuilder<'a, T, R, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.within.data[self.start..]
    }
}

#[repr(transparent)]
pub struct VecSlice<T, R, P> {
    slice: Slice<T, P>,
    _marker: PhantomData<R>,
}

impl<T, R, P: Copy> Copy for VecSlice<T, R, P> {}

impl<T, R, P: Copy> Clone for VecSlice<T, R, P> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, R, P: RefRepr> Default for VecSlice<T, R, P> {
    fn default() -> Self {
        Self {
            slice: Slice::default(),
            _marker: PhantomData,
        }
    }
}

impl<T, R, P: RefRepr> PartialEq for VecSlice<T, R, P> {
    fn eq(&self, other: &Self) -> bool {
        self.slice == other.slice
    }
}

impl<T, R, P: RefRepr> Eq for VecSlice<T, R, P> {}

impl<T, R, P: RefRepr> hash::Hash for VecSlice<T, R, P> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.slice.hash(state)
    }
}

impl<T, R, P: RefRepr> PartialOrd for VecSlice<T, R, P> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.slice.partial_cmp(&other.slice)
    }
}

impl<T, R, P: RefRepr> Ord for VecSlice<T, R, P> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.slice.cmp(&other.slice)
    }
}

impl<T, R, P: RefRepr> VecSlice<T, R, P> {
    pub fn start(&self) -> usize {
        self.slice.range().start
    }
}

pub struct VecSliceView<T, R> {
    phantom: PhantomData<R>,
    view: [T],
}

impl<T, R> Deref for VecSliceView<T, R> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.view
    }
}

impl<T, R> DerefMut for VecSliceView<T, R> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.view
    }
}

impl<T, R: RefRepr> Index<Slice<T, R>> for VecSliceView<T, R> {
    type Output = [T];

    fn index(&self, index: Slice<T, R>) -> &Self::Output {
        &self.view[index.range()]
    }
}

impl<T, R: RefRepr> IndexMut<Slice<T, R>> for VecSliceView<T, R> {
    fn index_mut(&mut self, index: Slice<T, R>) -> &mut Self::Output {
        &mut self.view[index.range()]
    }
}

impl<T, R: RefRepr> Index<Ref<T, R>> for VecSliceView<T, R> {
    type Output = T;

    fn index(&self, index: Ref<T, R>) -> &Self::Output {
        &self.view[index.index()]
    }
}

impl<T, R: RefRepr> IndexMut<Ref<T, R>> for VecSliceView<T, R> {
    fn index_mut(&mut self, index: Ref<T, R>) -> &mut Self::Output {
        &mut self.view[index.index()]
    }
}

impl<'a, T, R> IntoIterator for &'a VecSliceView<T, R> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.view.iter()
    }
}

impl<'a, T, R> IntoIterator for &'a mut VecSliceView<T, R> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.view.iter_mut()
    }
}

pub struct BitSet {
    data: Vec<usize>,
    len: usize,
}

impl Default for BitSet {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            len: 0,
        }
    }
}

impl BitSet {
    const BUCKET_WIDTH: usize = mem::size_of::<usize>() * 8;
    const BUCKET_WIDTH_POW: usize = Self::BUCKET_WIDTH.trailing_zeros() as usize;
    const BUCKET_MASK: usize = Self::BUCKET_WIDTH - 1;

    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let buckets = Self::capacity_for_len(capacity);
        Self {
            data: vec![0; buckets],
            len: capacity,
        }
    }

    pub fn resize(&mut self, new_len: usize) {
        let buckets = Self::capacity_for_len(new_len);
        self.data.resize(buckets, 0);
        self.len = new_len;
    }

    /// Returns true if bit was changed.
    pub fn insert(&mut self, index: usize) -> bool {
        self.len = self.len.max(index + 1);
        let (bucket, offset) = Self::project_index(index);
        if unlikely(bucket >= self.data.len()) {
            self.data.resize(bucket + 1, 0);
        }
        let prev = self.data[bucket] & (1 << offset) == 0;
        self.data[bucket] |= 1 << offset;
        prev
    }

    /// Returns true if bit was changed.
    pub fn remove(&mut self, index: usize) -> bool {
        if unlikely(index >= self.len) {
            return false;
        }
        let (bucket, offset) = Self::project_index(index);
        let prev = self.data[bucket] & (1 << offset) != 0;
        self.data[bucket] &= !(1 << offset);
        prev
    }

    pub fn contains(&self, index: usize) -> bool {
        if unlikely(index >= self.len) {
            return false;
        }
        let (bucket, offset) = Self::project_index(index);
        self.data[bucket] & (1 << offset) != 0
    }

    fn project_index(index: usize) -> (usize, usize) {
        let bucket = index >> Self::BUCKET_WIDTH_POW;
        let offset = index & Self::BUCKET_MASK;
        (bucket, offset)
    }

    fn capacity_for_len(len: usize) -> usize {
        (len + Self::BUCKET_WIDTH - 1) >> Self::BUCKET_WIDTH_POW
    }

    pub fn clear(&mut self) {
        self.data.fill(0);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn preserve_chunks() {
        let mut vec = super::VecStore::<u32, u16>::new();
        let mut to_reserve = (0..100)
            .map(|_| vec.extend([1, 2, 3, 4]))
            .enumerate()
            .filter_map(|(i, r)| if i % 3 != 0 { Some(r) } else { None })
            .collect::<Vec<_>>();

        vec.preserve_chunks(to_reserve.iter_mut());

        assert!(to_reserve
            .array_windows()
            .all(|[a, b]| a.range().end == b.range().start));
        assert!(to_reserve.iter().all(|&r| dbg!(&vec[r]) == &[1, 2, 3, 4]));
    }
}

use std::{
    hash,
    intrinsics::unlikely,
    marker::PhantomData,
    mem,
    ops::{Index, IndexMut},
};

pub struct CachePool<T> {
    data: Vec<Option<T>>,
    free: Vec<CacheRef<T>>,
}

impl<T> Default for CachePool<T> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            free: Vec::new(),
        }
    }
}

impl<T> CachePool<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, value: T) -> CacheRef<T> {
        let index = if let Some(index) = self.free.pop() {
            index
        } else {
            let index = self.data.len();
            self.data.push(None);
            CacheRef::new(index)
        };
        self.data[index.0 as usize] = Some(value);
        index
    }

    pub fn remove(&mut self, index: CacheRef<T>) -> T {
        let value = self.data[index.0 as usize]
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

    pub fn index_to_ref(s: &Self, index: usize) -> CacheRef<T> {
        Self::by_index(s, index);
        CacheRef::new(index)
    }
}

impl<T> Index<CacheRef<T>> for CachePool<T> {
    type Output = T;

    fn index(&self, index: CacheRef<T>) -> &Self::Output {
        self.data[index.0 as usize]
            .as_ref()
            .expect("accessed removed cache entry")
    }
}

impl<T> IndexMut<CacheRef<T>> for CachePool<T> {
    fn index_mut(&mut self, index: CacheRef<T>) -> &mut Self::Output {
        self.data[index.0 as usize]
            .as_mut()
            .expect("accessed removed cache entry")
    }
}

type Repr = u16;
type BaseRepr = u32;

#[derive(Debug)]
#[repr(transparent)]
pub struct CacheRef<T>(Repr, PhantomData<T>);

impl<T> PartialEq for CacheRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for CacheRef<T> {}

impl<T> hash::Hash for CacheRef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> Clone for CacheRef<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Copy for CacheRef<T> {}

impl<T> Ord for CacheRef<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> PartialOrd for CacheRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> CacheRef<T> {
    fn new(index: usize) -> Self {
        Self(
            index.try_into().expect("exceeded cache entiry limit"),
            PhantomData,
        )
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CacheSlice<T>(Repr, Repr, PhantomData<T>);

impl<T> CacheSlice<T> {
    pub fn new(start: usize, end: usize) -> Self {
        Self(
            start.try_into().expect("exceeded cache entiry limit"),
            end.try_into().expect("exceeded cache entiry limit"),
            PhantomData,
        )
    }
}

impl<T> Default for CacheSlice<T> {
    fn default() -> Self {
        Self(0, 0, PhantomData)
    }
}

pub struct CacheVec<T> {
    data: Vec<T>,
}

impl<T> CacheVec<T> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn push(&mut self) -> CacheVecPush<T> {
        let start = self.data.len();
        CacheVecPush { data: self, start }
    }

    pub fn view(&self, range: CacheVecRange<T>) -> &CacheVecView<T> {
        let start = BaseRepr::from_ne_bytes(range.base) as usize;
        let end = start + range.len as usize;
        unsafe { mem::transmute(&self.data[start..end]) }
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) -> CacheVecRange<T>
    where
        T: Clone,
    {
        let start = self.data.len();
        self.data.extend(iter);
        let end = self.data.len();
        CacheVecRange {
            base: (start as BaseRepr).to_ne_bytes(),
            len: (end - start) as Repr,
            _marker: PhantomData,
        }
    }
}

impl<T> Default for CacheVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CacheVecPush<'a, T> {
    data: &'a mut CacheVec<T>,
    start: usize,
}

impl<'a, T> CacheVecPush<'a, T> {
    pub fn push(&mut self, value: T) -> CacheRef<T> {
        let index = self.data.data.len();
        self.data.data.push(value);
        CacheRef::new(index - self.start)
    }

    pub fn extend(&mut self, values: impl IntoIterator<Item = T>) -> CacheSlice<T> {
        let start = self.data.data.len();
        self.data.data.extend(values);
        let end = self.data.data.len();
        CacheSlice::new(start - self.start, end - self.start)
    }

    pub fn finish(self) -> CacheVecRange<T> {
        let end = self.data.data.len();
        CacheVecRange {
            base: (self.start as BaseRepr).to_ne_bytes(),
            len: (end - self.start) as Repr,
            _marker: PhantomData,
        }
    }

    pub fn as_slice(&self) -> &[T] {
        &self.data.data[self.start..]
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.data.data[self.start..]
    }
}

impl<T> Index<CacheRef<T>> for CacheVecPush<'_, T> {
    type Output = T;

    fn index(&self, index: CacheRef<T>) -> &Self::Output {
        &self.data.data[self.start + index.0 as usize]
    }
}

impl<T> IndexMut<CacheRef<T>> for CacheVecPush<'_, T> {
    fn index_mut(&mut self, index: CacheRef<T>) -> &mut Self::Output {
        &mut self.data.data[self.start + index.0 as usize]
    }
}

impl<T> Index<CacheSlice<T>> for CacheVecPush<'_, T> {
    type Output = [T];

    fn index(&self, index: CacheSlice<T>) -> &Self::Output {
        &self.data.data[self.start + index.0 as usize..self.start + index.1 as usize]
    }
}

impl<T> IndexMut<CacheSlice<T>> for CacheVecPush<'_, T> {
    fn index_mut(&mut self, index: CacheSlice<T>) -> &mut Self::Output {
        &mut self.data.data[self.start + index.0 as usize..self.start + index.1 as usize]
    }
}

pub struct CacheVecView<T> {
    data: [T],
}

impl<T> CacheVecView<T> {
    pub fn as_slice(&self) -> &[T] {
        &self.data
    }
}

impl<T> Index<CacheRef<T>> for CacheVecView<T> {
    type Output = T;

    fn index(&self, index: CacheRef<T>) -> &Self::Output {
        &self.data[index.0 as usize]
    }
}

impl<T> Index<CacheSlice<T>> for CacheVecView<T> {
    type Output = [T];

    fn index(&self, index: CacheSlice<T>) -> &Self::Output {
        &self.data[index.0 as usize..index.1 as usize]
    }
}

#[derive(Clone, Copy)]
pub struct CacheVecRange<T> {
    base: [u8; 4],
    len: Repr,
    _marker: PhantomData<T>,
}

impl<T> Default for CacheVecRange<T> {
    fn default() -> Self {
        Self {
            base: [0; 4],
            len: 0,
            _marker: PhantomData,
        }
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

    pub fn get(&self, index: usize) -> bool {
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

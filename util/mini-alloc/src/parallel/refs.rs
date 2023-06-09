use core::{fmt::Debug, hash, marker::PhantomData, ops::Range};

use alloc::fmt;

#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
#[cfg_attr(feature = "serde128", ignore_param(T))]
pub struct ParRef<T> {
    lane: u16,
    index: u32,
    _niche: bool,
    _marker: PhantomData<T>,
}

impl<T> ParRef<T> {
    pub(super) fn new(lane: usize, index: usize) -> Self {
        Self {
            lane: lane as u16,
            index: index as u32,
            _niche: false,
            _marker: PhantomData,
        }
    }

    pub fn as_slice(&self) -> ParSlice<T> {
        ParSlice {
            lane: self.lane,
            index: self.index,
            len: 1,
            _marker: PhantomData,
        }
    }

    pub(super) fn lane(&self) -> usize {
        self.lane as usize
    }

    pub(super) fn index(&self) -> usize {
        self.index as usize
    }
}

impl<T> fmt::Debug for ParRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParRef")
            .field("lane", &self.lane)
            .field("index", &self.index)
            .field("type", &core::any::type_name::<T>())
            .finish()
    }
}

impl<T> Copy for ParRef<T> {}

impl<T> Clone for ParRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for ParRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.lane == other.lane && self.index == other.index
    }
}

impl<T> Eq for ParRef<T> {}

impl<T> PartialOrd for ParRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for ParRef<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.lane
            .cmp(&other.lane)
            .then(self.index.cmp(&other.index))
    }
}

impl<T> hash::Hash for ParRef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.lane.hash(state);
        self.index.hash(state);
    }
}

#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
#[cfg_attr(feature = "serde128", ignore_param(T))]
pub struct ParSlice<T> {
    lane: u16,
    index: u32,
    len: u16,
    _marker: PhantomData<T>,
}

impl<T> ParSlice<T> {
    pub(super) fn new(lane: usize, range: Range<usize>) -> Self {
        Self {
            lane: lane as u16,
            index: range.start as u32,
            len: range.len() as u16,
            _marker: PhantomData,
        }
    }

    pub fn get(&self, index: usize) -> Option<ParRef<T>> {
        (index < self.len as usize).then_some(ParRef {
            lane: self.lane,
            index: self.index + index as u32,
            _niche: false,
            _marker: PhantomData,
        })
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = ParRef<T>> + DoubleEndedIterator + '_ {
        (self.index..self.index + self.len as u32).map(move |index| ParRef {
            lane: self.lane,
            index,
            _niche: false,
            _marker: PhantomData,
        })
    }

    pub(super) fn range(&self) -> Range<usize> {
        self.index as usize..self.index as usize + self.len as usize
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub(super) fn lane(&self) -> usize {
        self.lane as usize
    }
}

impl<T> Default for ParSlice<T> {
    fn default() -> Self {
        Self {
            lane: 0,
            index: 0,
            len: 0,
            _marker: PhantomData,
        }
    }
}

impl<T> Debug for ParSlice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParSlice")
            .field("lane", &self.lane)
            .field("index", &self.index)
            .field("len", &self.len)
            .field("type", &core::any::type_name::<T>())
            .finish()
    }
}

impl<T> Copy for ParSlice<T> {}

impl<T> Clone for ParSlice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for ParSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.lane == other.lane && self.index == other.index && self.len == other.len
    }
}

impl<T> Eq for ParSlice<T> {}

impl<T> PartialOrd for ParSlice<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for ParSlice<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.lane
            .cmp(&other.lane)
            .then(self.index.cmp(&other.index))
            .then(self.len.cmp(&other.len))
    }
}

impl<T> hash::Hash for ParSlice<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.lane.hash(state);
        self.index.hash(state);
        self.len.hash(state);
    }
}

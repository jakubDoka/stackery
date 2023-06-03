use std::{
    fmt::{Debug, Display},
    hash,
    marker::PhantomData,
    num::{NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize},
    ops::Range,
};

pub trait RefRepr: Copy + Eq + Ord + hash::Hash + Display + Debug + 'static {
    type TryFromError: Debug;
    fn try_from_usize(repr: usize) -> Result<Self, Self::TryFromError>;
    fn into_usize(self) -> usize;
    fn default() -> Self {
        Self::try_from_usize(0).unwrap()
    }
}

macro_rules! impl_ref_repr {
    ($($ty:ident)*) => {$(
        impl RefRepr for $ty {
            type TryFromError = <$ty as TryFrom<usize>>::Error;

            fn try_from_usize(repr: usize) -> Result<Self, Self::TryFromError> {
                repr.try_into()
            }

            fn into_usize(self) -> usize {
                self as usize
            }
        }
    )*};

    (@non_zero $($ty:ident $nonzero:ident)*) => {$(
        impl RefRepr for $nonzero {
            type TryFromError = <$ty as TryFrom<usize>>::Error;

            fn try_from_usize(repr: usize) -> Result<Self, Self::TryFromError> {
                (repr + 1).try_into().map(|repr| unsafe { Self::new_unchecked(repr) })
            }

            fn into_usize(self) -> usize {
                self.get() as usize - 1
            }
        }
    )*};
}

impl_ref_repr!(u8 u16 u32 u64 usize);
impl_ref_repr!(@non_zero u8 NonZeroU8 u16 NonZeroU16 u32 NonZeroU32 u64 NonZeroU64 usize NonZeroUsize);

#[derive(Debug)]
#[repr(transparent)]
pub struct Ref<T, R>(R, PhantomData<T>);

impl<T, R: PartialEq> PartialEq for Ref<T, R> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T, R: RefRepr> Eq for Ref<T, R> {}

impl<T, R: RefRepr> hash::Hash for Ref<T, R> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T, R: RefRepr> Clone for Ref<T, R> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T, R: RefRepr> Copy for Ref<T, R> {}

impl<T, R: RefRepr> Ord for Ref<T, R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T, R: RefRepr> PartialOrd for Ref<T, R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T, R: RefRepr> Ref<T, R> {
    pub(super) fn new(index: usize) -> Self {
        Self(
            R::try_from_usize(index).expect("exceeded cache entiry limit"),
            PhantomData,
        )
    }

    pub fn index(&self) -> usize {
        self.0.into_usize()
    }
}

#[derive(Debug)]
pub struct Slice<T, R>(R, R, PhantomData<T>);

impl<T, R: RefRepr> Slice<T, R> {
    pub(super) fn new(start: usize, end: usize) -> Self {
        Self(
            R::try_from_usize(start).expect("exceeded cache entiry limit"),
            R::try_from_usize(end).expect("exceeded cache entiry limit"),
            PhantomData,
        )
    }

    pub fn len(&self) -> usize {
        self.1.into_usize() - self.0.into_usize()
    }

    pub fn range(&self) -> Range<usize> {
        self.0.into_usize()..self.1.into_usize()
    }

    pub fn shifted_range(&self, shift: usize) -> Range<usize> {
        (self.0.into_usize() + shift)..(self.1.into_usize() + shift)
    }

    pub(crate) fn keys(&self) -> impl DoubleEndedIterator<Item = Ref<T, R>> + ExactSizeIterator {
        (self.0.into_usize()..self.1.into_usize()).map(Ref::new)
    }
}

impl<T, R: RefRepr> Default for Slice<T, R> {
    fn default() -> Self {
        Self(R::default(), R::default(), PhantomData)
    }
}

impl<T, R: Copy> Clone for Slice<T, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, R: Copy> Copy for Slice<T, R> {}

impl<T, R: PartialEq> PartialEq for Slice<T, R> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl<T, R: RefRepr> Eq for Slice<T, R> {}

impl<T, R: RefRepr> hash::Hash for Slice<T, R> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}

impl<T, R: RefRepr> Ord for Slice<T, R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0).then(self.1.cmp(&other.1))
    }
}

impl<T, R: RefRepr> PartialOrd for Slice<T, R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

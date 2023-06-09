use alloc::{alloc::Global, slice, vec::Vec};
use core::{
    alloc::Allocator,
    cell::UnsafeCell,
    hash::{BuildHasherDefault, Hash},
    intrinsics::unlikely,
    iter::TrustedLen,
    marker::PhantomData,
    mem,
    num::NonZeroUsize,
    ops::Index,
    ptr::{self, NonNull},
};
use hashbrown::{hash_map::RawEntryMut, HashMap, HashSet};

#[cfg(feature = "serde128")]
use serde_base128::Serde128;

#[derive(Default)]
pub struct StrInterner<A: Allocator + Clone = Global> {
    interner: Interner<u8, A>,
}

#[cfg(feature = "serde128")]
impl<A: Allocator + Clone + Default> Serde128 for StrInterner<A> {
    fn serialize(&self, encoder: &mut serde_base128::Encoder) {
        self.interner.serialize(encoder);
    }

    unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
        Self {
            interner: Interner::deserialize(decoder),
        }
    }
}

impl StrInterner {
    pub fn new(bucket_size: u16) -> Self {
        Self::new_in(bucket_size, Global)
    }
}

impl<A: Allocator + Clone> StrInterner<A> {
    pub fn new_in(bucket_size: u16, alloc: A) -> Self {
        Self {
            interner: Interner::new_in(bucket_size, alloc),
        }
    }

    pub fn intern(&self, value: &str) -> InternedStr {
        let id = self.interner.intern_slice(value.as_bytes());

        InternedStr { id }
    }
}

impl<A: Allocator + Clone> Index<InternedStr> for StrInterner<A> {
    type Output = str;

    fn index(&self, index: InternedStr) -> &Self::Output {
        let slice = &self.interner[index.id];
        unsafe { core::str::from_utf8_unchecked(slice) }
    }
}

#[derive(Clone, Default, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct InternedStr {
    id: InternedSlice<u8>,
}

type Lookup<T, A> = HashMap<HashView<T>, InternerSlice, BuildHasherDefault<InternerHasher>, A>;

pub struct Interner<T, A: Allocator + Clone = Global> {
    lookup: UnsafeCell<Lookup<T, A>>,
    allocator: UnsafeCell<InternerAllocator<T, A>>,
}

impl<T, A: Allocator + Clone + Default> Default for Interner<T, A> {
    fn default() -> Self {
        Self::new_in(u16::MAX / mem::size_of::<T>() as u16, A::default())
    }
}

impl<T> Interner<T> {
    pub fn new(bucket_size: u16) -> Self {
        Self::new_in(bucket_size, Global)
    }
}

impl<T, A: Allocator + Clone> Interner<T, A> {
    pub fn new_in(bucket_size: u16, alloc: A) -> Self {
        Self {
            lookup: HashMap::with_hasher_in(BuildHasherDefault::default(), alloc.clone()).into(),
            allocator: InternerAllocator::new_in(bucket_size, alloc).into(),
        }
    }

    unsafe fn state(&self) -> (&mut Lookup<T, A>, &mut InternerAllocator<T, A>) {
        (&mut *self.lookup.get(), &mut *self.allocator.get())
    }

    unsafe fn state_ref(&self) -> (&Lookup<T, A>, &InternerAllocator<T, A>) {
        (&*self.lookup.get(), &*self.allocator.get())
    }

    pub fn intern(&self, value: T) -> Interned<T>
    where
        T: Hash + Eq,
    {
        struct InternOne<T>(T);

        impl<T: Hash + Eq> Internable for InternOne<T> {
            type Element = T;

            fn values(&self) -> &[Self::Element] {
                slice::from_ref(&self.0)
            }

            unsafe fn move_to(self, ptr: *mut Self::Element) {
                ptr.write(self.0);
            }
        }

        let id = self.intern_untyped(InternOne(value));

        Interned {
            id: InternerObj {
                offset: id.offset,
                chunk: id.chunk,
            },
            _marker: PhantomData,
        }
    }

    pub fn intern_slice<I>(&self, values: I) -> InternedSlice<T>
    where
        I: Internable<Element = T>,
        T: Hash + Eq,
    {
        let id = self.intern_untyped(values);

        InternedSlice {
            id,
            _marker: PhantomData,
        }
    }

    pub fn intern_untyped<I>(&self, values: I) -> InternerSlice
    where
        I: Internable<Element = T>,
        T: Hash + Eq,
    {
        let Some(len) = NonZeroUsize::new(values.values().len()) else {
            return InternerSlice::default();
        };

        let hash = HashView::new(values.values());

        unsafe { self.intern_untyped_low(values, len, hash) }
    }

    pub fn intern_iter<I>(&self, values: I) -> InternedSlice<T>
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
        T: Hash + Eq,
    {
        let values = values.into_iter();
        let Some(len) = NonZeroUsize::new(values.len()) else {
            return InternedSlice::default();
        };

        let (ptr, offset) = {
            let (_, allocator) = unsafe { self.state() };
            allocator.bump(len)
        };
        unsafe {
            let mut ptr = ptr.as_ptr();
            for value in values {
                ptr.write(value);
                ptr = ptr.add(1);
            }
        }

        let hash = HashView::new(unsafe { slice::from_raw_parts(ptr.as_ptr(), len.get()) });

        let (lookup, allocator) = unsafe { self.state() };

        let entry = lookup.raw_entry_mut().from_key(&hash);

        let entry = match entry {
            RawEntryMut::Occupied(entry) => {
                unsafe { allocator.rewind(len) };
                return InternedSlice {
                    id: *entry.get(),
                    _marker: PhantomData,
                };
            }
            RawEntryMut::Vacant(entry) => entry,
        };

        let id = InternerSlice {
            offset: offset as u16,
            len: len.get() as u16,
            chunk: allocator.buckets.len() as u16 - 1,
        };

        entry.insert(hash, id);

        InternedSlice {
            id,
            _marker: PhantomData,
        }
    }

    pub fn try_intern_iter<I, E>(&self, values: I) -> Result<InternedSlice<T>, E>
    where
        I: IntoIterator<Item = Result<T, E>>,
        I::IntoIter: TrustedLen + ExactSizeIterator,
        T: Hash + Eq,
    {
        let values = values.into_iter();
        let Some(len) = NonZeroUsize::new(values.len()) else {
            return Ok(InternedSlice::default());
        };

        let (ptr, offset) = {
            let (_, allocator) = unsafe { self.state() };
            allocator.bump(len)
        };
        unsafe {
            let mut ptr = ptr.as_ptr();
            for value in values {
                let value = match value {
                    Ok(value) => value,
                    Err(e) => {
                        {
                            let (_, allocator) = self.state();
                            allocator.rewind(len);
                        }
                        return Err(e);
                    }
                };
                ptr.write(value);
                ptr = ptr.add(1);
            }
        }

        let hash = HashView::new(unsafe { slice::from_raw_parts(ptr.as_ptr(), len.get()) });

        let (lookup, allocator) = unsafe { self.state() };

        let entry = lookup.raw_entry_mut().from_key(&hash);

        let entry = match entry {
            RawEntryMut::Occupied(entry) => {
                unsafe { allocator.rewind(len) };
                return Ok(InternedSlice {
                    id: *entry.get(),
                    _marker: PhantomData,
                });
            }
            RawEntryMut::Vacant(entry) => entry,
        };

        let id = InternerSlice {
            offset: offset as u16,
            len: len.get() as u16,
            chunk: allocator.buckets.len() as u16 - 1,
        };

        entry.insert(hash, id);

        Ok(InternedSlice {
            id,
            _marker: PhantomData,
        })
    }

    unsafe fn intern_untyped_low<I>(
        &self,
        values: I,
        len: NonZeroUsize,
        mut hash: HashView<T>,
    ) -> InternerSlice
    where
        I: Internable<Element = T>,
        T: Hash + Eq,
    {
        let (lookup, allocator) = self.state();

        let entry = lookup.raw_entry_mut().from_key(&hash);

        let entry = match entry {
            RawEntryMut::Occupied(entry) => return *entry.get(),
            RawEntryMut::Vacant(entry) => entry,
        };

        let (ptr, offset) = allocator.bump(len);
        unsafe {
            values.move_to(ptr.as_ptr());
        }

        let id = InternerSlice {
            offset: offset as u16,
            len: len.get() as u16,
            chunk: allocator.buckets.len() as u16 - 1,
        };

        hash.data = ptr::slice_from_raw_parts(ptr.as_ptr(), len.get());

        entry.insert(hash, id);

        id
    }
}

#[cfg(feature = "serde128")]
impl<T: Serde128 + Hash + Eq, A: Allocator + Clone + Default> serde_base128::Serde128
    for Interner<T, A>
{
    fn serialize(&self, encoder: &mut serde_base128::Encoder) {
        let (lookup, allocator) = unsafe { self.state_ref() };
        allocator.serialize(encoder);
        lookup.len().serialize(encoder);
        for val in lookup.values() {
            val.serialize(encoder);
        }
    }

    unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
        let allocator = InternerAllocator::<T, A>::deserialize(decoder);

        let len = usize::deserialize(decoder);
        let mut lookup =
            Lookup::<T, A>::with_capacity_and_hasher_in(len, Default::default(), A::default());

        for _ in 0..len {
            let index = InternerSlice::deserialize(decoder);

            let bucket = &allocator.buckets[index.chunk as usize];
            let offset = index.offset as usize;
            let len = index.len as usize;

            let slice = slice::from_raw_parts(bucket.ptr.as_mut_ptr().add(offset), len);

            let hash = HashView::new(slice);
            lookup.insert(hash, index);
        }

        Self {
            lookup: lookup.into(),
            allocator: allocator.into(),
        }
    }
}

impl<T, A: Allocator + Clone> Index<InternerSlice> for Interner<T, A> {
    type Output = [T];

    fn index(&self, index: InternerSlice) -> &Self::Output {
        let bucket = unsafe { &self.state_ref().1.buckets[index.chunk as usize] };
        let offset = index.offset as usize;
        let len = index.len as usize;

        assert!(offset + len <= bucket.taken);

        unsafe { slice::from_raw_parts(bucket.ptr.as_mut_ptr().add(offset), len) }
    }
}

impl<T, A: Allocator + Clone> Index<InternerObj> for Interner<T, A> {
    type Output = T;

    fn index(&self, index: InternerObj) -> &Self::Output {
        let bucket = unsafe { &self.state_ref().1.buckets[index.chunk as usize] };
        let offset = index.offset as usize;

        assert!(offset < bucket.taken);

        unsafe { &*bucket.ptr.as_mut_ptr().add(offset) }
    }
}

impl<T, A: Allocator + Clone> Index<Interned<T>> for Interner<T, A> {
    type Output = T;

    fn index(&self, index: Interned<T>) -> &Self::Output {
        &self[index.id]
    }
}

impl<T, A: Allocator + Clone> Index<InternedSlice<T>> for Interner<T, A> {
    type Output = [T];

    fn index(&self, index: InternedSlice<T>) -> &Self::Output {
        &self[index.id]
    }
}

pub trait Internable {
    type Element: Hash + Eq;

    fn values(&self) -> &[Self::Element];
    unsafe fn move_to(self, ptr: *mut Self::Element);

    fn len(&self) -> usize {
        self.values().len()
    }
}

impl<T: Hash + Eq, O: Allocator> Internable for &mut Vec<T, O> {
    type Element = T;

    fn values(&self) -> &[Self::Element] {
        self
    }

    unsafe fn move_to(self, ptr: *mut Self::Element) {
        self.set_len(0);
        ptr::copy_nonoverlapping(self.as_ptr(), ptr, self.len());
    }
}

impl<T: Hash + Eq + Clone> Internable for &[T] {
    type Element = T;

    fn values(&self) -> &[Self::Element] {
        self
    }

    unsafe fn move_to(self, ptr: *mut Self::Element) {
        for (i, v) in self.iter().enumerate() {
            unsafe { ptr::write(ptr.add(i), v.clone()) };
        }
    }
}

struct InternerAllocator<T, A: Allocator + Clone> {
    bucket_size: usize,
    current_alloc: *mut T,
    current_len: *mut usize,
    buckets: Vec<Bucket<T>, A>,
    alloc: A,
}

impl<T, A: Allocator + Clone> InternerAllocator<T, A> {
    fn new_in(bucket_size: u16, alloc: A) -> Self {
        Self {
            bucket_size: bucket_size as usize,
            current_alloc: ptr::null_mut(),
            current_len: &(u16::MAX as usize) as *const usize as *mut usize,
            buckets: Vec::new_in(alloc.clone()),
            alloc,
        }
    }

    fn bump(&mut self, len: NonZeroUsize) -> (NonNull<T>, usize) {
        let len = len.get();
        let mut current_len = unsafe { self.current_len.read() };

        if unlikely(current_len + len > self.bucket_size) {
            let bucket = self.create_bucket(len);
            self.buckets.push(Bucket::new(bucket));

            self.current_alloc = bucket.as_mut_ptr();
            let last = self.buckets.last_mut().expect("c'mon, we just pushed");
            self.current_len = &mut last.taken;
            current_len = 0;
        }

        let alloc_ptr = unsafe { self.current_alloc.add(current_len) };
        unsafe { *self.current_len += len };

        (unsafe { NonNull::new_unchecked(alloc_ptr) }, current_len)
    }

    fn create_bucket(&self, required_len: usize) -> NonNull<[T]> {
        Self::create_bucket_low(self.bucket_size.max(required_len), &self.alloc)
    }

    fn create_bucket_low(bucket_size: usize, alloc: &A) -> NonNull<[T]> {
        let layout = core::alloc::Layout::array::<T>(bucket_size).unwrap();
        let (ptr, len) = alloc.allocate(layout).unwrap().to_raw_parts();
        unsafe {
            NonNull::new_unchecked(ptr::slice_from_raw_parts_mut(
                ptr.as_ptr().cast(),
                len / mem::size_of::<T>(),
            ))
        }
    }

    unsafe fn rewind(&self, len: NonZeroUsize) {
        *self.current_len -= len.get();
    }
}

#[cfg(feature = "serde128")]
impl<T: Serde128, A: Allocator + Clone + Default> Serde128 for InternerAllocator<T, A> {
    fn serialize(&self, encoder: &mut serde_base128::Encoder) {
        self.bucket_size.serialize(encoder);
        self.buckets.len().serialize(encoder);
        for bucket in self.buckets.iter() {
            bucket.taken.serialize(encoder);
            let slice = unsafe { slice::from_raw_parts(bucket.ptr.as_mut_ptr(), bucket.taken) };
            for v in slice.iter() {
                v.serialize(encoder);
            }
        }
    }

    unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
        let alloc = A::default();
        let bucket_size = u16::deserialize(decoder);

        let bucket_count = usize::deserialize(decoder);

        let Some(one_less) = bucket_count.checked_sub(1) else {
            return Self::new_in(bucket_size, alloc);
        };

        let mut buckets = Vec::with_capacity_in(bucket_count, alloc.clone());

        for _ in 0..one_less {
            let taken = usize::deserialize(decoder);
            let bucket = Self::create_bucket_low(taken, &alloc);
            buckets.push(Bucket { ptr: bucket, taken });
            let base = bucket.as_mut_ptr();
            for i in 0..taken {
                *base.add(i) = T::deserialize(decoder);
            }
        }

        let taken = usize::deserialize(decoder);
        let bucket = Self::create_bucket_low((bucket_size as usize).max(taken), &alloc);
        let base = bucket.as_mut_ptr();
        for i in 0..taken {
            *base.add(i) = T::deserialize(decoder);
        }
        buckets.push(Bucket { ptr: bucket, taken });
        let last = buckets.last_mut().expect("c'mon, we just pushed");

        Self {
            bucket_size: bucket_size as usize,
            current_alloc: bucket.as_mut_ptr(),
            current_len: &mut last.taken,
            buckets,
            alloc,
        }
    }
}

impl<T, A: Allocator + Clone> Drop for InternerAllocator<T, A> {
    fn drop(&mut self) {
        for bucket in self.buckets.iter() {
            unsafe {
                let ptr_slice =
                    ptr::slice_from_raw_parts_mut(bucket.ptr.as_mut_ptr(), bucket.taken);
                ptr::drop_in_place(ptr_slice);

                let size = bucket.ptr.len();
                let layout = core::alloc::Layout::array::<T>(size).unwrap();
                self.alloc.deallocate(bucket.ptr.cast(), layout);
            }
        }
    }
}

struct Bucket<T> {
    ptr: NonNull<[T]>,
    taken: usize,
}

impl<T> Bucket<T> {
    fn new(bucket: NonNull<[T]>) -> Bucket<T> {
        Self {
            ptr: bucket,
            taken: 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, PartialOrd, Ord)]
#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
pub struct InternerSlice {
    offset: u16,
    len: u16,
    chunk: u16,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
pub struct InternerObj {
    offset: u16,
    chunk: u16,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
#[cfg_attr(feature = "serde128", ignore_param(T))]
pub struct InternedSlice<T> {
    id: InternerSlice,
    _marker: PhantomData<T>,
}

impl<T> Default for InternedSlice<T> {
    fn default() -> Self {
        Self {
            id: InternerSlice::default(),
            _marker: PhantomData,
        }
    }
}

#[cfg_attr(feature = "serde128", derive(serde_base128::Serde128))]
#[cfg_attr(feature = "serde128", ignore_param(T))]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct Interned<T> {
    id: InternerObj,
    _marker: PhantomData<T>,
}

struct HashView<T> {
    hash: u64,
    data: *const [T],
}

impl<T: Hash> HashView<T> {
    fn new(data: &[T]) -> Self {
        use core::hash::Hasher;
        let mut hasher = FnvHasher::default();
        data.hash(&mut hasher);

        Self {
            hash: hasher.finish(),
            data,
        }
    }
}

impl<T> Hash for HashView<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state)
    }
}

impl<T: PartialEq> PartialEq for HashView<T> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && unsafe { self.data.as_ref() == other.data.as_ref() }
    }
}

impl<T: Eq> Eq for HashView<T> {}

#[derive(Default)]
struct InternerHasher {
    hash: u64,
}

impl core::hash::Hasher for InternerHasher {
    fn finish(&self) -> u64 {
        self.hash
    }

    fn write(&mut self, bytes: &[u8]) {
        self.hash = unsafe { ptr::read_unaligned(bytes.as_ptr().cast::<u64>()) };
    }
}

pub type FnvBuildHasher = core::hash::BuildHasherDefault<FnvHasher>;
pub type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;
pub type FnvHashSet<T> = HashSet<T, FnvBuildHasher>;

pub struct FnvHasher {
    hash: u64,
}

impl Default for FnvHasher {
    fn default() -> Self {
        Self {
            hash: 0xcbf29ce484222325,
        }
    }
}

impl core::hash::Hasher for FnvHasher {
    fn finish(&self) -> u64 {
        self.hash
    }

    fn write(&mut self, bytes: &[u8]) {
        let mut hash = self.hash;
        for byte in bytes {
            hash = hash.wrapping_mul(0x100000001b3);
            hash ^= *byte as u64;
        }
        self.hash = hash;
    }
}

#[cfg(test)]
mod test {
    use alloc::string::String;

    use super::*;

    #[test]
    fn test_str_interning() {
        let interner = StrInterner::new(6);

        let a = interner.intern("hello");
        let b = interner.intern("world of rust");

        assert_eq!(a, interner.intern("hello"));
        assert_eq!(b, interner.intern("world of rust"));

        assert_eq!(&interner[a], "hello");
        assert_eq!(&interner[b], "world of rust");
    }

    #[test]
    fn arbitrary_interning() {
        let interner = Interner::new(6);

        #[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
        struct Point {
            x: i32,
            y: i32,
        }

        let pa = Point { x: 42, y: 1337 };
        let pb = Point { x: 1337, y: 42 };

        let a = interner.intern(pa);
        let b = interner.intern(pb);

        assert_eq!(a, interner.intern(pa));
        assert_eq!(b, interner.intern(pb));

        assert_eq!(interner[a], pa);
        assert_eq!(interner[b], pb);
    }

    #[test]
    fn correct_dealloc() {
        let i = Interner::new(1);

        let sa = String::from("hello");
        let sb = String::from("world of rust");

        i.intern(sa);
        i.intern(sb);
    }

    #[cfg(feature = "serde128")]
    #[test]
    fn serde() {
        let interner = StrInterner::new(6);

        let a = interner.intern("hello");
        let b = interner.intern("world of rust");

        let mut writer = serde_base128::Encoder::new();
        interner.serialize(&mut writer);
        interner.serialize(&mut writer);

        let buf = writer.into_vec();
        let mut reader = serde_base128::Decoder::new(&buf[..]);
        let c = unsafe { StrInterner::<Global>::deserialize(&mut reader) };
        let d = unsafe { StrInterner::<Global>::deserialize(&mut reader) };

        assert_eq!(a, c.intern("hello"));
        assert_eq!(b, c.intern("world of rust"));
        assert_eq!(a, d.intern("hello"));
        assert_eq!(b, d.intern("world of rust"));
    }
}

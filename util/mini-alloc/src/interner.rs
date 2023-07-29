use alloc::{alloc::Global, slice, vec::Vec};
use core::{
    alloc::Allocator,
    cell::UnsafeCell,
    hash::{BuildHasher, BuildHasherDefault, Hash},
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

type IdentStrRepr = u32;
const ENCODED_STR_LEN_SIZE: usize = 4;

fn encode_str_len(len: usize) -> [u8; ENCODED_STR_LEN_SIZE] {
    let component_width = 7;
    let component_mask = (1 << component_width) - 1;
    [
        (len & component_mask) as u8,
        ((len >> component_width) & component_mask) as u8,
        ((len >> component_width * 2) & component_mask) as u8,
        0xff,
    ]
}

fn decode_str_len(bytes: [u8; ENCODED_STR_LEN_SIZE]) -> Option<usize> {
    let component_width = 7;

    if unlikely(bytes[3] != 0xff) {
        return None;
    }

    Some(
        bytes[0] as usize
            | (bytes[1] as usize) << component_width
            | (bytes[2] as usize) << component_width * 2,
    )
}

pub struct StrInterner<A: Allocator + Clone = Global> {
    lookup: FnvHashMap<IdentStrRepr, (), A>,
    strings: Vec<u8, A>,
}

impl<A: Allocator + Clone + Default> Default for StrInterner<A> {
    fn default() -> Self {
        Self::new_in(A::default())
    }
}

#[cfg(feature = "serde128")]
impl<A: Allocator + Clone + Default> Serde128 for StrInterner<A> {
    fn serialize(&self, encoder: &mut serde_base128::Encoder) {
        self.lookup.len().serialize(encoder);
        self.strings.len().serialize(encoder);
        encoder.write_slice(&self.strings);
    }

    unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
        let string_count = usize::deserialize(decoder);
        let len = usize::deserialize(decoder);
        let strings = decoder.read_slice(len);
        let mut lookup = FnvHashMap::with_capacity_and_hasher_in(
            string_count,
            FnvBuildHasher::default(),
            A::default(),
        );

        let mut string_decoder = strings;
        for _ in 0..string_count {
            let offset = len - string_decoder.len();
            let str = Self::read_str_from_buffer(&mut string_decoder);
            let hash = lookup.hasher().hash_one(str);
            lookup
                .raw_entry_mut()
                // all strings are unique so comparison is useless
                .from_hash(hash, |_| false)
                .insert(offset as u32, ());
        }

        Self {
            lookup,
            strings: {
                let mut vec = Vec::new_in(A::default());
                vec.extend_from_slice(strings);
                vec
            },
        }
    }
}

impl StrInterner {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<A: Allocator + Clone> StrInterner<A> {
    pub fn new_in(alloc: A) -> Self {
        Self {
            lookup: FnvHashMap::with_hasher_in(FnvBuildHasher::default(), alloc.clone()),
            strings: Vec::new_in(alloc),
        }
    }

    pub fn intern(&mut self, value: &str) -> IdentStr {
        let hash = self.lookup.hasher().hash_one(value);
        let offset = self.intern_low(hash, value);
        IdentStr::new(offset)
    }

    pub fn intern_low(&mut self, hash: u64, value: &str) -> u32 {
        let entry = self.lookup.raw_entry_mut().from_hash(hash, |&key| {
            let str = unsafe { Self::get_str_unchecked(&self.strings, key) };
            str == value
        });

        let offset = match entry {
            RawEntryMut::Occupied(value) => return *value.key(),
            RawEntryMut::Vacant(vacant) => {
                *vacant
                    .insert_hashed_nocheck(hash, self.strings.len() as IdentStrRepr, ())
                    .0
            }
        };

        let total_len = ENCODED_STR_LEN_SIZE + value.len();

        self.strings.reserve(total_len);

        self.strings.extend_from_slice(&encode_str_len(value.len()));
        self.strings.extend_from_slice(value.as_bytes());

        offset
    }

    fn get_str(&self, key: IdentStrRepr) -> &str {
        let offset = key as usize;

        let mut slice = self.strings.get(offset..).expect("invalid string offset");

        if slice.len() < ENCODED_STR_LEN_SIZE {
            unreachable!("invalid string offset");
        }

        unsafe { Self::read_str_from_buffer(&mut slice) }
    }

    unsafe fn get_str_unchecked(strings: &[u8], key: IdentStrRepr) -> &str {
        let offset = key as usize;
        let mut slice = strings.get_unchecked(offset..);
        Self::read_str_from_buffer(&mut slice)
    }

    unsafe fn read_str_from_buffer<'a>(buffer: &mut &'a [u8]) -> &'a str {
        let len_bytes = (buffer.as_ptr() as *const [u8; ENCODED_STR_LEN_SIZE]).read();
        let len = decode_str_len(len_bytes).expect("failed to decode string length");
        let slice = slice::from_raw_parts(buffer.as_ptr().add(ENCODED_STR_LEN_SIZE), len);
        *buffer = buffer.get_unchecked(len + ENCODED_STR_LEN_SIZE..);
        core::str::from_utf8_unchecked(slice)
    }
}

impl<A: Allocator + Clone> Index<IdentStr> for StrInterner<A> {
    type Output = str;

    fn index(&self, index: IdentStr) -> &Self::Output {
        self.get_str(index.index())
    }
}

#[derive(Clone, Default, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct IdentStr {
    id: [u8; mem::size_of::<IdentStrRepr>()],
}
impl IdentStr {
    fn new(key: u32) -> IdentStr {
        IdentStr {
            id: key.to_ne_bytes(),
        }
    }

    pub fn index(self) -> IdentStrRepr {
        IdentStrRepr::from_ne_bytes(self.id)
    }
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
pub type FnvHashMap<K, V, A = Global> = HashMap<K, V, FnvBuildHasher, A>;
pub type FnvHashSet<T, A = Global> = HashSet<T, FnvBuildHasher, A>;

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
    use core::hash::Hasher;
    use std::{sync::RwLock, thread, time::Instant};

    use alloc::{string::String, sync::Arc};

    use crate::ident_experiment::{comparing_impl::IdentStr, Ident};

    use super::*;

    #[test]
    fn test_str_interning() {
        let mut interner = StrInterner::new();

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
        let mut interner = StrInterner::new();

        let a = interner.intern("hello");
        let b = interner.intern("world of rust");

        let mut writer = serde_base128::Encoder::new();
        interner.serialize(&mut writer);
        interner.serialize(&mut writer);

        let buf = writer.into_vec();
        let mut reader = serde_base128::Decoder::new(&buf[..]);
        let mut c = unsafe { StrInterner::<Global>::deserialize(&mut reader) };
        let mut d = unsafe { StrInterner::<Global>::deserialize(&mut reader) };

        assert_eq!(a, c.intern("hello"));
        assert_eq!(b, c.intern("world of rust"));
        assert_eq!(a, d.intern("hello"));
        assert_eq!(b, d.intern("world of rust"));
    }

    struct SyncStrInterner {
        interner: Arc<[spin::Mutex<StrInterner>]>,
    }

    impl SyncStrInterner {
        fn new() -> Self {
            Self {
                interner: (0..16)
                    .map(|_| spin::Mutex::new(StrInterner::new()))
                    .collect::<Vec<_>>()
                    .into(),
            }
        }

        fn intern(&self, s: &str) -> IdentStr {
            let mut hasher = FnvHasher::default();
            s.hash(&mut hasher);
            let hash = hasher.finish();

            let mut interner = self.interner[hash as usize & (self.interner.len() - 1)].lock();
            interner.intern(s)
        }
    }

    #[ignore]
    #[test]
    fn compare_str_interner_specialization_performance() {
        let iter_count = 10_000_000;
        let modulo = 20;
        let thread_count = 4;

        let mut num_buffer = String::new();
        let set_buffer = |mut n: usize, buf: &mut String| {
            let make_big = n & 1 == 0;
            n &= (1 << modulo) - 1;
            buf.clear();
            while n > 0 {
                buf.push(char::from_digit((n & (32 - 1)) as u32, 32).unwrap());
                n >>= 5;
            }
            if make_big {
                buf.push_str("hello world and gorld");
            }
        };

        let mut interner = StrInterner::new();
        let now = Instant::now();
        for i in 0..iter_count {
            set_buffer(i, &mut num_buffer);
            let ident = interner.intern(&num_buffer);
            assert_eq!(&interner[ident], &num_buffer);
        }
        println!(
            "StrInterner: {:?}",
            now.elapsed().checked_div(iter_count as u32).unwrap()
        );

        let interner = Interner::<u8, Global>::default();
        let now = Instant::now();
        for i in 0..iter_count {
            set_buffer(i, &mut num_buffer);
            interner.intern_slice(num_buffer.as_bytes());
        }
        println!(
            "Interner: {:?}",
            now.elapsed().checked_div(iter_count as u32).unwrap()
        );

        let interner = SyncStrInterner::new();
        let now = thread::scope(|s| {
            (0..thread_count)
                .map(|_| {
                    s.spawn(|| {
                        let mut num_buffer = String::new();
                        let now = Instant::now();
                        for i in 0..iter_count {
                            set_buffer(i, &mut num_buffer);
                            interner.intern(&num_buffer);
                        }
                        now.elapsed()
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|h| h.join().unwrap())
                .sum::<std::time::Duration>()
        });
        println!(
            "SyncStrInterner: {:?}",
            now.checked_div((iter_count * thread_count) as u32).unwrap()
        );

        let now = thread::scope(|s| {
            (0..thread_count)
                .map(|_| {
                    s.spawn(|| {
                        let mut num_buffer = String::new();
                        let now = Instant::now();
                        for i in 0..iter_count {
                            set_buffer(i, &mut num_buffer);
                            let ident = Ident::from_str(&num_buffer);
                            assert_eq!(ident.as_str(), &num_buffer);
                        }
                        now.elapsed()
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|h| h.join().unwrap())
                .sum::<std::time::Duration>()
        });
        println!(
            "PackedIdent: {:?}",
            now.checked_div((iter_count * thread_count) as u32).unwrap()
        );

        let now = thread::scope(|s| {
            (0..thread_count)
                .map(|_| {
                    s.spawn(|| {
                        let mut num_buffer = String::new();
                        let now = Instant::now();
                        for i in 0..iter_count {
                            set_buffer(i, &mut num_buffer);
                            let ident = IdentStr::from_str(&num_buffer);
                            assert_eq!(ident.as_str(), &num_buffer);
                        }
                        now.elapsed()
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|h| h.join().unwrap())
                .sum::<std::time::Duration>()
        });
        println!(
            "RocIdent: {:?}",
            now.checked_div((iter_count * thread_count) as u32).unwrap()
        );
    }
}

//! Implements data structures used for efficiently representing small strings, like identifiers.
#![warn(clippy::dbg_macro)]

use core::cmp::Ordering;
use core::convert::From;
use core::{fmt, mem, ptr, slice};
use std::alloc::{alloc, dealloc, Layout};
use std::mem::ManuallyDrop;
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::os::raw::c_char;
use std::ptr::NonNull;

/// A string which can store identifiers using the small string optimization.
/// It relies on the invariant that it cannot store null characters to store
/// an extra character; if the last byte is 0, that means it's a large string.
///
/// Because the msbyte of the length is always 0, this can only store up to
/// 2^56 bytes on a 64-bit target, or 2^28 bytes in a 32-bit target. That's
/// way more than enough for an identifier!
///
/// If it's a small string, that discriminant byte is used to store the length,
/// except it stores it as (255 - length) so that it will be in the range
/// 192 - 255 (all of which are invalid UTF-8 when in the final position of
/// a UTF-8 string). This design works on little-endian targets, but a different
/// design for storing length might be necessary on big-endian targets.

pub struct ArcStr {
    ptr: NonNull<u8>,
    len: usize,
}

type ArcStrRefCount = std::sync::atomic::AtomicU32;

impl ArcStr {
    const EMPTY: Self = Self {
        ptr: NonNull::dangling(),
        len: 0,
    };

    const COUNTER_SIZE: usize = mem::size_of::<ArcStrRefCount>();
    const COUNTER_ALIGN: usize = mem::align_of::<ArcStrRefCount>();

    pub fn new(value: &str) -> Self {
        let Some(len) = NonZeroUsize::new(value.len()) else {
            return Self::EMPTY;
        };

        let layout = Self::layout_for(len.get());
        let base = unsafe { alloc(layout) };

        unsafe { base.cast::<ArcStrRefCount>().write(ArcStrRefCount::new(1)) };

        let ptr = unsafe { base.add(Self::COUNTER_SIZE) };
        unsafe { ptr::copy_nonoverlapping(value.as_ptr(), ptr, len.get()) };

        Self {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            len: len.get(),
        }
    }

    fn layout_for(len: usize) -> Layout {
        let size = Self::COUNTER_SIZE + len;
        unsafe { Layout::from_size_align_unchecked(size, Self::COUNTER_ALIGN) }
    }

    fn get_ref_count(&self) -> &ArcStrRefCount {
        let base = unsafe { self.ptr.as_ptr().sub(Self::COUNTER_SIZE) };
        unsafe { &*base.cast() }
    }

    fn drop_ref(&mut self) -> bool {
        self.get_ref_count()
            .fetch_sub(1, std::sync::atomic::Ordering::Relaxed)
            == 1
    }

    fn inc_ref(&self) -> Self {
        self.get_ref_count()
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        Self {
            ptr: self.ptr,
            len: self.len,
        }
    }

    fn as_bytes(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl Clone for ArcStr {
    fn clone(&self) -> Self {
        self.inc_ref()
    }
}

impl Drop for ArcStr {
    fn drop(&mut self) {
        if !self.drop_ref() {
            return;
        }

        let layout = Self::layout_for(self.len);
        unsafe { dealloc(self.ptr.as_ptr().sub(Self::COUNTER_SIZE), layout) };
    }
}

#[repr(C)]
pub union IdentStr {
    big: ManuallyDrop<ArcStr>,
    small: [u8; 16],
}

impl IdentStr {
    // Reserve 1 byte for the discriminant
    const SIZE: usize = std::mem::size_of::<Self>();

    pub const fn last_byte(&self) -> u8 {
        unsafe { self.small[Self::SIZE - 1] }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        let last_byte = self.last_byte();

        // We always perform this subtraction so that the following
        // conditionals can all be cmov instructions.
        let small_str_variable_len = (u8::MAX - last_byte) as usize;

        // The numbers 192 - 255 (0xC0 - 0xFF) are not valid as the final
        // byte of a UTF-8 string. Hence they are unused and we can use them
        // to store the length of a small string!
        //
        // Reference: https://en.wikipedia.org/wiki/UTF-8#Codepage_layout
        if last_byte >= 0xC0 {
            small_str_variable_len
        } else if last_byte == 0 {
            // This is a big string, so return its length.
            unsafe { self.big.len }
        } else {
            // This is a valid UTF-8 character, meaning the entire struct must
            // be in use for storing characters.
            mem::size_of::<IdentStr>()
        }
    }

    pub fn is_empty(&self) -> bool {
        unsafe { self.big.len == 0 }
    }

    pub const fn is_small_str(&self) -> bool {
        self.last_byte() != 0
    }

    pub fn get(&self, index: usize) -> Option<&u8> {
        self.as_bytes().get(index)
    }

    pub fn get_bytes(&self) -> *const u8 {
        if self.is_small_str() {
            self.get_small_str_ptr()
        } else {
            unsafe { self.big.ptr.as_ptr() }
        }
    }

    fn get_small_str_ptr(&self) -> *const u8 {
        (self as *const IdentStr).cast()
    }

    #[inline(always)]
    const fn small_str_from_bytes(slice: &[u8]) -> Self {
        assert!(slice.len() <= Self::SIZE - 1);

        let len = slice.len();
        let mut bytes = [0; mem::size_of::<Self>()];

        // Copy the bytes from the slice into bytes.
        // while because for/Iterator does not work in const context
        let mut i = 0;
        while i < len {
            bytes[i] = slice[i];
            i += 1;
        }

        // Write length and small string bit to last byte of length.
        bytes[Self::SIZE - 1] = u8::MAX - len as u8;

        Self { small: bytes }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: &str) -> Self {
        let slice = str.as_bytes();
        let len = slice.len();

        match len.cmp(&mem::size_of::<Self>()) {
            Ordering::Less => Self::small_str_from_bytes(slice),
            Ordering::Equal => {
                // This fits in a small string, and is exactly long enough to
                // take up the entire available struct
                let mut bytes = [0; mem::size_of::<Self>()];

                // Copy the bytes from the slice into the answer
                bytes.copy_from_slice(slice);

                Self { small: bytes }
            }
            Ordering::Greater => {
                let big = ArcStr::new(str);
                Self {
                    big: ManuallyDrop::new(big),
                }
            }
        }
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[u8] {
        use core::slice::from_raw_parts;

        if self.is_empty() {
            &[]
        } else if self.is_small_str() {
            unsafe { from_raw_parts(self.get_small_str_ptr(), self.len()) }
        } else {
            unsafe { self.big.as_bytes() }
        }
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        let slice = self.as_slice();

        unsafe { core::str::from_utf8_unchecked(slice) }
    }

    /// Write a CStr (null-terminated) representation of this IdentStr into
    /// the given buffer.
    ///
    /// # Safety
    /// This assumes the given buffer has enough space, so make sure you only
    /// pass in a pointer to an allocation that's at least as long as this Str!
    pub unsafe fn write_c_str(&self, buf: *mut c_char) {
        let bytes = self.as_bytes();
        ptr::copy_nonoverlapping(bytes.as_ptr().cast(), buf, bytes.len());

        // null-terminate
        *buf.add(self.len()) = 0;
    }
}

impl Default for IdentStr {
    fn default() -> Self {
        Self {
            small: [0; mem::size_of::<Self>()],
        }
    }
}

impl std::ops::Deref for IdentStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl From<&str> for IdentStr {
    fn from(str: &str) -> Self {
        Self::from_str(str)
    }
}

impl From<IdentStr> for String {
    fn from(ident_str: IdentStr) -> Self {
        ident_str.as_str().to_string()
    }
}

impl From<String> for IdentStr {
    fn from(string: String) -> Self {
        Self::from_str(string.as_str())
    }
}

impl fmt::Debug for IdentStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // IdentStr { is_small_str: false, storage: Refcounted(3), elements: [1,2,3,4] }
        f.debug_struct("IdentStr")
            //.field("is_small_str", &self.is_small_str())
            .field("string", &self.as_str())
            //.field("elements", &self.as_slice())
            .finish()
    }
}

impl fmt::Display for IdentStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // IdentStr { is_small_str: false, storage: Refcounted(3), elements: [1,2,3,4] }
        f.write_str(self.as_str())
    }
}

unsafe impl std::marker::Sync for IdentStr {}
unsafe impl std::marker::Send for IdentStr {}

impl PartialEq for IdentStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for IdentStr {}

impl PartialOrd for IdentStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for IdentStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl std::hash::Hash for IdentStr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.as_str().hash(hasher)
    }
}

impl Clone for IdentStr {
    fn clone(&self) -> Self {
        if self.is_empty() || self.is_small_str() {
            // we can just copy the bytes
            Self {
                small: unsafe { self.small },
            }
        } else {
            // we need to clone the Arc
            Self {
                big: ManuallyDrop::new(unsafe { self.big.deref().clone() }),
            }
        }
    }
}

impl Drop for IdentStr {
    fn drop(&mut self) {
        if !self.is_empty() && !self.is_small_str() {
            unsafe {
                ManuallyDrop::drop(&mut self.big);
            }
        }
    }
}

#[test]
fn default() {
    let answer = IdentStr::default();

    assert_eq!(answer.len(), 0);
    assert_eq!(answer, answer);
    assert_eq!(answer.clone(), answer);
    assert_eq!(answer, answer);
    assert_eq!(answer.as_str(), "");
    assert_eq!(answer.as_str(), "");
}

#[test]
fn big_str() {
    for &string in &[
        "0123456789abcdefg",
        "0123456789abcdefgh",
        "0123456789abcdefghi",
    ] {
        let answer = IdentStr::from(string);

        assert_eq!(answer.len(), string.len());
        assert_eq!(answer, answer);
        assert_eq!(answer.clone(), answer);
        assert_eq!(answer.clone(), answer.clone());
        assert_eq!(answer.as_str(), string);
        assert_eq!(answer.clone().as_str(), string);
    }
}

#[cfg(target_pointer_width = "64")]
#[test]
fn small_var_length() {
    for &string in &[
        "",
        "0",
        "01",
        "012",
        "0123",
        "01234",
        "012345",
        "0123456",
        "01234567",
        "012345678",
        "0123456789",
        "0123456789a",
        "0123456789ab",
        "0123456789abc",
        "0123456789abcd",
        "0123456789abcde ",
    ] {
        let answer = IdentStr::from(string);

        assert_eq!(answer.len(), string.len());
        assert_eq!(answer, answer);
        assert_eq!(answer.clone(), answer);
        assert_eq!(answer.clone(), answer.clone());
        assert_eq!(answer.as_str(), string);
        assert_eq!(answer.clone().as_str(), string);
    }
}

#[cfg(target_pointer_width = "64")]
#[test]
fn small_max_length() {
    let string = "0123456789abcdef";
    let answer = IdentStr::from(string);

    assert_eq!(answer.len(), string.len());
    assert_eq!(answer, answer);
    assert_eq!(answer.clone(), answer);
    assert_eq!(answer, answer);
    assert_eq!(answer.as_str(), string);
    assert_eq!(answer.as_str(), string);
}

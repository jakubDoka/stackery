pub use serde_base128_derive::Serde128;

mod impls;
#[cfg(test)]
mod test;

pub trait Serde128 {
    fn serialize(&self, encoder: &mut Encoder);
    unsafe fn deserialize(decoder: &mut Decoder) -> Self;
}

pub trait UseSerde128<T>: Default {
    fn serialize(self, value: &T, decoder: &mut Encoder);
    unsafe fn deserialize(self, decoder: &mut Decoder) -> T;
}

pub unsafe trait CheckType: Copy + 'static + Serde128 + Eq {}

unsafe impl CheckType for u64 {}

#[derive(Default)]
pub struct Encoder {
    data: Vec<u8>,
}

impl Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.data
    }

    pub fn save_with_check<K: CheckType, V: Serde128>(&mut self, key: K, value: &V) {
        key.serialize(self);
        value.serialize(self);
    }

    pub fn write_byte(&mut self, value: u8) {
        self.data.push(value);
    }

    pub fn write_slice(&mut self, value: &[u8]) {
        self.write_u64(value.len() as u64);
        self.data.extend_from_slice(value);
    }

    pub fn write_u64(&mut self, mut value: u64) {
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            self.write_byte(byte);
            if value == 0 {
                break;
            }
        }
    }

    pub fn write_i64(&mut self, value: i64) {
        self.write_u64(value as u64);
    }

    pub fn write_object_slice<T: Serde128>(&mut self, slice: &[T]) {
        self.write_u64(slice.len() as u64);
        for value in slice {
            value.serialize(self);
        }
    }
}



pub struct Decoder<'a> {
    data: &'a [u8],
}

impl<'a> Decoder<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data }
    }

    pub unsafe fn load_with_check<K: CheckType, V: Serde128>(&mut self, against: K) -> Option<V> {
        let key = K::deserialize(self);
        if key == against {
            Some(V::deserialize(self))
        } else {
            None
        }
    }

    pub unsafe fn read_byte(&mut self) -> u8 {
        debug_assert!(!self.data.is_empty());
        let value = *self.data.get_unchecked(0);
        self.data = self.data.get_unchecked(1..);
        value
    }

    pub unsafe fn read_slice(&mut self, len: usize) -> &'a [u8] {
        debug_assert!(self.data.len() >= len);
        let value = self.data.get_unchecked(..len);
        self.data = self.data.get_unchecked(len..);
        value
    }

    pub unsafe fn read_u64(&mut self) -> u64 {
        let mut value = 0u64;
        let mut shift = 0;
        loop {
            let byte = self.read_byte();
            value |= ((byte & 0x7f) as u64) << shift;
            if byte & 0x80 == 0 {
                break value;
            }
            shift += 7;
        }
    }

    pub unsafe fn read_i64(&mut self) -> i64 {
        self.read_u64() as i64
    }

    pub fn remining(&self) -> usize {
        self.data.len()
    }

    pub unsafe fn read_str(&mut self) -> &str {
        let len = self.read_u64() as usize;
        let value = unsafe { self.read_slice(len) };
        unsafe { std::str::from_utf8_unchecked(value) }
    }
}

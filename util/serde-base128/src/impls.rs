use std::marker::PhantomData;

use super::*;

macro_rules! impl_for_int {
    ($($int:ident)*) => {$(
        impl Serde128 for $int {
            fn serialize(&self, encoder: &mut Encoder) {
                encoder.write_u64(*self as u64);
            }

            unsafe fn deserialize(decoder: &mut Decoder) -> Self {
                decoder.read_u64() as Self
            }
        }
    )*};
}

impl_for_int!(u16 u32 u64 usize i16 i32 i64 isize);

impl Serde128 for u8 {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_byte(*self);
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        decoder.read_byte()
    }
}

impl Serde128 for bool {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_byte(*self as u8);
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        decoder.read_byte() != 0
    }
}

impl Serde128 for i8 {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_i64(*self as i64);
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        decoder.read_i64() as Self
    }
}

impl<T: Serde128> Serde128 for Option<T> {
    fn serialize(&self, encoder: &mut Encoder) {
        match self {
            Some(value) => {
                encoder.write_byte(1);
                value.serialize(encoder);
            }
            None => {
                encoder.write_byte(0);
            }
        }
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        match decoder.read_byte() {
            0 => None,
            _ => Some(T::deserialize(decoder)),
        }
    }
}

impl<T: Serde128> Serde128 for Vec<T> {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_object_slice(self);
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        let len = usize::deserialize(decoder);
        (0..len).map(|_| T::deserialize(decoder)).collect()
    }
}

impl<T: Serde128> Serde128 for Box<[T]> {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_object_slice(self);
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        let len = usize::deserialize(decoder);
        (0..len).map(|_| T::deserialize(decoder)).collect()
    }
}

impl Serde128 for String {
    fn serialize(&self, encoder: &mut Encoder) {
        encoder.write_slice(self.as_bytes());
    }

    unsafe fn deserialize(decoder: &mut Decoder) -> Self {
        String::from_utf8_unchecked(Vec::deserialize(decoder))
    }
}

impl<T> Serde128 for PhantomData<T> {
    fn serialize(&self, _encoder: &mut Encoder) {}

    unsafe fn deserialize(_decoder: &mut Decoder) -> Self {
        PhantomData
    }
}

use crate::*;

impl Sealed for u8 {}
impl Encode for u8 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        encoder.output.push(self);
    }
}

pub struct B128Iter {
    value: u64,
    init_zero: bool,
}

impl B128Iter {
    pub fn new(value: u64) -> Self {
        Self {
            value,
            init_zero: true,
        }
    }
}

impl Iterator for B128Iter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.init_zero {
            self.init_zero = false;
            return Some(0);
        }

        if self.value == 0 {
            None
        } else {
            let mut byte = (self.value & 0x7f) as u8;
            self.value >>= 7;
            if self.value != 0 {
                byte |= 0x80;
            }
            Some(byte)
        }
    }
}

impl Sealed for u64 {}
impl Encode for u64 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        for byte in B128Iter::new(self) {
            encoder.output.push(byte);
        }
    }
}

impl Sealed for i64 {}
impl Encode for i64 {
    fn encode<B: Backend>(mut self, encoder: Encoder<B>) {
        loop {
            let mut byte = (self & 0x7f) as u8;
            self >>= 7;
            if (self == 0 && (byte & 0x40) == 0) || (self == -1 && (byte & 0x40) != 0) {
                encoder.output.push(byte);
                break;
            } else {
                byte |= 0x80;
                encoder.output.push(byte);
            }
        }
    }
}

impl Sealed for u32 {}
impl Encode for u32 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        (self as u64).encode(encoder)
    }
}

impl Sealed for i32 {}
impl Encode for i32 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        (self as i64).encode(encoder)
    }
}

impl Sealed for f32 {}
impl Encode for f32 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        encoder.output.extend(&self.to_le_bytes());
    }
}

impl Sealed for f64 {}
impl Encode for f64 {
    fn encode<B: Backend>(self, encoder: Encoder<B>) {
        encoder.output.extend(&self.to_le_bytes());
    }
}

impl Sealed for &str {}
impl Encode for &str {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.len() as u64);
        encoder.output.extend(self.as_bytes());
    }
}

pub fn unsigned_integer_length(value: u64) -> usize {
    B128Iter::new(value).count()
}

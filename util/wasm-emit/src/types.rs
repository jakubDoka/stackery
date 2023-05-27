use crate::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Valtype {
    Num(Numtype),
    Ref(Reftype),
    Vec(Vectype),
}

impl Sealed for Valtype {}
impl Encode for Valtype {
    fn encode<B: Backend>(self, backend: Encoder<B>) {
        match self {
            Valtype::Num(numtype) => numtype.encode(backend),
            Valtype::Ref(reftype) => reftype.encode(backend),
            Valtype::Vec(vectype) => vectype.encode(backend),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Numtype {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

impl Sealed for Numtype {}
impl Encode for Numtype {
    fn encode<B: Backend>(self, mut backend: Encoder<B>) {
        backend.byte(self as u8);
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Reftype {
    FuncRef = 0x70,
    ExternRef = 0x6f,
}

impl Sealed for Reftype {}
impl Encode for Reftype {
    fn encode<B: Backend>(self, mut backend: Encoder<B>) {
        backend.byte(self as u8);
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Vectype {
    V128 = 0x7b,
}

impl Sealed for Vectype {}
impl Encode for Vectype {
    fn encode<B: Backend>(self, mut backend: Encoder<B>) {
        backend.byte(self as u8);
    }
}

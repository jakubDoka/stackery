use alloc::vec::Vec;

use crate::{module::EmmitBuffer, Buffer, Integer, Integers};

impl Buffer for Vec<u8> {
    fn extend(&mut self, slice: &[u8]) {
        self.extend_from_slice(slice)
    }

    fn push(&mut self, byte: u8) {
        self.push(byte)
    }

    fn data(&self) -> &[u8] {
        self.as_slice()
    }
}

impl Integers for Vec<Integer> {
    fn push(&mut self, offset: usize) -> crate::IntegerId {
        let id = self.len() as crate::IntegerId;
        self.push(Integer { offset, value: 0 });
        id
    }

    fn update(&mut self, id: crate::IntegerId, value: usize) {
        self[id].value = value as u64;
    }

    fn offset(&self, id: crate::IntegerId) -> usize {
        self[id].offset
    }

    fn data(&self) -> &[crate::Integer] {
        self.as_slice()
    }
}

impl EmmitBuffer for Vec<u8> {
    fn ensure_capacity(&mut self, amount: usize) -> *mut u8 {
        self.reserve(amount);
        unsafe { self.as_mut_ptr().add(self.len()) }
    }

    unsafe fn set_len(&mut self, len: usize) {
        self.set_len(len)
    }
}

pub struct DefaultBackend;

impl crate::Backend for DefaultBackend {
    type Integers = Vec<Integer>;
    type Buffer = Vec<u8>;
}

//#![no_std]

use core::{
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr,
    slice::SliceIndex,
};

#[cfg(any(feature = "alloc", test))]
extern crate alloc;

pub mod b128;

#[cfg(any(feature = "alloc", test))]
mod alloc_stuff;
mod expr;
mod module;
mod sections;
#[cfg(test)]
mod tests;
mod types;
mod vec;

pub use {
    expr::{ExprEncoder, IfExprEncoder, Laneidx, Memarg, MultiByteInstr, OneByteInstr},
    module::{
        CodeSection, DataSection, Dataidx, ElemSection, Elemidx, ExportSection, Funcidx,
        FunctionSection, GlobalSection, Globalidx, ImportSection, Labelidx, Localidx, Memidx,
        MemorySection, Module, TableSection, Tableidx, TypeSection, Typeidx,
    },
    sections::{
        ActiveDataEncoder, BlockType, CodeBodyEncoder, CodeEncoder, CodeLocalsEncoder,
        CustomSection, DataEncoder, ElemEncoder, ElemKind, Export, ExportDesc, ExprsElemEncoder,
        FuncEncoder, FuncsElemEncoder, GlobalEncoder, Globaltype, Import, ImportDesc, ImportItem,
        ImportSectionEncoder, IndexSpace, IndexSpaceOffsets, MemArg, Memtype, SectionId, Tabletype,
        VecSectionEncoder,
    },
    types::{Numtype, Reftype, Valtype, Vectype},
    vec::VecEncoder,
};

#[cfg(any(feature = "alloc", test))]
pub use alloc_stuff::DefaultBackend;

pub trait Encode: Copy + Sealed {
    fn encode<B: Backend>(self, encoder: Encoder<B>);
}

#[derive(Copy, Clone, Debug)]
pub struct Integer {
    pub offset: usize,
    pub value: u64,
}

pub type IntegerId = usize;

pub trait Integers {
    fn push(&mut self, offset: usize) -> IntegerId;
    fn update(&mut self, id: IntegerId, value: usize);
    fn offset(&self, id: IntegerId) -> usize;
    fn data(&self) -> &[Integer];

    fn update_byte_range(&mut self, id: IntegerId, current_len: usize) {
        let offset = self.offset(id);
        let delta = current_len - offset;
        let integer_expansion = self.bytes_taken(id + 1..);

        self.update(id, integer_expansion + delta);
    }

    fn bytes_taken(&self, range: impl SliceIndex<[Integer], Output = [Integer]>) -> usize {
        let relevant = &self.data()[range];
        relevant
            .iter()
            .map(|i| b128::unsigned_integer_length(i.value))
            .sum::<usize>()
    }
}

pub trait Buffer {
    fn extend(&mut self, slice: &[u8]);
    fn push(&mut self, byte: u8);
    fn data(&self) -> &[u8];

    fn len(&self) -> usize {
        self.data().len()
    }
}

pub trait Backend {
    type Integers: Integers + 'static;
    type Buffer: Buffer + 'static;
}

pub trait VecSection: Sealed {
    const ID: SectionId;
    type Index: Index;
    type Element<'a, B: Backend>;

    fn new(start: usize, end: usize, entity_count: usize, _: Guard) -> Self;

    fn start(&self) -> usize;
    fn end(&self) -> usize;
}

pub trait Index: Sealed + Copy {
    fn new(idx: usize) -> Self;
}

pub trait Sealed {}

pub trait NestedEncode: VecSection {
    fn new_encoder<B: Backend>(encoder: Encoder<'_, B>, _: Guard) -> Self::Element<'_, B>;
}

pub struct Guard(());

pub struct Encoder<'a, B: Backend> {
    integers: &'a mut B::Integers,
    output: &'a mut B::Buffer,
}

impl<B: Backend> Drop for Encoder<'_, B> {
    fn drop(&mut self) {}
}

impl<'a, B: Backend> Encoder<'a, B> {
    pub fn new(integers: &'a mut B::Integers, output: &'a mut B::Buffer) -> Self {
        Self { integers, output }
    }

    pub fn types(&mut self) -> VecSectionEncoder<TypeSection, B> {
        VecSectionEncoder::new(self.stack_borrow())
    }

    pub fn imports(&mut self) -> ImportSectionEncoder<B> {
        ImportSectionEncoder::new(self.stack_borrow())
    }

    pub fn functions(&mut self, iso: IndexSpaceOffsets) -> VecSectionEncoder<FunctionSection, B> {
        VecSectionEncoder::with_index_offset::<Typeidx>(self.stack_borrow(), iso)
    }

    pub fn tables(&mut self, iso: IndexSpaceOffsets) -> VecSectionEncoder<TableSection, B> {
        VecSectionEncoder::with_index_offset::<Tabletype>(self.stack_borrow(), iso)
    }

    pub fn memories(&mut self, iso: IndexSpaceOffsets) -> VecSectionEncoder<MemorySection, B> {
        VecSectionEncoder::with_index_offset::<Memtype>(self.stack_borrow(), iso)
    }

    pub fn globals(&mut self, iso: IndexSpaceOffsets) -> VecSectionEncoder<GlobalSection, B> {
        VecSectionEncoder::with_index_offset::<Globaltype>(self.stack_borrow(), iso)
    }

    pub fn exports(&mut self) -> VecSectionEncoder<ExportSection, B> {
        VecSectionEncoder::new(self.stack_borrow())
    }

    pub fn elements(&mut self) -> VecSectionEncoder<ElemSection, B> {
        VecSectionEncoder::new(self.stack_borrow())
    }

    pub fn codes(&mut self) -> VecSectionEncoder<CodeSection, B> {
        VecSectionEncoder::new(self.stack_borrow())
    }

    pub fn data(&mut self) -> VecSectionEncoder<DataSection, B> {
        VecSectionEncoder::new(self.stack_borrow())
    }

    fn stack_borrow(&mut self) -> Encoder<'_, B> {
        Encoder {
            integers: self.integers,
            output: self.output,
        }
    }

    fn encode<T: Encode>(&mut self, value: T) {
        value.encode(self.stack_borrow());
    }

    fn register_integer(&mut self) -> IntegerId {
        self.integers.push(self.output.len())
    }

    fn byte(&mut self, value: u8) {
        self.output.push(value);
    }

    fn update_byte_range(&mut self, size: IntegerId) {
        self.integers.update_byte_range(size, self.output.len());
    }
}

fn take_field_and_leak<T, F>(value: T, addr_fn: impl FnOnce(&T) -> *const F) -> F {
    let addr = addr_fn(&value);
    let uninit = unsafe { ptr::read(addr as *const MaybeUninit<F>) };
    mem::forget(value);
    unsafe { uninit.assume_init() }
}

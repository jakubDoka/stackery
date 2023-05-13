use core::{mem::ManuallyDrop, num::NonZeroU32};

use crate::{module::DataSection, *};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum SectionId {
    Custom,
    Type,
    Import,
    Func,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
    DataCount,
}

pub struct IndexSpaceOffset(usize);

pub struct VecSectionEncoder<'a, T, B: Backend> {
    size: IntegerId,
    inner: VecEncoder<'a, T, B>,
    index_offset: usize,
}

impl<'a, T, B: Backend> VecSectionEncoder<'a, T, B> {
    pub fn new(
        mut encoder: Encoder<'a, B>,
        IndexSpaceOffset(index_offset): IndexSpaceOffset,
    ) -> Self
    where
        T: VecSection,
    {
        encoder.byte(T::ID as u8);
        Self {
            size: encoder.register_integer(),
            inner: VecEncoder::new(encoder),
            index_offset,
        }
    }

    pub fn push(&mut self, value: T) -> T::Index
    where
        T: VecSection + Encode,
    {
        let index = T::Index::new(self.inner.written() + self.index_offset);
        self.inner.push(value);
        index
    }

    pub fn finish(mut self) -> T::Range
    where
        T: VecSection,
    {
        let start = self.inner.inner().integers.offset(self.size) - 1;
        let current_len = self.inner.inner().output.len();
        self.inner.inner().update_byte_range(self.size);
        T::Range::new(start, current_len, self.inner.written(), Guard(()))
    }

    pub fn encoder(&'a mut self) -> (T::Index, T)
    where
        T: NestedEncode<'a, B> + VecSection,
    {
        (
            T::Index::new(self.inner.written() + self.index_offset),
            self.inner.encoder(),
        )
    }
}

pub struct DataEncoder<'a, B: Backend> {
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> DataEncoder<'a, B> {
    pub fn active(mut self, mem: Option<Memidx>) -> ActiveDataEncoder<'a, B> {
        match mem {
            Some(Memidx::ZERO) | None => {
                self.inner.byte(0x00);
            }
            Some(m) => {
                self.inner.byte(0x02);
                self.inner.encode(m);
            }
        }

        ActiveDataEncoder {
            inner: ManuallyDrop::new(ExprEncoder::new(take_field_and_leak(self, |s| {
                ptr::addr_of!(s.inner)
            }))),
        }
    }

    pub fn passive(mut self, init: &[u8]) {
        self.inner.encode(init.len() as u32);
        self.inner.output.extend(init);
        mem::forget(self);
    }
}

impl<'a, B: Backend> NestedEncode<'a, B> for DataEncoder<'a, B> {
    fn new(encoder: Encoder<'a, B>, _: Guard) -> Self {
        Self { inner: encoder }
    }
}

impl<'a, B: Backend> Drop for DataEncoder<'a, B> {
    fn drop(&mut self) {
        self.inner.byte(0x00); // default variant
        self.inner.byte(0x0b); // empty expr
        self.inner.byte(0x00); // empty vec
    }
}

impl<B: Backend> Sealed for DataEncoder<'_, B> {}
impl<B: Backend> VecSection for DataEncoder<'_, B> {
    const ID: SectionId = SectionId::Data;
    type Index = Dataidx;
    type Range = DataSection;
}

pub struct ActiveDataEncoder<'a, B: Backend> {
    inner: ManuallyDrop<ExprEncoder<'a, B>>,
}

impl<'a, B: Backend> ActiveDataEncoder<'a, B> {
    pub fn finish(mut self, init: &[u8]) {
        self.inner.inner().encode(init.len() as u32);
        self.inner.inner().output.extend(init);
    }
}

impl<'a, B: Backend> Deref for ActiveDataEncoder<'a, B> {
    type Target = ExprEncoder<'a, B>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for ActiveDataEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a, B: Backend> Drop for ActiveDataEncoder<'a, B> {
    fn drop(&mut self) {
        self.inner.inner().byte(0x0b); // end
        self.inner.inner().byte(0x00); // empty vec
    }
}

pub struct CodeEncoder<'a, B: Backend> {
    size: IntegerId,
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> CodeEncoder<'a, B> {
    pub fn locals(mut self) -> CodeLocalsEncoder<'a, B> {
        CodeLocalsEncoder {
            size: self.inner.register_integer(),
            current: None,
            inner: VecEncoder::new(take_field_and_leak(self, |s| ptr::addr_of!(s.inner))),
        }
    }
}

impl<'a, B: Backend> NestedEncode<'a, B> for CodeEncoder<'a, B> {
    fn new(mut encoder: Encoder<'a, B>, _: Guard) -> Self {
        Self {
            size: encoder.register_integer(),
            inner: encoder,
        }
    }
}

impl<'a, B: Backend> Drop for CodeEncoder<'a, B> {
    fn drop(&mut self) {
        let len = 2; // len for param vec and expr terminator
        self.inner.integers.update(self.size, len);
        self.inner.byte(ExprEncoder::<B>::TERMINATOR);
    }
}

impl<B: Backend> Sealed for CodeEncoder<'_, B> {}
impl<B: Backend> VecSection for CodeEncoder<'_, B> {
    const ID: SectionId = SectionId::Code;
    type Index = ();
    type Range = CodeSection;
}

pub struct CodeLocalsEncoder<'a, B: Backend> {
    size: IntegerId,
    current: Option<Locals>,
    inner: VecEncoder<'a, Locals, B>,
}

impl<'a, B: Backend> CodeLocalsEncoder<'a, B> {
    pub fn extend(&mut self, local: Locals) {
        let Some(ref mut current) = self.current else {
            self.current = Some(local);
            return;
        };

        if current.ty == local.ty {
            current.count = current.count.saturating_add(local.count.get());
            return;
        }

        let prev = mem::replace(current, local);

        self.inner.push(prev);
    }

    pub fn push(&mut self, ty: Valtype) {
        self.extend(Locals::new(ty));
    }

    fn flush_last(&mut self) {
        if let Some(current) = self.current.take() {
            self.inner.push(current);
        }
    }

    fn write_len(&mut self) {
        self.flush_last();
        self.inner.inner().byte(ExprEncoder::<B>::TERMINATOR);
        self.inner.inner().update_byte_range(self.size);
    }

    pub fn body(self) -> CodeBodyEncoder<'a, B> {
        let Self { size, .. } = self;
        let inner = take_field_and_leak(self, |s| ptr::addr_of!(s.inner));
        let encoder = inner.finish();
        CodeBodyEncoder {
            size,
            inner: ExprEncoder::new(encoder),
        }
    }
}

impl<B: Backend> Drop for CodeLocalsEncoder<'_, B> {
    fn drop(&mut self) {
        if let Some(current) = self.current.take() {
            self.inner.push(current);
        }

        self.write_len();
    }
}

pub struct CodeBodyEncoder<'a, B: Backend> {
    size: IntegerId,
    inner: ExprEncoder<'a, B>,
}

impl<'a, B: Backend> Deref for CodeBodyEncoder<'a, B> {
    type Target = ExprEncoder<'a, B>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for CodeBodyEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<B: Backend> Drop for CodeBodyEncoder<'_, B> {
    fn drop(&mut self) {
        self.inner.inner().update_byte_range(self.size);
    }
}

pub struct GlobalEncoder<'a, B: Backend> {
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> GlobalEncoder<'a, B> {
    fn new(encoder: Encoder<'a, B>) -> Self {
        Self { inner: encoder }
    }

    pub fn r#type(mut self, ty: Globaltype) -> ExprEncoder<'a, B> {
        self.inner.encode(ty);
        ExprEncoder::new(take_field_and_leak(self, |s| ptr::addr_of!(s.inner)))
    }
}

impl<B: Backend> Sealed for GlobalEncoder<'_, B> {}

impl<B: Backend> VecSection for GlobalEncoder<'_, B> {
    const ID: SectionId = SectionId::Global;
    type Index = Globalidx;
    type Range = GlobalSection;
}

impl<'a, B: Backend> NestedEncode<'a, B> for GlobalEncoder<'a, B> {
    fn new(encoder: Encoder<'a, B>, _: Guard) -> Self {
        Self::new(encoder)
    }
}

pub struct FuncEncoder<'a, B: Backend> {
    encoder: VecEncoder<'a, Valtype, B>,
}

impl<'a, B: Backend> FuncEncoder<'a, B> {
    pub fn returns(mut self) -> VecEncoder<'a, Valtype, B> {
        self.encoder.flush();
        take_field_and_leak(self, |s| ptr::addr_of!(s.encoder))
    }
}

impl<'a, B: Backend> NestedEncode<'a, B> for FuncEncoder<'a, B> {
    fn new(mut encoder: Encoder<'a, B>, _: Guard) -> Self {
        encoder.byte(0x60);
        Self {
            encoder: VecEncoder::new(encoder),
        }
    }
}

impl<B: Backend> Sealed for FuncEncoder<'_, B> {}
impl<B: Backend> VecSection for FuncEncoder<'_, B> {
    const ID: SectionId = SectionId::Type;
    type Index = Typeidx;
    type Range = TypeSection;
}

impl<'a, B: Backend> Deref for FuncEncoder<'a, B> {
    type Target = VecEncoder<'a, Valtype, B>;

    fn deref(&self) -> &Self::Target {
        &self.encoder
    }
}

impl<'a, B: Backend> DerefMut for FuncEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.encoder
    }
}

impl<B: Backend> Drop for FuncEncoder<'_, B> {
    fn drop(&mut self) {
        self.encoder.flush();
    }
}

pub type Memtype = Limits;

impl VecSection for Memtype {
    const ID: SectionId = SectionId::Memory;
    type Index = Memidx;
    type Range = MemorySection;
}

#[derive(Clone, Copy)]
pub enum Limits {
    Unbounded { min: u32 },
    Bounded { min: u32, max: u32 },
}

impl Sealed for Limits {}
impl Encode for Limits {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        match self {
            Limits::Unbounded { min } => {
                encoder.byte(0x00);
                encoder.encode(min);
            }
            Limits::Bounded { min, max } => {
                encoder.byte(0x01);
                encoder.encode(min);
                encoder.encode(max);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct Tabletype {
    pub elem_type: Reftype,
    pub limits: Limits,
}

impl Sealed for Tabletype {}
impl Encode for Tabletype {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.elem_type);
        self.limits.encode(encoder);
    }
}

impl VecSection for Tabletype {
    const ID: SectionId = SectionId::Table;
    type Index = Tableidx;
    type Range = TableSection;
}

#[derive(Clone, Copy)]
pub struct Globaltype {
    pub content_type: Valtype,
    pub mutable: bool,
}

impl Sealed for Globaltype {}
impl Encode for Globaltype {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.content_type);
        encoder.byte(if self.mutable { 0x01 } else { 0x00 });
    }
}

impl VecSection for Typeidx {
    const ID: SectionId = SectionId::Func;
    type Index = Funcidx;
    type Range = TypeSection;
}

#[derive(Clone, Copy)]
pub struct Locals {
    pub(super) count: NonZeroU32,
    pub(super) ty: Valtype,
}

impl Locals {
    pub fn new(ty: Valtype) -> Self {
        Self {
            count: NonZeroU32::MIN,
            ty,
        }
    }
}

impl Sealed for Locals {}
impl Encode for Locals {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.count.get());
        encoder.encode(self.ty);
    }
}

#[derive(Clone, Copy)]
pub struct Import<'a> {
    pub module: &'a str,
    pub name: &'a str,
    pub desc: ImportDesc,
}

impl Encode for Import<'_> {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.module);
        encoder.encode(self.name);
        self.desc.encode(encoder);
    }
}

impl Sealed for Import<'_> {}
impl VecSection for Import<'_> {
    const ID: SectionId = SectionId::Import;
    type Index = ();
    type Range = ImportSection;
}

#[derive(Clone, Copy)]
pub struct Export<'a> {
    pub name: &'a str,
    pub desc: ExportDesc,
}

impl Sealed for Export<'_> {}
impl Encode for Export<'_> {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.name);
        self.desc.encode(encoder);
    }
}

impl VecSection for Export<'_> {
    const ID: SectionId = SectionId::Export;
    type Index = ();
    type Range = ExportSection;
}

impl Sealed for () {}
impl Index for () {
    fn new(_: usize) -> Self {}
}

#[derive(Clone, Copy)]
pub enum ExportDesc {
    Func(Funcidx),
    Table(Tableidx),
    Mem(Memidx),
    Global(Globalidx),
}

impl Sealed for ExportDesc {}
impl Encode for ExportDesc {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        match self {
            ExportDesc::Func(idx) => {
                encoder.byte(0x00);
                idx.encode(encoder);
            }
            ExportDesc::Table(idx) => {
                encoder.byte(0x01);
                idx.encode(encoder);
            }
            ExportDesc::Mem(idx) => {
                encoder.byte(0x02);
                idx.encode(encoder);
            }
            ExportDesc::Global(idx) => {
                encoder.byte(0x03);
                idx.encode(encoder);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum ImportDesc {
    Type(Typeidx),
    Table(Tabletype),
    Mem(Memtype),
    Global(Globaltype),
}

impl Sealed for ImportDesc {}
impl Encode for ImportDesc {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        match self {
            ImportDesc::Type(idx) => {
                encoder.byte(0x00);
                idx.encode(encoder);
            }
            ImportDesc::Table(ty) => {
                encoder.byte(0x01);
                ty.encode(encoder);
            }
            ImportDesc::Mem(ty) => {
                encoder.byte(0x02);
                ty.encode(encoder);
            }
            ImportDesc::Global(ty) => {
                encoder.byte(0x03);
                ty.encode(encoder);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct CustomSection<'a> {
    pub name: &'a str,
    pub bytes: &'a [u8],
}
impl<'a> CustomSection<'a> {
    pub(crate) fn len(&self) -> usize {
        let inner_len = self.inner_len();
        inner_len + b128::unsigned_integer_length(inner_len as u64) + 1
    }

    pub(crate) fn inner_len(&self) -> usize {
        self.bytes.len()
            + self.name.len()
            + b128::unsigned_integer_length(self.name.len() as u64)
            + b128::unsigned_integer_length(self.bytes.len() as u64)
    }
}

impl Sealed for CustomSection<'_> {}
impl Encode for CustomSection<'_> {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.byte(SectionId::Custom as u8);
        let len = self.inner_len() as u32;
        encoder.encode(len);
        encoder.encode(self.name);
        encoder.output.extend(self.bytes);
    }
}

#[derive(Clone, Copy)]
pub enum ElemKind {
    FuncRef,
}

impl Sealed for ElemKind {}
impl Encode for ElemKind {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        match self {
            ElemKind::FuncRef => encoder.byte(0x0),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Start {
    pub func: Funcidx,
}

impl Start {
    pub(crate) fn len(&self) -> usize {
        self.func.len() + 1 + 1
    }
}

#[derive(Clone, Copy)]
pub enum BlockType {
    Empty,
    Val(Valtype),
    Scalar(u64),
}

impl Sealed for BlockType {}
impl Encode for BlockType {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        match self {
            BlockType::Empty => encoder.byte(0x40),
            BlockType::Val(ty) => ty.encode(encoder),
            BlockType::Scalar(n) => {
                encoder.encode(n);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct MemArg {
    pub align: u32,
    pub offset: u32,
}

impl Sealed for MemArg {}
impl Encode for MemArg {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.align);
        encoder.encode(self.offset);
    }
}

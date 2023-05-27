use core::{mem::ManuallyDrop, num::NonZeroU32};

use crate::*;

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
    Elem,
    Code,
    Data,
    DataCount,
}

#[derive(Clone, Copy)]
pub struct IndexSpaceOffsets {
    functions: usize,
    tables: usize,
    memories: usize,
    globals: usize,
}

impl IndexSpaceOffsets {
    pub(super) fn new() -> Self {
        Self {
            functions: 0,
            tables: 0,
            memories: 0,
            globals: 0,
        }
    }
}

pub trait IndexSpace: VecSection {}
impl IndexSpace for FunctionSection {}
impl IndexSpace for TableSection {}
impl IndexSpace for MemorySection {}
impl IndexSpace for GlobalSection {}

pub trait ImportItem: Into<ImportDesc> {
    type Index: Index;
    fn counter(offsets: &mut IndexSpaceOffsets, _: Guard) -> &mut usize;
}

impl ImportItem for Typeidx {
    type Index = Funcidx;
    fn counter(offsets: &mut IndexSpaceOffsets, _: Guard) -> &mut usize {
        &mut offsets.functions
    }
}

impl ImportItem for Tabletype {
    type Index = Tableidx;
    fn counter(offsets: &mut IndexSpaceOffsets, _: Guard) -> &mut usize {
        &mut offsets.tables
    }
}

impl ImportItem for Memtype {
    type Index = Memidx;
    fn counter(offsets: &mut IndexSpaceOffsets, _: Guard) -> &mut usize {
        &mut offsets.memories
    }
}

impl ImportItem for Globaltype {
    type Index = Globalidx;
    fn counter(offsets: &mut IndexSpaceOffsets, _: Guard) -> &mut usize {
        &mut offsets.globals
    }
}

pub struct VecSectionEncoder<'a, 'b, T: VecSection, B: Backend> {
    size: IntegerId,
    pub(super) inner: VecEncoder<'a, T::Element<'b, B>, B>,
    index_offset: usize,
}

impl<'a, 'b, T: VecSection, B: Backend> VecSectionEncoder<'a, 'b, T, B> {
    pub(super) fn new(mut encoder: Encoder<'a, B>) -> Self {
        encoder.byte(T::ID as u8);
        Self {
            size: encoder.register_integer(),
            inner: VecEncoder::new(encoder),
            index_offset: 0,
        }
    }

    pub(super) fn with_index_offset<I: ImportItem>(
        mut encoder: Encoder<'a, B>,
        mut offsets: IndexSpaceOffsets,
    ) -> Self
    where
        T: IndexSpace,
    {
        encoder.byte(T::ID as u8);
        Self {
            size: encoder.register_integer(),
            inner: VecEncoder::new(encoder),
            index_offset: *I::counter(&mut offsets, Guard(())),
        }
    }

    pub fn push(&mut self, value: T::Element<'b, B>) -> T::Index
    where
        T::Element<'b, B>: Encode,
    {
        let index = T::Index::new(self.inner.written() + self.index_offset);
        self.inner.push(value);
        index
    }

    pub fn finish(mut self) -> T {
        let start = self.inner.inner().integers.offset(self.size) - 1;
        let current_len = self.inner.inner().output.len();
        self.inner.inner().update_byte_range(self.size);
        T::new(start, current_len, self.inner.written(), Guard(()))
    }

    pub fn encoder(&mut self) -> (T::Index, T::Element<'_, B>)
    where
        T: NestedEncode,
    {
        (T::Index::new(self.inner.written() + self.index_offset), {
            self.inner.inc_written();
            T::new_encoder(self.inner.inner(), Guard(()))
        })
    }
}

pub struct ImportSectionEncoder<'a, 'b, B: Backend> {
    inner: VecSectionEncoder<'a, 'b, ImportSection, B>,
    index_offsets: IndexSpaceOffsets,
}

impl<'a, 'b, B: Backend> ImportSectionEncoder<'a, 'b, B> {
    pub(super) fn new(encoder: Encoder<'a, B>) -> Self {
        Self {
            inner: VecSectionEncoder::new(encoder),
            index_offsets: IndexSpaceOffsets::new(),
        }
    }

    pub fn import<I: ImportItem>(&mut self, name: &'b str, module: &'b str, item: I) -> I::Index {
        let func = Import {
            module,
            name,
            desc: item.into(),
        };

        self.inner.push(func);

        let counter = I::counter(&mut self.index_offsets, Guard(()));
        *counter += 1;

        I::Index::new(*counter - 1)
    }

    pub fn finish(self) -> (ImportSection, IndexSpaceOffsets) {
        (self.inner.finish(), self.index_offsets)
    }
}

pub struct DataEncoder<'a, B: Backend> {
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> DataEncoder<'a, B> {
    pub(super) fn new(encoder: Encoder<'a, B>) -> Self {
        Self { inner: encoder }
    }

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

impl<'a, B: Backend> Drop for DataEncoder<'a, B> {
    fn drop(&mut self) {
        self.inner.byte(0x00); // default variant
        self.inner.byte(0x0b); // empty expr
        self.inner.byte(0x00); // empty vec
    }
}

impl<B: Backend> Sealed for DataEncoder<'_, B> {}

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
    pub(super) fn new(mut encoder: Encoder<'a, B>) -> Self {
        Self {
            size: encoder.register_integer(),
            inner: encoder,
        }
    }

    pub fn locals(mut self) -> CodeLocalsEncoder<'a, B> {
        CodeLocalsEncoder {
            size: self.inner.register_integer(),
            current: None,
            inner: VecEncoder::new(take_field_and_leak(self, |s| ptr::addr_of!(s.inner))),
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
    pub(super) fn new(encoder: Encoder<'a, B>) -> Self {
        Self { inner: encoder }
    }

    pub fn r#type(mut self, ty: Globaltype) -> ExprEncoder<'a, B> {
        self.inner.encode(ty);
        ExprEncoder::new(take_field_and_leak(self, |s| ptr::addr_of!(s.inner)))
    }
}

impl<B: Backend> Sealed for GlobalEncoder<'_, B> {}

pub struct FuncEncoder<'a, B: Backend> {
    encoder: VecEncoder<'a, Valtype, B>,
}

impl<'a, B: Backend> FuncEncoder<'a, B> {
    pub(super) fn new(mut encoder: Encoder<'a, B>) -> Self {
        encoder.byte(0x60);
        Self {
            encoder: VecEncoder::new(encoder),
        }
    }

    pub fn returns(mut self) -> VecEncoder<'a, Valtype, B> {
        self.encoder.flush();
        take_field_and_leak(self, |s| ptr::addr_of!(s.encoder))
    }
}

impl<B: Backend> Sealed for FuncEncoder<'_, B> {}

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

impl From<Memtype> for ImportDesc {
    fn from(ty: Memtype) -> Self {
        Self::Mem(ty)
    }
}

impl From<Globaltype> for ImportDesc {
    fn from(ty: Globaltype) -> Self {
        Self::Global(ty)
    }
}

impl From<Tabletype> for ImportDesc {
    fn from(ty: Tabletype) -> Self {
        Self::Table(ty)
    }
}

impl From<Typeidx> for ImportDesc {
    fn from(idx: Typeidx) -> Self {
        Self::Type(idx)
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
        1 + b128::unsigned_integer_length(inner_len as u64) + inner_len
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
#[repr(u8)]
pub enum ElemKind {
    FuncRef = 0x0,
}

impl Sealed for ElemKind {}
impl Encode for ElemKind {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.byte(self as u8);
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

#[cfg(test)]
mod tests {
    #[test]
    fn test_custom_section_len() {
        use super::CustomSection;
        let section = CustomSection {
            name: "test",
            bytes: &[0x01, 0x02, 0x03],
        };
        assert_eq!(section.len(), 1 + 1 + 1 + 4 + 1 + 3);
    }
}

pub struct ElemEncoder<'a, B: Backend> {
    id: IntegerId,
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> ElemEncoder<'a, B> {
    const PASSIVE_RESULT_ID: u8 = 0b01000100;
    const DECLARATIVE_RESULT_ID: u8 = 0b00010001;
    pub fn new(mut inner: Encoder<'a, B>) -> Self {
        ElemEncoder {
            id: inner.register_integer(),
            inner,
        }
    }

    fn as_parts(self) -> (IntegerId, Encoder<'a, B>) {
        let id = self.id;
        let inner = take_field_and_leak(self, |s| ptr::addr_of!(s.inner));
        (id, inner)
    }

    pub fn active(self, table: Tableidx) -> ActiveElemEncoder<'a, B> {
        let (id, inner) = self.as_parts();
        ActiveElemEncoder::new(inner, table, id)
    }

    pub fn passive_funcs(self, kind: ElemKind) -> FuncsElemEncoder<'a, B> {
        let (id, inner) = self.as_parts();
        FuncsElemEncoder::new(inner, kind, id, Self::PASSIVE_RESULT_ID)
    }

    pub fn declarative_funcs(self, kind: ElemKind) -> FuncsElemEncoder<'a, B> {
        let (id, inner) = self.as_parts();
        FuncsElemEncoder::new(inner, kind, id, Self::DECLARATIVE_RESULT_ID)
    }

    pub fn passive_exprs(self, kind: Reftype) -> ExprsElemEncoder<'a, B> {
        let (id, inner) = self.as_parts();
        ExprsElemEncoder::new(inner, kind, id, Self::PASSIVE_RESULT_ID)
    }

    pub fn declarative_exprs(self, kind: Reftype) -> ExprsElemEncoder<'a, B> {
        let (id, inner) = self.as_parts();
        ExprsElemEncoder::new(inner, kind, id, Self::DECLARATIVE_RESULT_ID)
    }
}

impl<'a, B: Backend> Drop for ElemEncoder<'a, B> {
    fn drop(&mut self) {
        self.inner.integers.update(self.id, 1);
        self.inner.encode(ElemKind::FuncRef);
        self.inner.byte(0); // func count
    }
}

pub struct ActiveElemEncoder<'a, B: Backend> {
    id: IntegerId,
    result_id: u8,
    inner: ManuallyDrop<ExprEncoder<'a, B>>,
}

impl<'a, B: Backend> ActiveElemEncoder<'a, B> {
    const ZERO_TABLE_RESULT_ID: u8 = 0b10001000;
    const NON_ZERO_TABLE_RESULT_ID: u8 = 0b00100010;

    fn new(mut inner: Encoder<'a, B>, table: Tableidx, id: IntegerId) -> Self {
        if table != Tableidx::ZERO {
            inner.encode(table);
        }
        ActiveElemEncoder {
            id,
            result_id: if table == Tableidx::ZERO {
                Self::ZERO_TABLE_RESULT_ID
            } else {
                Self::NON_ZERO_TABLE_RESULT_ID
            },
            inner: ManuallyDrop::new(ExprEncoder::new(inner)),
        }
    }

    pub fn funcs(self, kind: ElemKind) -> FuncsElemEncoder<'a, B> {
        let Self { id, result_id, .. } = self;
        let inner = take_field_and_leak(self, |s| ptr::addr_of!(s.inner));
        let inner = ManuallyDrop::into_inner(inner).finish();
        FuncsElemEncoder::new(inner, kind, id, result_id)
    }

    pub fn exprs(self, kind: Reftype) -> ExprsElemEncoder<'a, B> {
        let Self { id, result_id, .. } = self;
        let inner = take_field_and_leak(self, |s| ptr::addr_of!(s.inner));
        let inner = ManuallyDrop::into_inner(inner).finish();
        ExprsElemEncoder::new(inner, kind, id, result_id)
    }
}

impl<'a, B: Backend> Deref for ActiveElemEncoder<'a, B> {
    type Target = ExprEncoder<'a, B>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for ActiveElemEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a, B: Backend> Drop for ActiveElemEncoder<'a, B> {
    fn drop(&mut self) {
        let mut inner = unsafe { ManuallyDrop::take(&mut self.inner).finish() };
        if self.result_id == Self::ZERO_TABLE_RESULT_ID {
            inner.integers.update(self.id, 0);
            inner.byte(0); // func count
        } else {
            inner.integers.update(self.id, 2);
            inner.encode(ElemKind::FuncRef);
            inner.byte(0); // func count
        }
    }
}

pub struct FuncsElemEncoder<'a, B: Backend> {
    id: IntegerId,
    result_id: u8,
    inner: VecEncoder<'a, Funcidx, B>,
}

impl<'a, B: Backend> FuncsElemEncoder<'a, B> {
    fn new(mut inner: Encoder<'a, B>, kind: ElemKind, id: IntegerId, result_id: u8) -> Self {
        if result_id != ActiveElemEncoder::<B>::ZERO_TABLE_RESULT_ID {
            inner.encode(kind);
        }

        FuncsElemEncoder {
            id,
            result_id: result_id & 0b11110000,
            inner: VecEncoder::new(inner),
        }
    }
}

impl<'a, B: Backend> Deref for FuncsElemEncoder<'a, B> {
    type Target = VecEncoder<'a, Funcidx, B>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for FuncsElemEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a, B: Backend> Drop for FuncsElemEncoder<'a, B> {
    fn drop(&mut self) {
        debug_assert!(self.result_id.is_power_of_two());
        self.inner
            .inner()
            .integers
            .update(self.id, self.result_id.leading_zeros() as usize);
    }
}

pub struct ExprsElemEncoder<'a, B: Backend> {
    id: IntegerId,
    result_id: u8,
    inner: VecEncoder<'a, ExprEncoder<'a, B>, B>,
}

impl<'a, B: Backend> ExprsElemEncoder<'a, B> {
    fn new(mut inner: Encoder<'a, B>, kind: Reftype, id: IntegerId, result_id: u8) -> Self {
        if result_id != ActiveElemEncoder::<B>::ZERO_TABLE_RESULT_ID {
            inner.encode(kind);
        }

        ExprsElemEncoder {
            id,
            result_id: result_id & 0b00001111,
            inner: VecEncoder::new(inner),
        }
    }

    pub fn expr(&mut self) -> ExprEncoder<B> {
        self.inner.inc_written();
        ExprEncoder::new(self.inner.inner())
    }
}

impl<'a, B: Backend> Drop for ExprsElemEncoder<'a, B> {
    fn drop(&mut self) {
        debug_assert!(self.result_id.is_power_of_two());
        self.inner
            .inner()
            .integers
            .update(self.id, self.result_id.leading_zeros() as usize);
    }
}

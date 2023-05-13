use crate::{b128::B128Iter, sections::Start, *};

macro_rules! indices {
    ($($name:ident)*) => {$(
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $name(u32);

        impl $name {
            #[allow(dead_code)]
            pub fn len(self) -> usize {
                b128::unsigned_integer_length(self.0 as u64)
            }
        }

        impl Sealed for $name {}
        impl Encode for $name {
            fn encode<B: Backend>(self, encoder: Encoder<B>) {
                self.0.encode(encoder);
            }
        }
        impl Index for $name {
            fn new(index: usize) -> Self {
                Self(index as u32)
            }
        }
    )*};
}

indices! {
    Typeidx
    Funcidx
    Tableidx
    Memidx
    Globalidx
    Elemidx
    Dataidx
    Localidx
    Labelidx
}

macro_rules! sections {
    ($lt:lifetime $b:ident $($name:ident, $id:ident, $idx:ty, $ty:ty;)*) => {$(
        #[derive(Default)]
        pub struct $name {
            start: usize,
            end: usize,
            #[allow(dead_code)]
            entity_count: usize,
        }

        #[allow(dead_code)]
        impl $name {
            fn len(&self) -> usize {
                self.end - self.start
            }
        }

        impl Sealed for $name {}
        impl VecSection for $name {
            const ID: SectionId = SectionId::$id;
            type Index = $idx;
            type Element<$lt, $b: Backend> = $ty;
            fn new(start: usize, end: usize, entity_count: usize, _: Guard) -> Self {
                Self { start, end, entity_count }
            }

            fn start(&self) -> usize {
                self.start
            }

            fn end(&self) -> usize {
                self.end
            }
        }
    )*};
}

impl Memidx {
    pub(super) const ZERO: Self = Self(0);
}

sections! {
    'a B
    TypeSection, Type, Typeidx, FuncEncoder<'a, B>;
    ImportSection, Import, (), Import<'a>;
    FunctionSection, Func, Funcidx, Typeidx;
    TableSection, Table, Tableidx, Tabletype;
    MemorySection, Memory, Memidx, Memtype;
    GlobalSection, Global, Globalidx, GlobalEncoder<'a, B>;
    ExportSection, Export, (), Export<'a>;
    CodeSection, Code, Funcidx, CodeEncoder<'a, B>;
    DataSection, Data, Dataidx, DataEncoder<'a, B>;
}

impl NestedEncode for TypeSection {
    fn new_encoder<'a, B: Backend>(encoder: Encoder<'a, B>, _: Guard) -> Self::Element<'a, B> {
        FuncEncoder::new(encoder)
    }
}

impl NestedEncode for GlobalSection {
    fn new_encoder<'a, B: Backend>(encoder: Encoder<'a, B>, _: Guard) -> Self::Element<'a, B> {
        GlobalEncoder::new(encoder)
    }
}

impl NestedEncode for CodeSection {
    fn new_encoder<'a, B: Backend>(encoder: Encoder<'a, B>, _: Guard) -> Self::Element<'a, B> {
        CodeEncoder::new(encoder)
    }
}

impl NestedEncode for DataSection {
    fn new_encoder<'a, B: Backend>(encoder: Encoder<'a, B>, _: Guard) -> Self::Element<'a, B> {
        DataEncoder::new(encoder)
    }
}

impl ImportSection {
    pub fn index_space_offset(&self) -> IndexSpaceOffset {
        IndexSpaceOffset::new(self.entity_count)
    }
}

#[derive(Default)]
pub struct Module<'a> {
    pub custom_sections: &'a [CustomSection<'a>],
    pub types: TypeSection,
    pub imports: ImportSection,
    pub functions: FunctionSection,
    pub tables: TableSection,
    pub memories: MemorySection,
    pub globals: GlobalSection,
    pub exports: ExportSection,
    pub start: Option<Start>,
    pub elements: TypeSection, // TODO: use ElementSection
    pub include_data_count: bool,
    pub codes: CodeSection,
    pub data: DataSection,
}

pub trait EmmitBuffer {
    fn ensure_capacity(&mut self, amount: usize) -> *mut u8;
    unsafe fn set_len(&mut self, len: usize);
}

impl<'a> Module<'a> {
    pub fn emmit<B: Backend>(
        &self,
        encoder: Encoder<B>,
        buffer: &mut impl EmmitBuffer,
    ) -> Result<(), EmmitError> {
        if self.functions.entity_count != self.codes.entity_count {
            return Err(EmmitError::UnmatchedFuncCount(
                self.functions.entity_count,
                self.codes.entity_count,
            ));
        }

        let (buffer_start, buffer_end) = unsafe {
            let len = self.compute_size(encoder.integers);
            let buffer = buffer.ensure_capacity(len);
            (buffer, buffer.add(len))
        };
        let mut encoder = ModuleEncoder {
            integers: encoder.integers.data(),
            encoded: encoder.output.data(),
            buffer_start,
            buffer_end,
        };

        encoder.preface();
        encoder.write_section(&self.types);
        encoder.write_section(&self.imports);
        encoder.write_section(&self.functions);
        encoder.write_section(&self.tables);
        encoder.write_section(&self.memories);
        encoder.write_section(&self.globals);
        encoder.write_section(&self.exports);

        encoder.write_start_section(&self.start);

        encoder.write_section(&self.elements);

        if self.include_data_count {
            encoder.write_data_count_section(&self.data);
        }

        encoder.write_section(&self.codes);
        encoder.write_section(&self.data);

        for section in self.custom_sections {
            encoder.write_custom_section(section);
        }

        unsafe {
            buffer.set_len(buffer_end.offset_from(buffer_start) as usize);
        }

        Ok(())
    }

    fn compute_size(&self, integers: &impl Integers) -> usize {
        const PREFACE_SIZE: usize = 8;
        integers.bytes_taken(..)
            + self.custom_sections.iter().map(|s| s.len()).sum::<usize>()
            + self.types.len()
            + self.imports.len()
            + self.functions.len()
            + self.tables.len()
            + self.memories.len()
            + self.globals.len()
            + self.exports.len()
            + self.start.map_or(0, |s| s.len())
            + self.elements.len()
            + self
                .include_data_count
                .then_some(b128::unsigned_integer_length(self.data.len() as u64))
                .unwrap_or(0)
            + self.codes.len()
            + self.data.len()
            + PREFACE_SIZE
    }
}

#[derive(Debug)]
pub enum EmmitError {
    UnmatchedFuncCount(usize, usize),
}

struct ModuleEncoder<'a> {
    integers: &'a [Integer],
    encoded: &'a [u8],
    buffer_start: *mut u8,
    buffer_end: *mut u8,
}

impl<'a> ModuleEncoder<'a> {
    fn preface(&mut self) {
        self.write_slice(b"\0asm");
        self.write_slice(&[1, 0, 0, 0]);
    }

    fn write_section(&mut self, section: &impl VecSection) {
        let start = section.start();
        let end = section.end();

        let section_integers_start = self
            .integers
            .binary_search_by_key(&start, |i| i.offset)
            .unwrap_or_else(|i| i);

        let section_integers_end = self
            .integers
            .binary_search_by_key(&end, |i| i.offset)
            .map(|i| i + 1)
            .unwrap_or_else(|i| i);

        let section_integers = &self.integers[section_integers_start..section_integers_end];

        let Some(&first) = section_integers.first() else {
            self.write_slice(&self.encoded[start..end]);
            return;
        };
        self.write_slice(&self.encoded[start..first.offset]);

        for w in section_integers.windows(2) {
            let [a, b] = [w[0], w[1]];
            self.write_integer(a.value);
            self.write_slice(&self.encoded[a.offset..b.offset]);
        }

        let last = section_integers.last().copied().unwrap_or(first);
        self.write_integer(last.value);
        self.write_slice(&self.encoded[last.offset..end]);
    }

    fn write_slice(&mut self, slice: &[u8]) {
        let len = slice.len();

        debug_assert!(unsafe { self.buffer_end.offset_from(self.buffer_start) } as usize >= len);

        unsafe {
            self.buffer_start
                .copy_from_nonoverlapping(slice.as_ptr(), len);
        }

        self.buffer_start = unsafe { self.buffer_start.add(len) };
    }

    fn write_integer(&mut self, integer: u64) {
        debug_assert!(
            unsafe { self.buffer_end.offset_from(self.buffer_start) } as usize
                >= b128::unsigned_integer_length(integer)
        );

        for byte in B128Iter::new(integer) {
            unsafe {
                self.buffer_start.write(byte);
                self.buffer_start = self.buffer_start.add(1);
            }
        }
    }

    fn write_start_section(&mut self, start: &Option<Start>) {
        let Some(start) = start else {
            return;
        };

        self.write_slice(&[SectionId::Start as u8]);
        self.write_integer(start.len() as u64);
        self.write_integer(start.func.0 as u64);
    }

    fn write_data_count_section(&mut self, data: &DataSection) {
        self.write_slice(&[SectionId::DataCount as u8]);
        self.write_integer(b128::unsigned_integer_length(data.len() as u64) as u64);
        self.write_integer(data.len() as u64);
    }

    fn write_custom_section(&mut self, section: &CustomSection) {
        self.write_slice(&[SectionId::Custom as u8]);
        self.write_integer(section.inner_len() as u64);
        self.write_integer(section.name.len() as u64);
        self.write_slice(section.name.as_bytes());
        self.write_integer(section.bytes.len() as u64);
        self.write_slice(section.bytes);
    }
}

impl Drop for ModuleEncoder<'_> {
    fn drop(&mut self) {
        debug_assert_eq!(unsafe { self.buffer_end.offset_from(self.buffer_start) }, 0);
    }
}

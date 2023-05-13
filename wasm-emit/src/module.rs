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
    ($($name:ident)*) => {$(
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
        impl Range for $name {
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
    TypeSection
    ImportSection
    FunctionSection
    TableSection
    MemorySection
    GlobalSection
    ExportSection
    ElementSection
    CodeSection
    DataSection
}

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
    pub elements: ElementSection,
    pub include_data_count: bool,
    pub codes: CodeSection,
    pub data: DataSection,
}

pub trait EmitBuffer {
    fn ensure_capacity(&mut self, amount: usize) -> &mut [u8];
}

impl<'a> Module<'a> {
    pub fn emmit<B: Backend>(
        &self,
        encoder: Encoder<B>,
        buffer: &mut impl EmitBuffer,
    ) -> Result<(), EmmitError> {
        if self.functions.entity_count != self.codes.entity_count {
            return Err(EmmitError::UnmatchedFuncCount(
                self.functions.entity_count,
                self.codes.entity_count,
            ));
        }

        let mut encoder = ModuleEncoder {
            integers: encoder.integers.data(),
            encoded: encoder.output.data(),
            buffer: buffer.ensure_capacity(self.compute_size(encoder.integers)),
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

pub enum EmmitError {
    UnmatchedFuncCount(usize, usize),
}

struct ModuleEncoder<'a> {
    integers: &'a [Integer],
    encoded: &'a [u8],
    buffer: &'a mut [u8],
}

impl<'a> ModuleEncoder<'a> {
    fn preface(&mut self) {
        self.write_slice(b"\0asm");
        self.write_slice(&[1, 0, 0, 0]);
    }

    fn write_section(&mut self, section: &impl Range) {
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
        let buffer = mem::take(&mut self.buffer);
        buffer[..slice.len()].copy_from_slice(slice);
        self.buffer = &mut buffer[slice.len()..];
    }

    fn write_integer(&mut self, integer: u64) {
        let buffer = mem::take(&mut self.buffer);

        let mut iter = buffer.iter_mut();
        let encoder = B128Iter::new(integer);
        iter.by_ref().zip(encoder).for_each(|(b, i)| *b = i);

        self.buffer = iter.into_slice();
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

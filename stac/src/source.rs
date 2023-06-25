use std::{
    iter,
    ops::{Index, IndexMut},
};

use mini_alloc::InternedStr;

use crate::{PoolStore, Ref};

type FileRefRepr = u16;
pub type FileRef = Ref<File, FileRefRepr>;

#[derive(Default)]
pub struct Files {
    files: PoolStore<File, FileRefRepr>,
}

impl Files {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, file: File) -> FileRef {
        self.files.push(file)
    }

    pub fn remove_file(&mut self, file: FileRef) -> File {
        self.files.remove(file)
    }

    pub(crate) fn span_for_offset(&self, offset: usize, from: FileRef) -> Span {
        self[from].span_for_offset(offset, from)
    }

    pub(crate) fn offset_for_span(&self, span: Span) -> usize {
        self[span.file()].offset_for_span(span)
    }
}

impl Index<FileRef> for Files {
    type Output = File;

    fn index(&self, index: FileRef) -> &Self::Output {
        &self.files[index]
    }
}

impl IndexMut<FileRef> for Files {
    fn index_mut(&mut self, index: FileRef) -> &mut Self::Output {
        &mut self.files[index]
    }
}

pub struct File {
    name: InternedStr,
    nline_offsets: Vec<u32>,
    source: String,
    modification_id: u64,
    is_dirty: bool,
}

impl File {
    pub fn new(name: InternedStr, source: String) -> Self {
        Self::with_modification_id(name, source, 0)
    }

    pub fn with_modification_id(name: InternedStr, source: String, modification_id: u64) -> Self {
        let nline_offsets = iter::once(0)
            .chain(source.match_indices('\n').map(|(i, ..)| i as u32))
            .collect::<Vec<_>>();

        assert!(
            nline_offsets.len() <= u16::MAX as usize,
            "amount of lines in file cannot exceed 65535"
        );

        Self {
            name,
            nline_offsets,
            source,
            modification_id,
            is_dirty: true,
        }
    }

    pub fn mark_clean(&mut self) {
        self.is_dirty = false;
    }

    pub fn name(&self) -> InternedStr {
        self.name
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn modification_id(&self) -> u64 {
        self.modification_id
    }

    pub fn update(&mut self, source: String, modification_id: u64) {
        self.source = source;
        self.modification_id = modification_id;
        self.is_dirty = true;
    }

    pub fn is_dirty(&self) -> bool {
        self.is_dirty
    }

    fn offset_for_span(&self, span: Span) -> usize {
        self.nline_offsets[span.row as usize] as usize + span.col as usize
    }

    fn span_for_offset(&self, pos: usize, this_ref: FileRef) -> Span {
        let row = self
            .nline_offsets
            .binary_search(&(pos as u32))
            .unwrap_or_else(|i| i - 1);

        let col = pos - self.nline_offsets[row] as usize;

        Span::new(row, col, this_ref)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    row: u16,
    col: u16,
    file: FileRef,
}

impl Span {
    pub(crate) fn new(row: usize, col: usize, file_ref: FileRef) -> Self {
        Self {
            row: row as u16,
            col: col as u16,
            file: file_ref,
        }
    }

    pub fn file(&self) -> FileRef {
        self.file
    }

    pub fn row(&self) -> usize {
        self.row as usize
    }

    pub fn col(&self) -> usize {
        self.col as usize
    }

    pub(crate) fn shift(&self, index: usize) -> Span {
        Span {
            row: self.row,
            col: self.col + index as u16,
            file: self.file,
        }
    }
}

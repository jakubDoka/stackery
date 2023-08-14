use std::{
    iter,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{DefaultRef, PoolStore, Ref, RefRepr};

type FileRefRepr = u16;
pub type FileRef = Ref<File, FileRefRepr>;
pub type Files = PoolStore<File, FileRefRepr>;

#[derive(Debug)]
pub struct File {
    name: Arc<Path>,
    nline_offsets: Vec<u32>,
    source: String,
    modification_id: u64,
    package: Option<FileRef>,
    is_dirty: bool,
}

impl DefaultRef for File {
    fn default_ref<R: RefRepr>() -> Ref<Self, R> {
        Ref::from_repr(R::MIN)
    }
}

impl File {
    pub fn new(name: PathBuf, source: String) -> Self {
        Self::with_modification_id(name, source, 0)
    }

    pub fn with_modification_id(name: PathBuf, source: String, modification_id: u64) -> Self {
        let nline_offsets = Self::compute_nline_offsets(&source);

        assert!(
            nline_offsets.len() <= u16::MAX as usize,
            "amount of lines in file cannot exceed {}",
            u16::MAX
        );

        Self {
            name: Arc::from(name),
            nline_offsets,
            source,
            modification_id,
            package: None,
            is_dirty: true,
        }
    }

    fn compute_nline_offsets(source: &str) -> Vec<u32> {
        iter::once(0)
            .chain(source.match_indices('\n').map(|(i, ..)| i as u32))
            .collect::<Vec<_>>()
    }

    pub fn mark_clean(&mut self) {
        self.is_dirty = false;
    }

    pub fn name(&self) -> &Arc<Path> {
        &self.name
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn modification_id(&self) -> u64 {
        self.modification_id
    }

    pub fn update(&mut self, source: String, modification_id: u64) {
        self.source = source;
        self.nline_offsets = Self::compute_nline_offsets(&self.source);
        self.modification_id = modification_id;
        self.is_dirty = true;
    }

    pub fn set_package(&mut self, package: FileRef) {
        self.package = Some(package);
    }

    pub fn package(&self) -> Option<FileRef> {
        self.package
    }

    pub fn is_dirty(&self) -> bool {
        self.is_dirty
    }

    pub fn offset_for_span(&self, span: Span) -> usize {
        self.nline_offsets[span.row as usize] as usize + span.col as usize
    }

    pub fn span_for_offset(&self, pos: usize, this_ref: FileRef) -> Span {
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
            row: row as _,
            col: col as _,
            file: file_ref,
        }
    }

    pub fn file(&self) -> FileRef {
        self.file
    }

    pub fn row(&self) -> usize {
        self.row as _
    }

    pub fn col(&self) -> usize {
        self.col as _
    }

    pub fn to_pos(&self) -> Pos {
        Pos {
            row: self.row,
            col: self.col,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Pos {
    row: u16,
    col: u16,
}

impl Pos {
    pub fn new(row: usize, col: usize) -> Self {
        Self {
            row: row as _,
            col: col as _,
        }
    }

    pub fn row(&self) -> usize {
        self.row as _
    }

    pub fn col(&self) -> usize {
        self.col as _
    }

    pub fn to_span(&self, file_ref: FileRef) -> Span {
        Span {
            row: self.row,
            col: self.col,
            file: file_ref,
        }
    }
}

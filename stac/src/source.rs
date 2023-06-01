use std::{
    iter,
    ops::{Index, IndexMut},
};

use mini_alloc::InternedStr;

#[derive(Default)]
pub struct Files {
    files: Vec<Option<File>>,
    free: Vec<FileRef>,
}

impl Files {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn add(&mut self, file: File) -> FileRef {
        let index = self.free.pop().unwrap_or_else(|| {
            let index = self.files.len();
            self.files.push(None);
            FileRef::new(index)
        });

        self.files[index.0 as usize] = Some(file);
        index
    }

    pub fn remove_file(&mut self, file: FileRef) -> File {
        self.files[file.0 as usize]
            .take()
            .expect("file has already been removed")
    }

    pub(crate) fn span_for_pos(&self, pos: usize, from: FileRef) -> Span {
        let file = &self[from];
        let (row, col) = file.source()[..pos]
            .match_indices('\n')
            .map(|(i, ..)| i)
            .enumerate()
            .last()
            .unwrap_or((0, pos));

        Span::new(row, col, from)
    }
}

impl Index<FileRef> for Files {
    type Output = File;

    fn index(&self, index: FileRef) -> &Self::Output {
        self.files[index.0 as usize]
            .as_ref()
            .expect("file has been removed")
    }
}

impl IndexMut<FileRef> for Files {
    fn index_mut(&mut self, index: FileRef) -> &mut Self::Output {
        self.files[index.0 as usize]
            .as_mut()
            .expect("file has been removed")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileRef(u16);

impl FileRef {
    fn new(index: usize) -> Self {
        Self(
            index
                .try_into()
                .expect("amount of source files cannot exceed 65535"),
        )
    }

    #[cfg(test)]
    pub fn fake() -> Self {
        Self(u16::MAX)
    }
}

pub struct File {
    name: InternedStr,
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
            .chain(source.match_indices('\n').map(|(i, ..)| i as u16))
            .collect::<Vec<_>>();

        assert!(
            nline_offsets.len() <= u16::MAX as usize,
            "amount of lines in file cannot exceed 65535"
        );

        Self {
            name,
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

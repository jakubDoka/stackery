use std::{
    iter,
    path::{Path, PathBuf},
};

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

    pub fn add_file(&mut self, file: File) -> (FileRef, &mut File) {
        let index = self.free.pop().unwrap_or_else(|| {
            let index = self.files.len();
            self.files.push(None);
            FileRef::new(index)
        });

        let moved = self.files[index.0 as usize].insert(file);
        (index, moved)
    }

    pub fn get_file(&self, file: FileRef) -> &File {
        self.files[file.0 as usize]
            .as_ref()
            .expect("file has been removed")
    }

    pub fn remove_file(&mut self, file: FileRef) -> File {
        self.files[file.0 as usize]
            .take()
            .expect("file has already been removed")
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
}

pub struct File {
    name: PathBuf,
    source: String,
}

impl File {
    pub fn new(name: PathBuf, source: String) -> Self {
        let nline_offsets = iter::once(0)
            .chain(source.match_indices('\n').map(|(i, ..)| i as u16))
            .collect::<Vec<_>>();

        assert!(
            nline_offsets.len() <= u16::MAX as usize,
            "amount of lines in file cannot exceed 65535"
        );

        Self { name, source }
    }

    pub fn name(&self) -> &Path {
        &self.name
    }

    pub fn source(&self) -> &str {
        &self.source
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
}

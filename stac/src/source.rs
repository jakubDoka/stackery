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

    pub(crate) fn finalize_file(&mut self, file: FileRef, offsets: Vec<u16>) {
        let file = self.files[file.0 as usize]
            .as_mut()
            .expect("file has already been removed");

        file.nline_offsets = offsets;
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
    nline_offsets: Vec<u16>,
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

        Self {
            name,
            nline_offsets,
            source,
        }
    }

    pub fn name(&self) -> &Path {
        &self.name
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn get_line_and_col_at_pos(&self, pos: u16) -> (usize, usize) {
        let line = self
            .nline_offsets
            .binary_search(&pos)
            .unwrap_or_else(|i| i)
            .saturating_sub(1);
        dbg!(line, &self.nline_offsets, pos);
        let col = pos - self.nline_offsets[line] - (line > 0) as u16;
        (line, col as usize)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn line_col() {
        let file = File::new(PathBuf::from("test"), "a\nbc\n\nd".into());

        let line_cols = [
            (0, (0, 0)),
            (1, (0, 1)),
            (2, (1, 0)),
            (3, (1, 1)),
            (4, (1, 2)),
            (5, (2, 0)),
            (6, (3, 0)),
            (7, (3, 1)),
        ];

        let failed = line_cols
            .iter()
            .filter_map(|&(pos, res)| {
                let lc = file.get_line_and_col_at_pos(pos);
                (lc != res).then(|| (pos, lc, res))
            })
            .collect::<Vec<_>>();

        assert!(
            failed.is_empty(),
            "failed to convert positions to line/column:\n{:?}",
            failed
        );
    }
}

use std::io;

use crate::*;
use mini_alloc::*;

mod loader_impl;

pub type ModuleRefRepr = u16;
pub type ModuleRef = Ref<Module, ModuleRefRepr>;
pub type ModuleDeps = Slice<ModuleRef, ModuleRefRepr>;

#[derive(Copy, Clone)]
pub struct Module {
    deps: ModuleDeps,
    file: FileRef,
}

impl Module {
    fn new(file: FileRef) -> Module {
        Module {
            deps: Default::default(),
            file,
        }
    }
}

pub struct ModuleMeta {
    order: Vec<ModuleRef>,
    root: ModuleRef,
    updated_modules: BitSet,
}

impl ModuleMeta {
    pub fn order(&self) -> &[ModuleRef] {
        &self.order
    }

    pub fn root(&self) -> ModuleRef {
        self.root
    }

    pub fn is_updated(&self, module: ModuleRef) -> bool {
        self.updated_modules.contains(module.index())
    }

    pub fn no_changes(&self) -> bool {
        !self.updated_modules.contains(self.root.index())
    }

    pub fn changed(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.order().iter().copied().filter(|&m| self.is_updated(m))
    }
}

#[derive(Default)]
pub struct Modules {
    eintities: PoolStore<Module, ModuleRefRepr>,
    loaded_modules: FnvHashMap<FileRef, ModuleRef>,
    deps: VecStore<ModuleRef, ModuleRefRepr>,
    files: Files,
}

impl Modules {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name_of(&self, module: ModuleRef) -> InternedStr {
        let module = &self.eintities[module];
        self.files[module.file].name()
    }

    pub fn files(&self) -> &Files {
        &self.files
    }
}

pub struct LoaderCtx<'a> {
    pub files: &'a mut Files,
    pub interner: &'a StrInterner,
}

pub trait Loader {
    async fn load(
        &mut self,
        ctx: LoaderCtx<'_>,
        from: Option<FileRef>,
        id: InternedStr,
    ) -> Result<FileRef, io::Error>;
}

pub struct CacheLoader<'a, L: Loader> {
    loader: &'a mut L,
    modules: &'a mut Modules,
    interner: &'a StrInterner,
    diagnostics: &'a mut Diagnostics,
}

impl<'a, L: Loader> CacheLoader<'a, L> {
    pub fn new(
        loader: &'a mut L,
        modules: &'a mut Modules,
        interner: &'a StrInterner,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            loader,
            modules,
            interner,
            diagnostics,
        }
    }
}

#[cfg(test)]
mod test_util {
    use std::collections::HashMap;

    use crate::*;
    use mini_alloc::*;

    pub struct LoaderMock<'a> {
        modules: HashMap<&'a str, &'a str>,
        loaded: HashMap<InternedStr, FileRef>,
    }

    impl<'a> LoaderMock<'a> {
        pub(super) fn new(mut files: &'a str) -> Self {
            let delim = '`';
            let mut modules = HashMap::new();
            while let Some((_, rest)) = files.split_once(delim) {
                let (name, rest) = rest.split_once(char::is_whitespace).unwrap();
                let (source, rest) = rest.split_once(delim).unwrap();
                modules.insert(name, source);
                files = rest;
            }
            Self {
                modules,
                loaded: HashMap::new(),
            }
        }
    }

    impl<'a> Loader for LoaderMock<'a> {
        async fn load(
            &mut self,
            ctx: crate::LoaderCtx<'_>,
            _: Option<crate::FileRef>,
            id: mini_alloc::InternedStr,
        ) -> Result<crate::FileRef, std::io::Error> {
            if let Some(&file) = self.loaded.get(&id) {
                return Ok(file);
            }

            let id_str = &ctx.interner[id];

            let &source = self.modules.get(id_str).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("module {} not found", id_str),
                )
            })?;

            let file = File::new(id, source.to_owned());
            let file = ctx.files.add(file);

            self.loaded.insert(id, file);

            Ok(file)
        }
    }
}

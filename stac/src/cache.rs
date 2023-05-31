use std::io;

use mini_alloc::{FnvHashMap, InternedStr, StrInterner};

use crate::*;

mod ast_loader_impl;
pub mod scope;
pub mod storage;

macro_rules! decl_ast_model {
    ($name:ident, $view_name:ident, $push_name:ident, $range_name:ident <$lt:lifetime> {$(
        $field:ident: $ty:ty,
    )*}) => {
        #[derive(Default)]
        pub struct $name {
            $($field: CacheVec<$ty>,)*
        }

        impl $name {
            pub fn push(&mut self) -> $push_name<'_> {
                $push_name {
                    $($field: self.$field.push(),)*
                }
            }

            pub fn view(&self, range: $range_name) -> $view_name<'_> {
                $view_name {
                    $($field: self.$field.view(range.$field),)*
                }
            }
        }

        pub struct $view_name<'a> {
            $(pub $field: &'a CacheVecView<$ty>,)*
        }

        pub struct $push_name<'a> {
            $(pub $field: CacheVecPush<'a, $ty>,)*
        }

        #[derive(Copy, Clone, Default)]
        pub struct $range_name {
            $($field: CacheVecRange<$ty>,)*
        }
    };
}

decl_ast_model! {
    Cache, CacheView, CachePush, CacheRange <'a> {
        consts: LiteralKindAst,
    }
}

pub type ModuleRef = CacheRef<Module>;

#[derive(Copy, Clone)]
pub struct Module {
    scope: ScopeRange,
    body: CacheRange,
    deps: CacheVecRange<ModuleRef>,
    file: FileRef,
}

impl Module {
    fn new(file: FileRef) -> Module {
        Module {
            scope: ScopeRange::default(),
            body: CacheRange::default(),
            deps: CacheVecRange::default(),
            file,
        }
    }
}

#[derive(Default)]
pub struct Modules {
    cahce: Cache,
    scope: ScopeCache,
    eintities: CachePool<Module>,
    loaded_modules: FnvHashMap<FileRef, CacheRef<Module>>,
    deps: CacheVec<ModuleRef>,
    files: Files,
}

impl Modules {
    pub fn new() -> Self {
        Self::default()
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
    types: &'a Types,
    interner: &'a StrInterner,
    diagnostics: &'a mut Diagnostics,
}

impl<'a, L: Loader> CacheLoader<'a, L> {
    pub fn new(
        loader: &'a mut L,
        modules: &'a mut Modules,
        types: &'a Types,
        interner: &'a StrInterner,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            loader,
            modules,
            types,
            interner,
            diagnostics,
        }
    }
}

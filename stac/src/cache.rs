use std::io;

use mini_alloc::{FnvHashMap, InternedStr, StrInterner};

use crate::*;

mod loader_impl;
mod parser_impl;
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
                    $($field: &self.$field[range.$field],)*
                }
            }

            pub fn preserve_chunks<'a>(
                &mut self,
                temp_mem: &mut TempMemBase,
                chunks: impl IntoIterator<Item = &'a mut $range_name>,
            ) {
                let scope = temp_mem.scope();
                let slice = scope.collect(chunks);

                $(
                    slice.sort_unstable_by_key(|ch| ch.$field.base());
                    self.$field
                        .preserve_chunks(slice.iter_mut().map(|ch| &mut ch.$field));
                )*
            }

        }

        pub struct $view_name<'a> {
            $(pub $field: &'a CacheVecView<$ty>,)*
        }

        pub struct $push_name<'a> {
            $(pub $field: CacheVecPush<'a, $ty>,)*
        }

        impl<'a> $push_name<'a> {
            pub fn view(&self, range: $range_name) -> $view_name<'_> {
                $view_name {
                    $($field: &self.$field[range.$field],)*
                }
            }

            pub fn finish(self) -> $range_name {
                $range_name {
                    $($field: self.$field.finish(),)*
                }
            }
        }

        #[derive(Copy, Clone, Default)]
        pub struct $range_name {
            $($field: CacheVecRange<$ty>,)*
        }
    };
}

impl Cache {}

decl_ast_model! {
    Cache, CacheView, CachePush, CacheRange <'a> {
        consts: LiteralKindAst,
        exprs: ast::Expr,
        strs: InternedStr,
        named_exprs: ast::StructField,
        struct_fields: ast::StructField,
        enums: ast::Enum,
        calls: ast::Call,
        funcs: ast::Func,
        func_args: ast::FuncArg,
        fields: ast::FieldAccess,
        ifs: ast::If,
    }
}

pub mod ast {
    use mini_alloc::InternedStr;

    use crate::*;

    #[derive(Copy, Clone)]
    pub enum Expr {
        Lit(CacheRef<LiteralKindAst>),
        Ident(Sym),
        Import(ModuleRef),
        Str(CacheRef<InternedStr>),
        Block {
            trailing_semi: bool,
            exprs: CacheSlice<Expr>,
        },
        Unary(OpCode, CacheRef<Expr>),
        Binary(CacheRef<Expr>, OpCode, CacheRef<Expr>),
        Array(CacheSlice<Expr>),
        FilledArray(CacheRef<Expr>, CacheRef<Expr>),
        Tuple(CacheSlice<Expr>),
        Struct(CacheSlice<StructField>),
        Enum(CacheRef<Enum>),
        Call(CacheRef<Call>),
        Func(CacheRef<Func>),
        Decl(Sym, CacheRef<Expr>),
        Loop(Option<Sym>, CacheRef<Expr>),
        Index(CacheRef<Expr>, CacheRef<Expr>),
        Break(Option<Sym>, Option<CacheRef<Expr>>),
        Continue(Option<Sym>),
        Field(CacheRef<FieldAccess>),
        If(CacheRef<If>),
        Ret(Option<CacheRef<Expr>>),
        Unknown,
    }

    #[derive(Copy, Clone)]
    pub enum StructField {
        Named { name: InternedStr, value: Expr },
        Inline(InternedStr),
        Embed(Expr),
    }

    #[derive(Copy, Clone)]
    pub struct Enum {
        pub name: InternedStr,
        pub value: Option<Expr>,
    }

    #[derive(Copy, Clone)]
    pub struct Call {
        pub caller: Expr,
        pub args: CacheSlice<Expr>,
    }

    #[derive(Copy, Clone)]
    pub struct Func {
        pub args: CacheSlice<FuncArg>,
        pub body: Expr,
    }

    #[derive(Copy, Clone)]
    pub struct FuncArg {
        pub name: Sym,
        pub default: Option<Expr>,
    }

    #[derive(Copy, Clone)]
    pub struct FieldAccess {
        pub expr: Expr,
        pub field: InternedStr,
    }

    #[derive(Copy, Clone)]
    pub struct If {
        pub cond: Expr,
        pub then: Expr,
        pub else_: Option<Expr>,
    }
}

pub type ModuleRef = CacheRef<Module>;

#[derive(Copy, Clone, Default)]
pub struct ParsedModule {
    scope: ScopeRange,
    cache: CacheRange,
    body: CacheSlice<ast::Expr>,
}

#[derive(Copy, Clone)]
pub struct Module {
    parsed: ParsedModule,
    deps: CacheVecRange<ModuleRef>,
    file: FileRef,
}

impl Module {
    fn new(file: FileRef) -> Module {
        Module {
            parsed: ParsedModule::default(),
            deps: CacheVecRange::default(),
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

    fn changed(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.order().iter().copied().filter(|&m| self.is_updated(m))
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

    pub fn name_of(&self, module: ModuleRef) -> InternedStr {
        let module = &self.eintities[module];
        self.files[module.file].name()
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

pub struct CacheParser<'a> {
    modules: &'a mut Modules,
    interner: &'a StrInterner,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> CacheParser<'a> {
    pub fn new(
        modules: &'a mut Modules,
        interner: &'a StrInterner,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            modules,
            interner,
            diagnostics,
        }
    }
}

#[cfg(test)]
mod test_util {
    use std::collections::HashMap;

    use mini_alloc::InternedStr;

    use crate::{File, FileRef, Loader};

    pub(super) struct LoaderMock<'a> {
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

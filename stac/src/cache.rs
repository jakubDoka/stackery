use mini_alloc::{InternedStr, StrInterner};

use crate::*;

// mod instr_impl;
// pub mod instrs;
// mod loader_impl;

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
        strs: InternedStr,
        instrs: instrs::Instr,
    }
}

#[derive(Copy, Clone, Default)]
pub struct ModuleInstrs {
    scope: ScopeRange,
    cache: CacheRange,
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

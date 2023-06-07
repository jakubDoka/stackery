use proc_macro::TokenStream;
use syn::{Attribute, Ident, Type, TypeParam};

#[proc_macro_derive(Serde128, attributes(ignore_param, uses))]
pub fn derive_serde_128(ts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(ts as syn::DeriveInput);

    let top_level_attrs = handle_top_level_attrs(&input.attrs);
    let generics = Generics {
        generics: input.generics,
        top_level_attrs,
    };

    match input.data {
        syn::Data::Struct(s) => handle_struct(input.ident, s, generics),
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => panic!("unions are not supported"),
    }
}

struct Generics {
    generics: syn::Generics,
    top_level_attrs: TopLevelAttrs,
}

impl Generics {
    fn generic_params(&self) -> impl Iterator<Item = &Ident> {
        self.generics.params.iter().map(|p| match p {
            syn::GenericParam::Type(t) => &t.ident,
            _ => panic!("only type parameters are supported"),
        })
    }

    fn bounded_generics(&self) -> impl Iterator<Item = TypeParam> + '_ {
        self.generics
            .params
            .iter()
            .map(|p| match p {
                syn::GenericParam::Type(t) => t,
                _ => panic!("only type parameters are supported"),
            })
            .map(|t| {
                let mut t = t.clone();
                if !self.top_level_attrs.iter().any(|a| &t.ident == a) {
                    t.bounds.push(syn::parse_quote!(Serde128));
                }
                t.default = None;
                t
            })
    }

    fn where_clause(&self) -> Option<&syn::WhereClause> {
        self.generics.where_clause.as_ref()
    }
}

fn handle_struct(ident: Ident, s: syn::DataStruct, generics: Generics) -> TokenStream {
    match s.fields {
        syn::Fields::Named(n) => handle_named_struct(ident, n, generics),
        syn::Fields::Unnamed(u) => handle_unnamed_struct(ident, u, generics),
        syn::Fields::Unit => handle_unit_struct(ident),
    }
}

fn handle_named_struct(name: Ident, n: syn::FieldsNamed, generics: Generics) -> TokenStream {
    let bounded_generics = generics.bounded_generics();
    let generic_params = generics.generic_params();
    let where_clause = generics.where_clause();

    let field_names = n
        .named
        .iter()
        .map(|f| {
            f.ident
                .clone()
                .expect("why doe the field not have the name?")
        })
        .collect::<Vec<_>>();
    let field_names_1 = field_names.iter();
    let serializes = n.named.iter().map(|f| {
        let name = &f.ident;
        match find_use(&f.attrs) {
            Some(encoder) => quote::quote! {
               serde_base128::UseSerde128::serialize(<#encoder>::default(), #name, encoder);
            },
            None => quote::quote! {
                serde_base128::Serde128::serialize(#name, encoder);
            },
        }
    });
    let field_names_2 = field_names.iter();
    let deserializes = n.named.iter().map(|t| match find_use(&t.attrs) {
        Some(encoder) => quote::quote! {
           serde_base128::UseSerde128::deserialize(<#encoder>::default(), decoder)
        },
        None => quote::quote! {
            serde_base128::Serde128::deserialize(decoder)
        },
    });

    quote::quote! {
        impl<#(#bounded_generics),*> serde_base128::Serde128 for #name<#(#generic_params),*> #where_clause {
            fn serialize(&self, encoder: &mut serde_base128::Encoder) {
                let Self { #(#field_names_1),* } = self;
                #(#serializes)*
            }

            unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
                Self {
                    #(#field_names_2: #deserializes,)*
                }
            }
        }
    }.into()
}

fn handle_unnamed_struct(name: Ident, u: syn::FieldsUnnamed, generics: Generics) -> TokenStream {
    let bounded_generics = generics.bounded_generics();
    let generic_params = generics.generic_params();
    let where_clause = generics.where_clause();

    let field_names = (0..u.unnamed.len())
        .map(|i| Ident::new(&format!("f{}", i), name.span()))
        .collect::<Vec<_>>();
    let field_names_1 = field_names.iter();
    let serializes =
        field_names
            .iter()
            .zip(u.unnamed.iter())
            .map(|(f, t)| match find_use(&t.attrs) {
                Some(encoder) => quote::quote! {
                   serde_base128::UseSerde128::serialize(<#encoder>::default(), #f, encoder);
                },
                None => quote::quote! {
                    serde_base128::Serde128::serialize(#f, encoder);
                },
            });
    let field_names_2 = field_names.iter();
    let deserializes = u.unnamed.iter().map(|t| match find_use(&t.attrs) {
        Some(encoder) => quote::quote! {
           serde_base128::UseSerde128::deserialize(<#encoder>::default(), decoder);
        },
        None => quote::quote! {
            serde_base128::Serde128::deserialize(decoder);
        },
    });
    let field_names_3 = field_names.iter();

    quote::quote! {
        impl<#(#bounded_generics),*> serde_base128::Serde128 for #name<#(#generic_params),*> #where_clause {
            fn serialize(&self, encoder: &mut serde_base128::Encoder) {
                let Self(#(#field_names_1),*) = self;
                #(#serializes)*
            }

            unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
                #(let #field_names_2 = #deserializes;)*
                Self(#(#field_names_3),*)
            }
        }
    }.into()
}

fn handle_unit_struct(ident: Ident) -> TokenStream {
    quote::quote! {
        impl serde_base128::Serde128 for #ident {
            fn serialize(&self, encoder: &mut serde_base128::Encoder) {}
            unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {Self}
        }
    }
    .into()
}

type TopLevelAttrs = Vec<Ident>;

fn handle_top_level_attrs(attrs: &[Attribute]) -> TopLevelAttrs {
    filter_attrs(attrs, "ignore_param")
        .map(|attr| attr.parse_args::<Ident>().expect("expected meta list"))
        .collect()
}

fn filter_attrs<'a>(attrs: &'a [Attribute], key: &'a str) -> impl Iterator<Item = &'a Attribute> {
    attrs.iter().filter(|attr| attr.path().is_ident(key))
}

fn find_use(attrs: &[Attribute]) -> Option<Type> {
    filter_attrs(attrs, "uses").find_map(|attr| attr.parse_args::<Type>().ok())
}

use std::sync::Arc;

use crate::{ModuleRef, Ref, Vis};
use mini_alloc::IdentStr;

mod unify_impl;

pub type TySlice<T> = Arc<[T]>;
pub type TyRef<T> = Arc<T>;
pub type Fields = TySlice<Field>;
pub type Methods = TySlice<Method>;
pub type Generics = TySlice<TySlice<Spec>>;

pub type TypeRefRepr = u16;
pub type StructRef = Ref<Struct, TypeRefRepr>;
pub type SpecBaseRef = Ref<SpecBase, TypeRefRepr>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructId {
    pub module: ModuleRef,
    pub id: StructRef,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SpecBaseId {
    pub module: ModuleRef,
    pub id: StructRef,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spec {
    pub base: SpecBaseId,
    pub args: TySlice<Type>,
    pub asocs: TySlice<InstAsocType>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InstAsocType {
    pub name: IdentStr,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SpecBase {
    pub vis: Vis,
    pub name: IdentStr,
    pub module: ModuleRef,
    pub generics: Generics,
    pub methods: Methods,
    pub asoc_types: TySlice<AsocTypeDef>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AsocTypeDef {
    pub name: IdentStr,
    pub generics: Generics,
    pub bounds: TySlice<Spec>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Method {
    pub name: IdentStr,
    pub signature: Signature,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub generics: Generics,
    pub args: TySlice<Type>,
    pub ret: TyRef<Type>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    BuiltIn(BuiltInType),
    Interned(InternedType),
    Param(ParamType),
    Asociated(TyRef<AsocType>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AsocType {
    pub on: Type,
    pub name: IdentStr,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum DefinedType {
    Struct(StructId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltInType {
    Int(IntType),
    Bool,
}

macro gen_builtin_int_types($($name:ident)*) {$(
    pub const $name: BuiltInType = BuiltInType::Int(IntType::$name);
)*}

impl BuiltInType {
    gen_builtin_int_types! {
        I8 U8 I16 U16 I32 U32 I64 U64
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct IntType {
    pub size: IntSize,
    pub signed: bool,
}

macro gen_int_types($($name:ident $size:ident $signed:literal;)*) {$(
    pub const $name: IntType = IntType {
        size: IntSize::$size,
        signed: $signed,
    };
)*}

impl IntType {
    gen_int_types! {
        I8 W8 true;
        U8 W8 false;
        I16 W16 true;
        U16 W16 false;
        I32 W32 true;
        U32 W32 false;
        I64 W64 true;
        U64 W64 false;
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum IntSize {
    W8,
    W16,
    W32,
    W64,
    W128,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum InternedType {
    Instance(InstanceType),
    Pointer(PointerType),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InstanceType {
    pub ty: DefinedType,
    pub args: TySlice<Type>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PointerType {
    pub mutability: Mutability,
    pub safety: Safety,
    pub ty: TyRef<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Mut,
    Generic(ParamType),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Safety {
    Safe,
    Unsafe,
    Open,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ParamType(u8);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub vis: Vis,
    pub name: IdentStr,
    pub module: ModuleRef,
    pub generics: Generics,
    pub fields: Fields,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub vis: Vis,
    pub name: IdentStr,
    pub ty: Type,
}

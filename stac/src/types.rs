use crate::{Ref, Slice};
use mini_alloc::{Interned, InternedSlice, InternedStr};

pub type TypeIndex = u16;
pub type NamedTypes = Slice<NamedType, TypeIndex>;
pub type TypeRef = Ref<Type, TypeIndex>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Defined(DefinedType),
    BuiltIn(BuiltInType),
    Interned(InternedType),
    Param(ParamType),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum DefinedType {
    Struct(Struct),
    Enum(Enum),
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

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum InternedType {
    Instance(InstanceType),
    Pointer(PointerType),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct InstanceType {
    pub ty: DefinedType,
    pub args: InternedSlice<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PointerType {
    pub mutability: Mutability,
    pub ty: Interned<Type>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Mut,
    Generic(ParamType),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ParamType(u8);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: NamedTypes,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub variants: NamedTypes,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct NamedType {
    pub name: InternedStr,
    pub ty: Type,
}

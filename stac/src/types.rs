use std::fmt;

mod unify_impl;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub args: Vec<Type>,
    pub ret: Type,
}

impl Default for Signature {
    fn default() -> Self {
        Self {
            args: Vec::new(),
            ret: Type::BuiltIn(BuiltInType::Unit),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    BuiltIn(BuiltInType),
}

impl Type {
    pub const UNIT: Self = Self::BuiltIn(BuiltInType::Unit);
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BuiltIn(b) => b.fmt(f),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum BuiltInType {
    Int(IntType),
    Bool,
    Unit,
    Unknown,
}

macro gen_builtin_int_types($($name:ident)*) {
    $(
        pub const $name: BuiltInType = BuiltInType::Int(IntType::$name);
    )*
}

impl BuiltInType {
    gen_builtin_int_types! {
        I8 U8 I16 U16 I32 U32 I64 U64
    }
}

impl fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => i.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Unit => write!(f, "unit"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IntType {
    pub size: IntSize,
    pub signed: bool,
}

macro_rules! gen_int_types {
    ($($code_name:ident $name:ident $size:ident $signed:literal;)*) => {

        $(
            pub const $name: IntType = IntType {
                size: IntSize::$size,
                signed: $signed,
            };
        )*

        pub const INT_TYPES: &[Self] = &[$(Self::$name,)*];
        pub const INT_TYPE_NAMES: &[&'static str] = &[$(stringify!($code_name),)*];
    };
}

impl IntType {
    gen_int_types! {
        i8  I8 W8 true;
        u8  U8 W8 false;
        i16 I16 W16 true;
        u16 U16 W16 false;
        i32 I32 W32 true;
        u32 U32 W32 false;
        i64 I64 W64 true;
        u64 U64 W64 false;
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign = if self.signed { 'i' } else { 'u' };
        sign.fmt(f)?;
        self.size.fmt(f)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum IntSize {
    W8,
    W16,
    W32,
    W64,
    W128,
}

impl IntSize {
    pub fn bit_width(self) -> u8 {
        match self {
            Self::W8 => 8,
            Self::W16 => 16,
            Self::W32 => 32,
            Self::W64 => 64,
            Self::W128 => 128,
        }
    }
}

impl fmt::Display for IntSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.bit_width().fmt(f)
    }
}

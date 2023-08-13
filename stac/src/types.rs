use std::fmt;

use crate::FuncId;

mod unify_impl;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature {
    pub args: Vec<Type>,
    pub ret: Type,
}

impl Signature {
    fn unify(&self, other: &Self) -> Option<Self> {
        if self.args.len() != other.args.len() {
            return None;
        }

        let args = self
            .args
            .iter()
            .zip(other.args.iter())
            .map(|(lhs, rhs)| lhs.unify(rhs))
            .collect::<Option<Vec<_>>>()?;

        let ret = self.ret.unify(&other.ret)?;

        Some(Self { args, ret })
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "({}) -> {}", args, self.ret)
    }
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
    Func(FuncType),
}

impl Type {
    pub const UNIT: Self = Self::BuiltIn(BuiltInType::Unit);

    pub(crate) fn unify(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::BuiltIn(lhs), &Self::BuiltIn(rhs)) => lhs.merge(rhs).map(Self::BuiltIn),
            (Self::Func(lhs), Self::Func(rgs)) => lhs.unify(rgs).map(Self::Func),
            _ => None,
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::BuiltIn(BuiltInType::Int(IntType { signed: true, .. }))
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BuiltIn(b) => b.fmt(f),
            Self::Func(sig) => sig.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum FuncType {
    Static(FuncId),
}

impl FuncType {
    pub fn unify(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Static(lhs), Self::Static(rhs)) if lhs == rhs => Some(Self::Static(*lhs)),
            _ => None,
        }
    }
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Static(id) => write!(f, "func#{}:{}", id.module.index(), id.module.index()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum BuiltInType {
    Int(IntType),
    Integer,
    Bool,
    Unit,
    Type,
    Module,
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

    fn merge(self, rhs: BuiltInType) -> Option<BuiltInType> {
        match (self, rhs) {
            (a, b) if a == b => Some(a),
            (Self::Unknown, o) | (o, Self::Unknown) => Some(o),
            (Self::Integer, Self::Int(o)) | (Self::Int(o), Self::Integer) => Some(Self::Int(o)),
            _ => None,
        }
    }
}

impl fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => i.fmt(f),
            Self::Integer => "{integer}".fmt(f),
            Self::Bool => "bool".fmt(f),
            Self::Unit => "()".fmt(f),
            Self::Type => "type".fmt(f),
            Self::Module => "module".fmt(f),
            Self::Unknown => "{unknown}".fmt(f),
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

    pub(crate) fn bite_width(&self) -> u8 {
        self.bit_width() / 8
    }
}

impl fmt::Display for IntSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.bit_width().fmt(f)
    }
}

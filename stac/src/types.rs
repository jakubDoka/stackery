use std::fmt;

use crate::{DefaultRef, FuncId};

mod unify_impl;

pub type ParamId = u16;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature {
    pub args: Vec<Type>,
    pub ret: Type,
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

pub enum UnificationError {
    Equal,
    Incompatible,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    BuiltIn(BuiltInType),
    Func(FuncType),
}

impl From<BuiltInType> for Type {
    fn from(b: BuiltInType) -> Self {
        Self::BuiltIn(b)
    }
}

impl Type {
    pub const UNIT: Self = Self::BuiltIn(BuiltInType::Unit);

    pub(crate) fn unify(&self, other: &Self) -> Result<Self, UnificationError> {
        match (self, other) {
            (&Self::BuiltIn(lhs), &Self::BuiltIn(rhs)) => lhs.unify(rhs).map(Self::BuiltIn),
            (Self::Func(lhs), Self::Func(rgs)) => lhs.unify(rgs).map(Self::Func),
            _ => Err(UnificationError::Incompatible),
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::BuiltIn(b) if b.is_signed()
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

impl DefaultRef for Type {
    fn default_ref<R: crate::RefRepr>() -> crate::Ref<Self, R> {
        crate::Ref::from_repr(R::MIN)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum FuncType {
    Static(FuncId),
}

impl FuncType {
    pub fn unify(&self, other: &Self) -> Result<Self, UnificationError> {
        match (self, other) {
            (Self::Static(lhs), Self::Static(rhs)) if lhs == rhs => Err(UnificationError::Equal),
            _ => Err(UnificationError::Incompatible),
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
#[repr(u8)]
pub enum BuiltInType {
    Unknown,
    Module,
    Type,
    Unit,
    Bool,
    Integer,
    Uint,
    U8,
    U16,
    U32,
    U64,
    Int,
    I8,
    I16,
    I32,
    I64,
}

impl BuiltInType {
    pub const COUNT: usize = 16;
    pub const ALL: [Self; Self::COUNT] = [
        Self::Unknown,
        Self::Module,
        Self::Type,
        Self::Unit,
        Self::Bool,
        Self::Integer,
        Self::Uint,
        Self::U8,
        Self::U16,
        Self::U32,
        Self::U64,
        Self::Int,
        Self::I8,
        Self::I16,
        Self::I32,
        Self::I64,
    ];

    pub const fn from_index(index: usize) -> Result<Self, usize> {
        match index.checked_sub(Self::ALL.len()) {
            Some(index) => Err(index),
            None => Ok(Self::ALL[index]),
        }
    }

    pub fn unify(self, rhs: Self) -> Result<Self, UnificationError> {
        use BuiltInType::*;
        match (self, rhs) {
            (a, b) if a == b => Err(UnificationError::Equal),
            (Unknown, a) | (a, Unknown) => Ok(a),
            (Integer, a) | (a, Integer) if a.is_integer() => Ok(a),
            _ => Err(UnificationError::Incompatible),
        }
    }

    pub fn is_integer(self) -> bool {
        use BuiltInType::*;
        matches!(
            self,
            U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | Int | Uint
        )
    }

    pub fn is_signed(self) -> bool {
        use BuiltInType::*;
        matches!(self, I8 | I16 | I32 | I64 | Int)
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Unknown => "{unknown}",
            Self::Module => "module",
            Self::Type => "type",
            Self::Unit => "()",
            Self::Bool => "bool",
            Self::Integer => "{integer}",
            Self::Uint => "uint",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::Int => "int",
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
        }
    }
}

impl fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name().fmt(f)
    }
}

use std::{
    fmt,
    ops::{Index, IndexMut},
};

use mini_alloc::IdentStr;

use crate::{Const, DefaultRef, RecordKind, Ref, VecStore};

mod unify_impl;

pub type RecordRepr = u32;
pub type RecordInst = Ref<Record, RecordRepr>;

pub struct Record {
    pub fields: Vec<RecordField>,
}

pub struct RecordField {
    pub name: IdentStr,
    pub ty: Type,
}

#[derive(Default)]
pub struct Types {
    reords: VecStore<Record, RecordRepr>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create_record(&mut self) -> RecordInst {
        self.reords.push(Record { fields: Vec::new() })
    }
}

impl Index<RecordInst> for Types {
    type Output = Record;

    fn index(&self, index: RecordInst) -> &Self::Output {
        &self.reords[index]
    }
}

impl IndexMut<RecordInst> for Types {
    fn index_mut(&mut self, index: RecordInst) -> &mut Self::Output {
        &mut self.reords[index]
    }
}

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
    Record(RecordKind, RecordInst),
}

impl From<BuiltInType> for Type {
    fn from(b: BuiltInType) -> Self {
        Self::BuiltIn(b)
    }
}

impl Type {
    pub const UNIT: Self = Self::BuiltIn(BuiltInType::Unit);

    pub fn unify(&self, other: &Self) -> Result<Self, UnificationError> {
        match (self, other) {
            (&Self::BuiltIn(lhs), &Self::BuiltIn(rhs)) => lhs.unify(rhs).map(Self::BuiltIn),
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
            Self::Record(kind, id) => write!(f, "{:?}#{}", kind, id.index()),
        }
    }
}

impl DefaultRef for Type {
    fn default_ref<R: crate::RefRepr>() -> crate::Ref<Self, R> {
        crate::Ref::from_repr(R::MIN)
    }
}

macro_rules! gen_builtin {
    ($($name:ident = $display:literal)*) => {
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        #[repr(u8)]
        pub enum BuiltInType {$( $name, )*}


        impl BuiltInType {
            pub const COUNT: usize = [$( BuiltInType::$name, )*].len();
            pub const ALL: [Self; Self::COUNT] = [$( BuiltInType::$name, )*];

            pub fn name(self) -> &'static str {
                match self {
                    $( Self::$name => $display, )*
                }
            }
        }
    }
}

gen_builtin! {
    Unknown = "{unknown}"
    Never = "never"
    Module = "module"
    Def = "def"
    Type = "type"
    Unit = "unit"
    Bool = "bool"
    Integer = "{integer}"
    Uint = "uint"
    U8 = "u8"
    U16 = "u16"
    U32 = "u32"
    U64 = "u64"
    Int = "int"
    I8 = "i8"
    I16 = "i16"
    I32 = "i32"
    I64 = "i64"
}

impl BuiltInType {
    pub const fn from_index(index: usize) -> Result<Self, usize> {
        match index.checked_sub(Self::ALL.len()) {
            Some(index) => Err(index),
            None => Ok(Self::ALL[index]),
        }
    }

    pub fn into_ref(self) -> Ref<Type, u16> {
        Ref::from_repr(self as _)
    }

    pub fn unify(self, rhs: Self) -> Result<Self, UnificationError> {
        use BuiltInType::*;
        match (self, rhs) {
            (a, b) if a == b => Err(UnificationError::Equal),
            (Unknown, a) | (a, Unknown) => Ok(a),
            (Never, a) | (a, Never) => Ok(a),
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

    pub fn from_const(c: &Const) -> BuiltInType {
        match c {
            Const::Bool(_) => Self::Bool,
            Const::Int(_) => Self::Integer,
        }
    }
}
impl fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name().fmt(f)
    }
}

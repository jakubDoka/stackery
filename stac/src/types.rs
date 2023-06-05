use mini_alloc::*;

use crate::*;

mod checker_impl;

pub struct GeneratorCtx<'a> {
    types: &'a Types,
    files: &'a Files,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
}

pub struct GeneratorArgs<'a> {
    instance: FuncInstId<Type>,
    func_view: FuncMetaView<'a>,
    types: &'a [Type],
    unknowns: &'a Unknowns,
}

pub trait Generator {
    fn generate(&mut self, ctx: GeneratorCtx, args: GeneratorArgs);
}

pub struct TypeChecker<'a> {
    types: &'a Types,
    instrs: &'a Instrs,
    modules: &'a Modules,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        types: &'a Types,
        instrs: &'a Instrs,
        modules: &'a Modules,
        interner: &'a StrInterner,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            types,
            instrs,
            modules,
            interner,
            diags: diagnostics,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncInstId<T> {
    id: FuncId,
    ty: FuncType<T>,
}

impl FuncInstId<Type> {
    fn downgrade(&self, types: &Types) -> FuncInstId<TypeValue> {
        FuncInstId {
            id: self.id,
            ty: self.ty.downgrade(types),
        }
    }
}

impl FuncInstId<TypeValue> {
    fn upgrade(&self, types: &Types, unknowns: &Unknowns) -> Result<FuncInstId<Type>, UnknownType> {
        Ok(FuncInstId {
            id: self.id,
            ty: self.ty.upgrade(types, unknowns)?,
        })
    }
}

pub struct NameSet(InternedSlice<InternedStr>);

pub struct Types {
    names: Interner<InternedStr>,
    types: Interner<Type>,
    values: Interner<TypeValue>,
}
impl Types {
    fn upgrade_slice(
        &self,
        slice: InternedSlice<TypeValue>,
        unknowns: &Unknowns,
    ) -> Result<InternedSlice<Type>, UnknownType> {
        self.types.try_intern_iter(
            self.values[slice]
                .iter()
                .map(|&t| t.upgrade(self, unknowns)),
        )
    }

    fn upgrade(
        &self,
        ty: Interned<TypeValue>,
        unknowns: &Unknowns,
    ) -> Result<Interned<Type>, UnknownType> {
        self.values[ty]
            .upgrade(self, unknowns)
            .map(|t| self.types.intern(t))
    }

    fn downgrade_slice(&self, args: InternedSlice<Type>) -> InternedSlice<TypeValue> {
        self.values
            .intern_iter(self.types[args].iter().map(|&t| t.downgrade()))
    }

    fn downgrade(&self, ret: Interned<Type>) -> Interned<TypeValue> {
        self.values.intern(self.types[ret].downgrade())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Complex(ComplexType<Type>),
    Leaf(LeafType),
}

impl Type {
    fn downgrade(self) -> TypeValue {
        TypeValue::Leaf(self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ComplexType<T> {
    Struct(StructType<T>),
    Enum(EnumType<T>),
    Array(ArrayType<T>),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum LeafType {
    Builtin(BuiltinType),
    Module(ModuleRef),
    Func(FuncId),
    Type,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Int(IntType),
    Float,
    Bool,
    Char,
    Str,
    Unit,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct IntType {
    signed: bool,
    width: IntWidth,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum IntWidth {
    W8,
    W16,
    W32,
    W64,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructType<T> {
    names: InternedSlice<InternedStr>,
    types: InternedSlice<T>,
}

impl<T> StructType<T> {
    pub fn names(self, types: &Types) -> &[InternedStr] {
        &types.names[self.names]
    }
}

impl StructType<TypeValue> {
    fn upgrade(&self, types: &Types, unknowns: &Unknowns) -> Result<StructType<Type>, UnknownType> {
        Ok(StructType {
            names: self.names,
            types: types.upgrade_slice(self.types, unknowns)?,
        })
    }
}

impl StructType<Type> {
    pub fn types(self, types: &Types) -> &[Type] {
        &types.types[self.types]
    }
}

pub type EnumType<T> = StructType<T>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType<T> {
    ty: Interned<T>,
    len: u32,
}

impl<T> ArrayType<T> {
    pub fn len(self) -> u32 {
        self.len
    }
}

impl ArrayType<TypeValue> {
    fn upgrade(&self, types: &Types, unknowns: &Unknowns) -> Result<ArrayType<Type>, UnknownType> {
        Ok(ArrayType {
            ty: types.upgrade(self.ty, unknowns)?,
            len: self.len,
        })
    }
}

impl ArrayType<Type> {
    pub fn ty(self, types: &Types) -> Type {
        types.types[self.ty]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncType<T> {
    args: InternedSlice<T>,
    ret: Interned<T>,
}

impl FuncType<Type> {
    pub fn args(self, types: &Types) -> &[Type] {
        &types.types[self.args]
    }

    pub fn ret(self, types: &Types) -> Type {
        types.types[self.ret]
    }

    fn downgrade(&self, types: &Types) -> FuncType<TypeValue> {
        FuncType {
            args: types.downgrade_slice(self.args),
            ret: types.downgrade(self.ret),
        }
    }
}

impl FuncType<TypeValue> {
    fn upgrade(&self, types: &Types, unknowns: &Unknowns) -> Result<FuncType<Type>, UnknownType> {
        Ok(FuncType {
            args: types.upgrade_slice(self.args, unknowns)?,
            ret: types.upgrade(self.ret, unknowns)?,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct UnknownType(u32);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum TypeValue {
    Unknown(UnknownType),
    Complex(ComplexType<TypeValue>),
    Leaf(Type),
}

impl TypeValue {
    fn upgrade(self, types: &Types, unknowns: &Unknowns) -> Result<Type, UnknownType> {
        unknowns.resolve(self, types)
    }
}

#[derive(Default)]
struct Unknowns {
    slots: Vec<Option<TypeValue>>,
}

impl Unknowns {
    fn new() -> Self {
        Self::default()
    }

    fn next(&mut self) -> UnknownType {
        let id = self.slots.len() as u32;
        self.slots.push(None);
        types::UnknownType(id)
    }

    fn set(&mut self, id: UnknownType, ty: Type) -> Option<TypeValue> {
        self.slots[id.0 as usize].replace(ty.downgrade())
    }

    fn start_frame(&mut self) -> UnknownFrame {
        UnknownFrame(self.slots.len())
    }

    fn end_frame(&mut self, frame: UnknownFrame) {
        self.slots.truncate(frame.0);
    }

    fn resolve(&self, value: TypeValue, types: &Types) -> Result<Type, UnknownType> {
        match value {
            TypeValue::Unknown(UnknownType(index)) => self.slots[index as usize]
                .as_ref()
                .map(|ty| ty.upgrade(types, self))
                .unwrap_or(Err(UnknownType(index))),
            TypeValue::Complex(c) => match c {
                ComplexType::Struct(s) => s.upgrade(types, self).map(ComplexType::Struct),
                ComplexType::Enum(e) => e.upgrade(types, self).map(ComplexType::Enum),
                ComplexType::Array(a) => a.upgrade(types, self).map(ComplexType::Array),
            }
            .map(Type::Complex),
            TypeValue::Leaf(ty) => Ok(ty),
        }
    }
}

struct UnknownFrame(usize);

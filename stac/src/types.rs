use mini_alloc::{Interned, InternedSlice, InternedStr, Interner};

pub struct NameSet(InternedSlice<InternedStr>);

#[derive(Default)]
pub struct Types {
    names: Interner<InternedStr>,
    types: Interner<Type>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern_enum(&self, variants: &mut [(InternedStr, Type)]) -> EnumType {
        let (names, types) = self.intern_type_set(variants);
        EnumType { names, types }
    }

    pub fn intern_struct(&self, fields: &mut [(InternedStr, Type)]) -> StructType {
        let (names, types) = self.intern_type_set(fields);
        StructType { names, types }
    }

    pub fn create_name_set(&self, names: &mut [InternedStr]) -> NameSet {
        names.sort_unstable();
        let names = self.names.intern_slice(&*names);
        NameSet(names)
    }

    fn intern_type_set(
        &self,
        elems: &mut [(InternedStr, Type)],
    ) -> (InternedSlice<InternedStr>, InternedSlice<Type>) {
        elems.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

        let names = self.names.intern_iter(elems.iter().map(|&(name, _)| name));
        let types = self.types.intern_iter(elems.iter().map(|&(_, ty)| ty));

        (names, types)
    }

    pub fn intern_func(&self, args: &[Type], ret: Type) -> FuncType {
        let args = self.types.intern_slice(args);
        let ret = self.types.intern(ret);
        FuncType { args, ret }
    }

    pub fn intern_array(&self, ty: Type, len: u32) -> ArrayType {
        let ty = self.types.intern(ty);
        ArrayType { ty, len }
    }
}

#[derive(Default)]
pub struct Unknowns {
    slots: Vec<Option<Type>>,
}

impl Unknowns {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> UnknownType {
        let id = self.slots.len() as u32;
        self.slots.push(None);
        UnknownType(id)
    }

    pub fn set(&mut self, id: UnknownType, ty: Type) -> Option<Type> {
        self.slots[id.0 as usize].replace(ty)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Builtin(BuiltinType),
    Struct(StructType),
    Enum(EnumType),
    Array(ArrayType),
    Func(FuncType),
    Module(InternedStr),
    Unknown(UnknownType),
    Type,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct UnknownType(u32);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Int,
    Float,
    Bool,
    Char,
    Str,
    Unit,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    names: InternedSlice<InternedStr>,
    types: InternedSlice<Type>,
}

impl StructType {
    pub fn names(self, types: &Types) -> &[InternedStr] {
        &types.names[self.names]
    }

    pub fn types(self, types: &Types) -> &[Type] {
        &types.types[self.types]
    }
}

pub type EnumType = StructType;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    ty: Interned<Type>,
    len: u32,
}

impl ArrayType {
    pub fn ty(self, types: &Types) -> Type {
        types.types[self.ty]
    }

    pub fn len(self) -> u32 {
        self.len
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    args: InternedSlice<Type>,
    ret: Interned<Type>,
}

impl FuncType {
    pub fn args(self, types: &Types) -> &[Type] {
        &types.types[self.args]
    }

    pub fn ret(self, types: &Types) -> Type {
        types.types[self.ret]
    }
}

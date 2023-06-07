use crate::{FuncId, ModuleRef};
use core::fmt;
use mini_alloc::{InternedStr, StrInterner};
use std::{ops::Deref, rc::Rc};

mod checker_impl;

#[derive(Clone, PartialEq, Eq, Hash)]
struct FuncInstanceId {
    func: FuncId,
    types: Rc<[Type]>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum Type {
    Leaf(LeafType),
    Branch(BranchType<Type>),
}

impl Type {
    fn from_value(value: TypeValue, inferred: &[Option<TypeValue>]) -> Result<Self, InferredType> {
        match value {
            TypeValue::Leaf(leaf) => Ok(Self::Leaf(leaf)),
            TypeValue::Branch(branch) => match branch {
                BranchType::Enum(e) | BranchType::Struct(e) => Ok(Self::Branch(BranchType::Enum(
                    e.into_iter()
                        .map(|(name, ty)| {
                            Type::from_value(ty.clone(), inferred).map(|ty| (*name, ty))
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into(),
                ))),
                BranchType::Array {
                    item_type,
                    item_count,
                } => Ok(Self::Branch(BranchType::Array {
                    item_type: Rc::new(Type::from_value(item_type.deref().clone(), inferred)?),
                    item_count,
                })),
            },
            TypeValue::Inferred(i) => {
                let Some(pointed) = inferred[i.0 as usize].clone() else {
                    return Err(i);
                };

                Type::from_value(pointed, inferred)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum TypeValue {
    Leaf(LeafType),
    Branch(BranchType<TypeValue>),
    Inferred(InferredType),
}

impl TypeValue {
    fn combine(self, other: Self, inferred: &mut [Option<Self>]) -> Result<Self, (Self, Self)> {
        match (self, other) {
            (a, b) if a == b => Ok(a),
            (TypeValue::Branch(a), TypeValue::Branch(b)) => match (a, b) {
                (BranchType::Enum(a), BranchType::Enum(b)) => {
                    let mut merged = a.iter().chain(b.iter()).cloned().collect::<Vec<_>>();
                    merged.sort_by_key(|&(name, _)| name);
                    if merged.group_by(|a, b| a.0 == b.0).any(|g| g.len() > 1) {
                        return Err((
                            TypeValue::Branch(BranchType::Enum(a)),
                            TypeValue::Branch(BranchType::Enum(b)),
                        ));
                    }
                    merged.dedup_by_key(|&mut (name, _)| name);
                    Ok(TypeValue::Branch(BranchType::Enum(merged.into())))
                }
                (BranchType::Struct(a), BranchType::Struct(b)) => {
                    if a.iter().zip(b.iter()).any(|(a, b)| a.0 != b.0) {
                        return Err((
                            TypeValue::Branch(BranchType::Struct(a)),
                            TypeValue::Branch(BranchType::Struct(b)),
                        ));
                    }

                    let merged = a
                        .iter()
                        .zip(b.iter())
                        .map(|(a, b)| a.1.clone().combine(b.1.clone(), inferred).map(|t| (a.0, t)))
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(TypeValue::Branch(BranchType::Struct(merged.into())))
                }
                (
                    BranchType::Array {
                        item_type: a,
                        item_count: a_count,
                    },
                    BranchType::Array {
                        item_type: b,
                        item_count: b_count,
                    },
                ) => {
                    if a_count != b_count {
                        return Err((
                            TypeValue::Branch(BranchType::Array {
                                item_type: a,
                                item_count: a_count,
                            }),
                            TypeValue::Branch(BranchType::Array {
                                item_type: b,
                                item_count: b_count,
                            }),
                        ));
                    }

                    Ok(TypeValue::Branch(BranchType::Array {
                        item_type: a
                            .deref()
                            .clone()
                            .combine(b.deref().clone(), inferred)?
                            .into(),
                        item_count: a_count,
                    }))
                }
                (a, b) => Err((TypeValue::Branch(a), TypeValue::Branch(b))),
            },
            (Self::Inferred(a), Self::Inferred(b)) => {
                match (
                    inferred[a.0 as usize].clone(),
                    inferred[b.0 as usize].clone(),
                ) {
                    (Some(a), Some(b)) => a.combine(b, inferred),
                    (Some(ia), None) => {
                        inferred[b.0 as usize] = Some(ia.clone());
                        Ok(ia)
                    }
                    (None, Some(ib)) => {
                        inferred[a.0 as usize] = Some(ib.clone());
                        Ok(ib)
                    }
                    (None, None) => {
                        inferred[a.0 as usize] = Some(Self::Inferred(b));
                        Ok(Self::Inferred(b))
                    }
                }
            }
            (Self::Inferred(a), b) => match inferred[a.0 as usize].clone() {
                Some(a) => a.combine(b, inferred),
                None => {
                    inferred[a.0 as usize] = Some(b.clone());
                    Ok(b)
                }
            },
            (a, Self::Inferred(b)) => match inferred[b.0 as usize].clone() {
                Some(b) => a.combine(b, inferred),
                None => {
                    inferred[b.0 as usize] = Some(a.clone());
                    Ok(a)
                }
            },
            e => Err(e),
        }
    }

    fn display<'a>(&'a self, interner: &'a StrInterner) -> impl fmt::Display + 'a {
        struct Displayer<'a> {
            root: &'a TypeValue,
            interner: &'a StrInterner,
        }

        fn display(
            root: &TypeValue,
            depth: usize,
            interner: &StrInterner,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            match root {
                TypeValue::Leaf(l) => match l {
                    LeafType::Builitin(b) => match b {
                        BuiltinType::Int => write!(f, "int"),
                        BuiltinType::Bool => write!(f, "bool"),
                        BuiltinType::Unit => write!(f, "unit"),
                    },
                    LeafType::Func(func) => {
                        write!(f, "m{}.f{}", func.module.index(), func.func.index())
                    }
                    LeafType::Module(m) => write!(f, "m{}", m.index()),
                },
                TypeValue::Branch(b) => match b {
                    BranchType::Enum(e) => {
                        write!(f, "|{{")?;
                        for (i, &(name, ref t)) in e.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            for _ in 0..depth {
                                write!(f, "  ")?;
                            }
                            write!(f, "{}: ", &interner[name])?;
                            display(t.deref(), depth, interner, f)?;
                        }
                        write!(f, "}}")
                    }
                    BranchType::Struct(s) => {
                        write!(f, "*{{")?;
                        for (i, &(name, ref t)) in s.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            for _ in 0..depth {
                                write!(f, "  ")?;
                            }
                            write!(f, "{}: ", &interner[name])?;
                            display(t.deref(), depth, interner, f)?;
                        }
                        write!(f, "}}")
                    }
                    BranchType::Array {
                        item_type,
                        item_count,
                    } => {
                        write!(f, "[")?;
                        display(item_type.deref(), depth, interner, f)?;
                        write!(f, "; {}]", item_count)
                    }
                },
                TypeValue::Inferred(_) => todo!(),
            }
        }

        impl fmt::Display for Displayer<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                display(self.root, 0, self.interner, f)
            }
        }

        Displayer {
            root: self,
            interner,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum BranchType<T> {
    Enum(Rc<[(InternedStr, T)]>),
    Struct(Rc<[(InternedStr, T)]>),
    Array { item_type: Rc<T>, item_count: usize },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum LeafType {
    Builitin(BuiltinType),
    Func(FuncId),
    Module(ModuleRef),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BuiltinType {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct InferredType(u16);

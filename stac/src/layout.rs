use std::mem;

use crate::{BuiltInType, Type};

type LayoutRepr = u32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Layout {
    repr: LayoutRepr,
}

impl Layout {
    const ALIGN_WIDTH: usize = 2;
    const SIZE_WIDTH: usize = mem::size_of::<LayoutRepr>() * 8 - Self::ALIGN_WIDTH;
    const SIZE_MASK: LayoutRepr = (1 << Self::SIZE_WIDTH) - 1;

    pub const ARCH_16: Self = Self::new(1, 2);
    pub const ARCH_32: Self = Self::new(2, 4);
    pub const ARCH_64: Self = Self::new(3, 8);
    pub const ZERO: Self = Self { repr: 0 };

    const fn new(align_pow: u8, size: LayoutRepr) -> Self {
        Self {
            repr: Self::encode(align_pow, size),
        }
    }

    pub fn align(&self) -> u8 {
        Self::decode(self.repr).0
    }

    pub fn size(&self) -> LayoutRepr {
        Self::decode(self.repr).1
    }

    /// extend this layout with another layout, returns the new layout and appended layout offset
    fn _extend(self, other: Self) -> (Self, LayoutRepr) {
        let (align, size) = Self::decode(self.repr);
        let (other_align, other_size) = Self::decode(other.repr);

        let align = align.max(other_align);

        let exp_other_align = Self::expand_align(other_align);
        let padding = exp_other_align - (size & exp_other_align - 1);
        let offset = size + padding;
        let size = offset + other_size;

        (Self::new(align, size), padding)
    }

    fn decode(repr: LayoutRepr) -> (u8, LayoutRepr) {
        let align = repr >> Self::SIZE_WIDTH;
        let size = repr & Self::SIZE_MASK;
        (align as u8, size)
    }

    const fn encode(align: u8, size: LayoutRepr) -> LayoutRepr {
        ((align as LayoutRepr) << Self::SIZE_WIDTH) | (size & Self::SIZE_MASK)
    }

    fn expand_align(align: u8) -> LayoutRepr {
        1 << align
    }

    fn _compress_align(align: LayoutRepr) -> u8 {
        align.trailing_zeros() as u8
    }

    pub fn from_ty(ty: &Type, ptr_layout: Self) -> Self {
        match ty {
            Type::BuiltIn(b) => Self::from_builtin_ty(*b, ptr_layout),
            Type::Func(_) => todo!(),
        }
    }

    fn from_builtin_ty(bty: BuiltInType, ptr_layout: Self) -> Self {
        use BuiltInType::*;
        match bty {
            U8 | I8 => Self::new(0, 1),
            U16 | I16 => Self::new(1, 2),
            U32 | I32 => Self::new(2, 4),
            U64 | I64 => Self::new(3, 8),
            Int | Uint => ptr_layout,
            Bool => Self::new(0, 1),
            Unit => Self::new(0, 0),
            Type | Integer | Module | Unknown => unreachable!(),
        }
    }

    pub fn expanded_align(&self) -> u32 {
        Self::expand_align(self.align())
    }
}
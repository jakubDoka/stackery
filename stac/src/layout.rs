use std::mem;

use crate::{BuiltInType, FieldIndex, RecordKind, Type, Types};

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

    #[track_caller]
    pub fn field_offset(
        field: FieldIndex,
        ty: Type,
        ptr_layout: Self,
        types: &Types,
    ) -> LayoutRepr {
        let Type::Record(kind, id) = ty else {
            unreachable!("field_offset called on non-record type {ty:?}")
        };

        let record = &types[id];

        let layout = match kind {
            RecordKind::Prod => {
                let mut layout = Self::ZERO;
                for field in record.fields.iter().take(field as usize) {
                    let field_layout = Self::from_ty(&field.ty, ptr_layout, types);
                    (layout, _) = layout.extend(field_layout);
                }
                layout
            }
            RecordKind::Sum | RecordKind::Max => Self::ZERO,
        };

        let flag = match kind {
            RecordKind::Max | RecordKind::Prod => Layout::ZERO,
            RecordKind::Sum => Self::sun_flag_layout(record.fields.len()),
        };

        flag.extend(layout).0.size()
    }

    /// extend this layout with another layout, returns the new layout and appended layout offset
    fn extend(self, other: Self) -> (Self, LayoutRepr) {
        let (align, size) = Self::decode(self.repr);
        let (other_align, other_size) = Self::decode(other.repr);

        let align = align.max(other_align);

        let exp_other_align = Self::expand_align(other_align);
        let padding = exp_other_align - (size & exp_other_align - 1) & exp_other_align - 1;
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

    fn max(self, other: Self) -> Self {
        let (align, size) = Self::decode(self.repr);
        let (other_align, other_size) = Self::decode(other.repr);
        let align = align.max(other_align);
        let size = size.max(other_size);
        Self::new(align, size)
    }

    pub fn from_ty(ty: &Type, ptr_layout: Self, types: &Types) -> Self {
        match ty {
            Type::BuiltIn(b) => Self::from_builtin_ty(*b, ptr_layout),
            Type::Record(kind, id) => {
                let record = &types[*id];

                let layout = match kind {
                    RecordKind::Prod => {
                        let mut layout = Self::ZERO;
                        for field in &record.fields {
                            let field_layout = Self::from_ty(&field.ty, ptr_layout, types);
                            (layout, _) = layout.extend(field_layout);
                        }
                        layout
                    }
                    RecordKind::Sum | RecordKind::Max => {
                        let mut layout = Self::ZERO;
                        for varian in &record.fields {
                            let variant = Self::from_ty(&varian.ty, ptr_layout, types);
                            layout = layout.max(variant);
                        }
                        layout
                    }
                };

                let flag = match kind {
                    RecordKind::Max | RecordKind::Prod => Layout::ZERO,
                    RecordKind::Sum => Self::sun_flag_layout(record.fields.len()),
                };

                flag.extend(layout).0
            }
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
            Type | Def | Integer | Module | Unknown | Never => unreachable!(),
        }
    }

    pub fn expanded_align(&self) -> u32 {
        Self::expand_align(self.align())
    }

    pub fn takes_space(&self) -> bool {
        self.size() > 0
    }

    fn sun_flag_layout(count: usize) -> Self {
        match count {
            0 | 1 => Self::ZERO,
            2..=256 => Self::new(0, 1),
            257..=65536 => Self::new(1, 2),
            _ => Self::new(2, 4),
        }
    }

    pub fn fits_register(&self, arch: Layout) -> bool {
        self.size() <= arch.size()
    }
}

#[cfg(test)]
mod test {
    use super::Layout;

    #[test]
    fn sum_flag_layout_sanity_check() {
        let [unit, byte, short, int] = [
            Layout::ZERO,
            Layout::new(0, 1),
            Layout::new(1, 2),
            Layout::new(2, 4),
        ];

        assert_eq!(Layout::sun_flag_layout(1), unit);
        assert_eq!(Layout::sun_flag_layout(256), byte);
        assert_eq!(Layout::sun_flag_layout(257), short);
        assert_eq!(Layout::sun_flag_layout(65536), short);
        assert_eq!(Layout::sun_flag_layout(65537), int);
    }
}

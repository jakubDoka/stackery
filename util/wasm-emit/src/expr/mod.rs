use crate::*;

mod generated_instrs;

pub use generated_instrs::{MultiByteInstr, OneByteInstr};

#[macro_export]
macro_rules! gen_instrs {
    (
		one_bytes: [$($byte:literal => $name:ident)*],
		multy_bytes: [$($prefix:literal $id:literal => $mname:ident)*],
		others: [$(
			$leading_byte:literal => $fn_name:ident($($arg:ident: $ty:ty),*)
				{$($body:tt)*}
		)*],
	) => {
		#[derive(Clone, Copy)]
		pub enum OneByteInstr {
			$(
				$name = $byte,
			)*
		}

		#[derive(Clone, Copy)]
		pub enum MultiByteInstr {
			$(
				$mname = $id << 8 | $prefix,
			)*
		}

		impl<'a, B: super::Backend> super::ExprEncoder<'a, B> {
			$(
				pub fn $fn_name(&mut self, $($arg: $ty,)*) {
					self.inner.byte($leading_byte);
					$(
						self.inner.encode($body);
					)*
				}
			)*
		}
	};

	(@arg_pass $name:literal: $type:ty) => {};
	(@arg_pass $name:ident: $type:ty) => {$name: $type,};
}

pub struct ExprEncoder<'a, B: Backend> {
    inner: Encoder<'a, B>,
}

impl<'a, B: Backend> ExprEncoder<'a, B> {
    pub(super) const TERMINATOR: u8 = 0x0b;

    pub(super) fn inner(&mut self) -> Encoder<B> {
        self.inner.stack_borrow()
    }

    pub(super) fn new(encoder: Encoder<'a, B>) -> Self {
        Self { inner: encoder }
    }

    pub fn block(&mut self, ty: BlockType) -> ExprEncoder<'_, B> {
        self.inner.byte(0x02);
        self.inner.encode(ty);
        ExprEncoder::new(self.inner.stack_borrow())
    }

    pub fn r#loop(&mut self, ty: BlockType) -> ExprEncoder<'_, B> {
        self.inner.byte(0x03);
        self.inner.encode(ty);
        ExprEncoder::new(self.inner.stack_borrow())
    }

    pub fn r#if(&mut self, ty: BlockType) -> IfExprEncoder<'_, B> {
        self.inner.byte(0x04);
        self.inner.encode(ty);
        IfExprEncoder {
            inner: ExprEncoder::new(self.inner.stack_borrow()),
        }
    }

    pub fn br_table(&mut self, default: Labelidx) -> BrTableEncoder<'_, B> {
        self.inner.byte(0x0e);
        BrTableEncoder {
            inner: VecEncoder::new(self.inner.stack_borrow()),
            default,
        }
    }

    pub fn select_t(&mut self) -> VecEncoder<'_, Valtype, B> {
        self.inner.byte(0x1c);
        VecEncoder::new(self.inner.stack_borrow())
    }

    pub fn one_byte(&mut self, instr: OneByteInstr) {
        self.inner.byte(instr as u8);
    }

    pub fn multi_byte(&mut self, instr: MultiByteInstr) {
        self.inner.byte(instr as u8);
        self.inner.encode(instr as u32 >> 8);
    }

    pub fn finish(mut self) -> Encoder<'a, B> {
        self.inner.byte(Self::TERMINATOR);
        take_field_and_leak(self, |s| ptr::addr_of!(s.inner))
    }
}

impl<'a, B: Backend> Drop for ExprEncoder<'a, B> {
    fn drop(&mut self) {
        self.inner.byte(Self::TERMINATOR);
    }
}

pub struct BrTableEncoder<'a, B: Backend> {
    inner: VecEncoder<'a, Labelidx, B>,
    default: Labelidx,
}

impl<'a, B: Backend> Deref for BrTableEncoder<'a, B> {
    type Target = VecEncoder<'a, Labelidx, B>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for BrTableEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<B: Backend> Drop for BrTableEncoder<'_, B> {
    fn drop(&mut self) {
        self.inner.inner().encode(self.default);
    }
}

pub struct IfExprEncoder<'a, B: Backend> {
    inner: ExprEncoder<'a, B>,
}

impl<'a, B: Backend> IfExprEncoder<'a, B> {
    const ELSE_BYTE: u8 = 0x05;

    pub fn r#else(mut self) -> ExprEncoder<'a, B> {
        self.inner.inner.byte(Self::ELSE_BYTE);
        take_field_and_leak(self, |s| ptr::addr_of!(s.inner))
    }
}

impl<'a, B: Backend> Deref for IfExprEncoder<'a, B> {
    type Target = ExprEncoder<'a, B>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, B: Backend> DerefMut for IfExprEncoder<'a, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Clone, Copy)]
pub enum BlockType {
    Empty,
    Val(Valtype),
    Scalar(u64),
}

impl Sealed for BlockType {}
impl Encode for BlockType {
    fn encode<B: Backend>(self, mut encoder: Encoder<'_, B>) {
        match self {
            Self::Empty => encoder.byte(0x40),
            Self::Val(ty) => ty.encode(encoder),
            Self::Scalar(n) => n.encode(encoder),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Memarg {
    align_exponent: u32,
    offset: u32,
}

impl Memarg {
    pub fn new(align_exponent: u32, offset: u32) -> Self {
        Self {
            align_exponent,
            offset,
        }
    }
}

impl Sealed for Memarg {}
impl Encode for Memarg {
    fn encode<B: Backend>(self, mut encoder: Encoder<B>) {
        encoder.encode(self.align_exponent);
        encoder.encode(self.offset);
    }
}

pub type Laneidx = u8;

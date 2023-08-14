use std::array;

use cranelift_codegen::{
    ir::{self, InstBuilder},
    isa, Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;
use stac::{
    BuiltInType, Finst, FinstKind, Ir, IrTypes, Layout, Mutable, OpCode, SubsRef, Type,
    TypeRefIndex,
};

use super::*;

struct Res {
    ctx: Context,
    bctx: FunctionBuilderContext,
}

#[derive(Clone, Copy)]
enum Slot {
    Value(ir::Value),
    Var(Variable),
}

struct Builder<'ctx> {
    fb: FunctionBuilder<'ctx>,
    types: &'ctx IrTypes,
    arch: Layout,
    stack: Vec<(Slot, TypeSpec)>,
}

impl<'ctx> Builder<'ctx> {
    fn new(res: &'ctx mut Res, types: &'ctx IrTypes, arch: Layout) -> Self {
        Self {
            fb: FunctionBuilder::new(&mut res.ctx.func, &mut res.bctx),
            types,
            arch,
            stack: vec![],
        }
    }

    fn spec(&mut self, ty: SubsRef) -> TypeSpec {
        let ty = self.types.get_ty(ty);
        TypeSpec::from_ty(&ty, self.arch)
    }

    fn slot_as_value(&mut self, slot: Slot) -> ir::Value {
        match slot {
            Slot::Value(v) => v,
            Slot::Var(v) => self.fb.use_var(v),
        }
    }
}

impl<'ctx> Emmiter<'ctx> {
    pub fn emit(self, entry: &stac::Entry, ir: Ir) -> CodeRef {
        let mut res = Res {
            ctx: Context::new(),
            bctx: FunctionBuilderContext::new(),
        };

        res.ctx.func.signature = self.load_signature(&entry);
        // TODO: push params

        let mut builder = Builder::new(&mut res, &ir.types, self.arch);
        let entry = builder.fb.create_block();
        builder.fb.switch_to_block(entry);

        for &Finst { ref kind, ty, .. } in ir.instrs.iter() {
            match kind {
                &FinstKind::Sym(sym) => {
                    let slot = builder.stack[sym as usize];
                    builder.stack.push(slot);
                }
                FinstKind::Const(c) => match c {
                    stac::LitKindAst::Int(i) => {
                        let spec = builder.spec(ty);
                        let value = builder.fb.ins().iconst(spec.repr, i.value() as i64);
                        builder.stack.push((Slot::Value(value), spec));
                    }
                },
                FinstKind::BinOp(op) => {
                    let [(lhs, lty), (rhs, _)] = pop_array(&mut builder.stack);

                    use ir::types::*;
                    use OpCode::*;
                    match (op, lty.repr) {
                        (stac::op_group!(math), I8 | I16 | I32 | I64) => {
                            let rhs = builder.slot_as_value(rhs);
                            let lhs = builder.slot_as_value(lhs);
                            let value = match op {
                                Mul => builder.fb.ins().imul(lhs, rhs),
                                Div if lty.signed => builder.fb.ins().sdiv(lhs, rhs),
                                Div => builder.fb.ins().udiv(lhs, rhs),
                                Mod if lty.signed => builder.fb.ins().srem(lhs, rhs),
                                Mod => builder.fb.ins().urem(lhs, rhs),
                                Add => builder.fb.ins().iadd(lhs, rhs),
                                Sub => builder.fb.ins().isub(lhs, rhs),
                                Shl => builder.fb.ins().ishl(lhs, rhs),
                                Shr if lty.signed => builder.fb.ins().sshr(lhs, rhs),
                                Shr => builder.fb.ins().ushr(lhs, rhs),
                                BitAnd => builder.fb.ins().band(lhs, rhs),
                                BitOr => builder.fb.ins().bor(lhs, rhs),
                                BitXor => builder.fb.ins().bxor(lhs, rhs),
                                _ => unreachable!(),
                            };
                            builder.stack.push((Slot::Value(value), lty));
                        }
                        (stac::op_group!(assign), _) => {
                            let Slot::Var(var) = lhs else {
                                unreachable!();
                            };
                            let rhs = builder.slot_as_value(rhs);
                            builder.fb.def_var(var, rhs);
                        }
                        _ => todo!(),
                    }
                }
                FinstKind::Call(_) => {
                    todo!()
                }
                FinstKind::Field(_) => todo!(),
                FinstKind::Decl(true) => {
                    let (slot, spec) = builder.stack.pop().unwrap();
                    let value = builder.slot_as_value(slot);
                    let var = Variable::from_u32(builder.stack.len() as u32);
                    builder.fb.declare_var(var, spec.repr);
                    builder.fb.def_var(var, value);
                    builder.stack.push((Slot::Var(var), spec));
                }
                FinstKind::Decl(_) => (),
                FinstKind::Drop => {
                    builder.stack.pop().unwrap();
                }
                &FinstKind::DropScope(size, returns) => {
                    let return_slot = returns.then(|| builder.stack.pop().unwrap());
                    builder.stack.drain(builder.stack.len() - size as usize..);
                    if let Some((slot, spec)) = return_slot {
                        let value = builder.slot_as_value(slot);
                        builder.stack.push((Slot::Value(value), spec));
                    }
                }
            }
        }

        if let Some((slot, _)) = builder.stack.pop() {
            let value = builder.slot_as_value(slot);
            builder.fb.ins().return_(&[value]);
        } else {
            builder.fb.ins().return_(&[]);
        }

        builder.fb.seal_block(entry);

        builder.fb.finalize();

        let id = self
            .gen
            .module
            .declare_function(
                "main",
                cranelift_module::Linkage::Export,
                &res.ctx.func.signature,
            )
            .unwrap();

        self.gen.module.define_function(id, &mut res.ctx).unwrap();

        CodeRef::from_repr(0)
    }

    pub fn load_signature(&self, entry: &stac::Entry) -> ir::Signature {
        ir::Signature {
            params: entry
                .inputs
                .iter()
                .filter_map(|ty| self.load_local(ty))
                .map(ir::AbiParam::new)
                .collect(),
            returns: vec![ir::AbiParam::new(ir::types::I32)],
            call_conv: isa::CallConv::SystemV,
        }
    }

    fn load_local(&self, local: &stac::Local) -> Option<ir::Type> {
        match local {
            stac::Local::Ct(_) => None,
            stac::Local::Rt(ty) => Some(TypeSpec::from_ty(ty, self.arch).repr),
        }
    }
}

#[derive(Clone, Copy)]
struct TypeSpec {
    signed: bool,
    repr: ir::Type,
    _layout: Layout,
}

impl TypeSpec {
    fn from_ty(ty: &Type, arch: Layout) -> Self {
        let ptr_ty = Self::arch_layout_repr(arch);
        let layout = Layout::from_ty(ty, arch);
        let repr = Self::type_repr(ty, ptr_ty);
        let signed = ty.is_signed();
        Self {
            repr,
            _layout: layout,
            signed,
        }
    }

    fn type_repr(ty: &Type, arch: ir::Type) -> ir::Type {
        match ty {
            Type::BuiltIn(b) => Self::built_in_type_repr(b, arch),
            Type::Func(_) => arch,
        }
    }

    fn built_in_type_repr(b: &BuiltInType, arch: ir::Type) -> ir::Type {
        use BuiltInType::*;
        match b {
            U8 | I8 => ir::types::I8,
            U16 | I16 => ir::types::I16,
            U32 | I32 => ir::types::I32,
            U64 | I64 => ir::types::I64,
            Int | Uint => arch,
            Bool => ir::types::I8,
            Unit => ir::types::INVALID,
            Type => unreachable!(),
            Module => unreachable!(),
            Unknown => unreachable!(),
            Integer => unreachable!(),
        }
    }

    fn arch_layout_repr(value: Layout) -> ir::Type {
        match value.size() {
            2 => ir::types::I16,
            4 => ir::types::I32,
            8 => ir::types::I64,
            _ => unreachable!(),
        }
    }

    fn _invalid() -> TypeSpec {
        TypeSpec {
            repr: ir::types::INVALID,
            _layout: Layout::ZERO,
            signed: false,
        }
    }
}

fn pop_array<T, const N: usize>(vec: &mut Vec<T>) -> [T; N] {
    let mut iter = vec.drain(vec.len() - N..);
    array::from_fn(|_| iter.next().unwrap())
}

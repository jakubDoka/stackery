use std::array;

use cranelift_codegen::{
    entity::SecondaryMap,
    ir::{self, condcodes::IntCC, InstBuilder},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;
use stac::{
    BuiltInType, Finst, FinstKind, Ir, IrTypes, Layout, OpCode, Severty, Type, TypeRef, Types,
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

struct If {
    returns: bool,
    end: ir::Block,
}

#[derive(Default, Clone)]
struct Block {
    has_back_refs: bool,
}

struct Builder<'ctx> {
    fb: FunctionBuilder<'ctx>,
    current_block: ir::Block,
    types: &'ctx IrTypes,
    arch: Layout,
    stack: Vec<(Slot, TypeSpec)>,
    ifs: Vec<If>,
    blocks: SecondaryMap<ir::Block, Block>,
    used_fns: Vec<(FuncInst, ir::FuncRef)>,
}

impl<'ctx> Builder<'ctx> {
    fn new(
        res: &'ctx mut Res,
        types: &'ctx IrTypes,
        arch: Layout,
        arg_specs: impl Iterator<Item = TypeSpec>,
    ) -> Self {
        let mut fb = FunctionBuilder::new(&mut res.ctx.func, &mut res.bctx);
        let current_block = fb.create_block();
        let mut stack = vec![];

        fb.append_block_params_for_function_params(current_block);
        for (&param, spec) in fb.block_params(current_block).iter().zip(arg_specs) {
            stack.push((Slot::Value(param), spec));
        }
        fb.switch_to_block(current_block);

        Self {
            fb,
            current_block,
            types,
            arch,
            stack,
            ifs: vec![],
            blocks: SecondaryMap::new(),
            used_fns: vec![],
        }
    }

    fn get_used_fn(&mut self, id: FuncInst) -> Option<ir::FuncRef> {
        self.used_fns
            .iter()
            .find_map(|(i, f)| (*i == id).then_some(*f))
    }

    fn push_used_fn(&mut self, id: FuncInst, f: ir::FuncRef) {
        self.used_fns.push((id, f));
    }

    fn spec(&mut self, ty: TypeRef, types: &Types) -> TypeSpec {
        let ty = match BuiltInType::from_index(ty.index()) {
            Ok(ty) => ty.into(),
            Err(index) => self.types[TypeRef::from_repr(index as _)].clone(),
        };
        TypeSpec::from_ty(&ty, self.arch, types)
    }

    fn slot_as_value(&mut self, slot: Slot) -> ir::Value {
        match slot {
            Slot::Value(v) => v,
            Slot::Var(v) => self.fb.use_var(v),
        }
    }

    fn pop_as_value(&mut self) -> (ir::Value, TypeSpec) {
        let (slot, spec) = self.stack.pop().unwrap();
        (self.slot_as_value(slot), spec)
    }

    fn switch_to_block(&mut self, block: ir::Block) {
        if !self.blocks[self.current_block].has_back_refs {
            self.fb.seal_block(self.current_block);
        }
        self.current_block = block;
        self.fb.switch_to_block(block);
    }

    fn finalize(mut self) {
        self.fb.seal_block(self.current_block);
        self.fb.finalize();
    }
}

impl<'ctx> Emmiter<'ctx> {
    pub fn emit(self, entry: &stac::Entry, id: FuncInst, ir: Ir) -> CodeRef {
        let mut res = Res {
            ctx: Context::new(),
            bctx: FunctionBuilderContext::new(),
        };

        res.ctx.func.signature = self.load_signature(&entry);

        let arg_specs = entry.inputs.iter().filter_map(|ty| self.load_local(ty));
        let mut builder = Builder::new(&mut res, &ir.types, self.arch, arg_specs);

        for &Finst { ref kind, ty, .. } in ir.instrs.iter() {
            match kind {
                &FinstKind::Sym(sym) => {
                    let slot = builder.stack[sym as usize];
                    builder.stack.push(slot);
                }
                FinstKind::Const(c) => match c {
                    stac::LitKindAst::Int(i) => {
                        let spec = builder.spec(ty, self.types);
                        let value = builder.fb.ins().iconst(spec.repr, i.value() as i64);
                        builder.stack.push((Slot::Value(value), spec));
                    }
                    &stac::LitKindAst::Bool(b) => {
                        let spec = builder.spec(ty, self.types);
                        let value = builder.fb.ins().iconst(spec.repr, b as i64);
                        builder.stack.push((Slot::Value(value), spec));
                    }
                },
                FinstKind::BinOp(op) => {
                    let [(rhs, rty), (lhs, _)] = pop_array(&mut builder.stack);

                    use ir::types::*;
                    use OpCode::*;
                    match (op, rty.repr) {
                        (stac::op_group!(math), I8 | I16 | I32 | I64) => {
                            let rhs = builder.slot_as_value(rhs);
                            let lhs = builder.slot_as_value(lhs);
                            let value = match op {
                                Mul => builder.fb.ins().imul(lhs, rhs),
                                Div if rty.signed => builder.fb.ins().sdiv(lhs, rhs),
                                Div => builder.fb.ins().udiv(lhs, rhs),
                                Mod if rty.signed => builder.fb.ins().srem(lhs, rhs),
                                Mod => builder.fb.ins().urem(lhs, rhs),
                                Add => builder.fb.ins().iadd(lhs, rhs),
                                Sub => builder.fb.ins().isub(lhs, rhs),
                                Shl => builder.fb.ins().ishl(lhs, rhs),
                                Shr if rty.signed => builder.fb.ins().sshr(lhs, rhs),
                                Shr => builder.fb.ins().ushr(lhs, rhs),
                                BitAnd => builder.fb.ins().band(lhs, rhs),
                                BitOr => builder.fb.ins().bor(lhs, rhs),
                                BitXor => builder.fb.ins().bxor(lhs, rhs),
                                _ => unreachable!(),
                            };
                            builder.stack.push((Slot::Value(value), rty));
                        }
                        (stac::op_group!(assign), _) => {
                            let Slot::Var(var) = lhs else {
                                unreachable!();
                            };
                            let rhs = builder.slot_as_value(rhs);
                            builder.fb.def_var(var, rhs);
                        }
                        (stac::op_group!(cmp), I8 | I16 | I32 | I64) => {
                            let rhs = builder.slot_as_value(rhs);
                            let lhs = builder.slot_as_value(lhs);
                            let strategy = match op {
                                Eq => IntCC::Equal,
                                Ne => IntCC::NotEqual,
                                Lt if rty.signed => IntCC::SignedLessThan,
                                Lt => IntCC::UnsignedLessThan,
                                Le if rty.signed => IntCC::SignedLessThanOrEqual,
                                Le => IntCC::UnsignedLessThanOrEqual,
                                Gt if rty.signed => IntCC::SignedGreaterThan,
                                Gt => IntCC::UnsignedGreaterThan,
                                Ge if rty.signed => IntCC::SignedGreaterThanOrEqual,
                                Ge => IntCC::UnsignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            let value = builder.fb.ins().icmp(strategy, lhs, rhs);
                            builder.stack.push((Slot::Value(value), rty));
                        }
                        _ => todo!(),
                    }
                }
                &FinstKind::Call(id) => {
                    let existing = builder.get_used_fn(id);
                    let (fref, arg_count) = self.gen.use_func(id, existing, builder.fb.func);
                    if existing.is_none() {
                        builder.push_used_fn(id, fref);
                    }
                    let args = builder
                        .stack
                        .drain(builder.stack.len().saturating_sub(arg_count as usize)..)
                        .collect::<Vec<_>>()
                        .into_iter()
                        .map(|(slot, ..)| builder.slot_as_value(slot))
                        .collect::<Vec<_>>();

                    let call = builder.fb.ins().call(fref, &args);
                    let rets = builder.fb.inst_results(call);
                    if let &[ret] = rets {
                        let spec = builder.spec(ty, self.types);
                        builder.stack.push((Slot::Value(ret), spec));
                    }
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
                FinstKind::If => {
                    let then = builder.fb.create_block();
                    let end = builder.fb.create_block();
                    let (cond, _) = builder.pop_as_value();
                    let spec = builder.spec(ty, self.types);
                    let returns = spec.layout.takes_space();

                    builder.fb.ins().brif(cond, then, &[], end, &[]);
                    builder.switch_to_block(then);
                    builder.ifs.push(If { returns, end });
                }
                FinstKind::Else => {
                    let If { returns, end } = builder.ifs.pop().unwrap();
                    let ret = returns.then(|| builder.pop_as_value().0);

                    let new_end = builder.fb.create_block();
                    builder.fb.ins().jump(new_end, ret.as_slice());
                    builder.switch_to_block(end);
                    builder.ifs.push(If {
                        returns,
                        end: new_end,
                    });
                }
                FinstKind::EndIf => {
                    let If { end, returns } = builder.ifs.pop().unwrap();
                    let ret = returns.then(|| builder.pop_as_value());

                    if let Some((_, spec)) = ret {
                        let val = builder.fb.append_block_param(end, spec.repr);
                        builder.stack.push((Slot::Value(val), spec));
                    }
                    builder.fb.ins().jump(end, ret.map(|(v, _)| v).as_slice());
                    builder.switch_to_block(end);
                }
                FinstKind::Return(returns) => {
                    let ret = returns.then(|| builder.pop_as_value().0);
                    builder.fb.ins().return_(ret.as_slice());
                }
                FinstKind::Uninit => {
                    let spec = builder.spec(ty, self.types);
                    let value = builder.fb.ins().iconst(spec.repr, 0);
                    builder.stack.push((Slot::Value(value), spec));
                }
            }
        }

        if let Some((slot, _)) = builder.stack.pop() {
            let value = builder.slot_as_value(slot);
            builder.fb.ins().return_(&[value]);
        } else {
            builder.fb.ins().return_(&[]);
        }

        builder.finalize();

        if self.dump_ir {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Note, format_args!("Generated IR for {entry:?}"))
                .footer(Severty::Note, res.ctx.func.display());
            //println!("{}", res.ctx.func.display());
        }

        let module_id = self.gen.func_mapping[id as usize];
        self.gen
            .module
            .define_function(module_id, &mut res.ctx)
            .unwrap();

        CodeRef::from_repr(0)
    }

    pub fn load_signature(&self, entry: &stac::Entry) -> ir::Signature {
        ir::Signature {
            params: entry
                .inputs
                .iter()
                .filter_map(|ty| self.load_local(ty))
                .map(|spec| spec.repr)
                .map(ir::AbiParam::new)
                .collect(),
            returns: vec![ir::AbiParam::new(ir::types::I32)],
            // TODO: we could use fast internally
            call_conv: self.gen.module.target_config().default_call_conv,
        }
    }

    fn load_local(&self, local: &stac::Local) -> Option<TypeSpec> {
        match local {
            stac::Local::Ct(_) => None,
            stac::Local::Rt(ty) => Some(TypeSpec::from_ty(ty, self.arch, self.types)),
        }
    }
}

#[derive(Clone, Copy)]
struct TypeSpec {
    signed: bool,
    repr: ir::Type,
    layout: Layout,
}

impl TypeSpec {
    fn from_ty(ty: &Type, arch: Layout, types: &Types) -> Self {
        let ptr_ty = Self::arch_layout_repr(arch);
        let layout = Layout::from_ty(ty, arch, types);
        let repr = Self::type_repr(ty, ptr_ty, layout);
        let signed = ty.is_signed();
        Self {
            repr,
            layout,
            signed,
        }
    }

    fn type_repr(ty: &Type, arch: ir::Type, layout: Layout) -> ir::Type {
        match ty {
            Type::BuiltIn(b) => Self::built_in_type_repr(b, arch),
            Type::Record(..) => Self::record_repr_for(layout),
        }
    }

    fn record_repr_for(layout: Layout) -> ir::Type {
        match layout.size() {
            0 => ir::types::INVALID,
            1 => ir::types::I8,
            2 => ir::types::I16,
            4 => ir::types::I32,
            _ => ir::types::I64,
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
            Type | Def | Module | Unknown | Integer | Never => unreachable!(),
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
            layout: Layout::ZERO,
            signed: false,
        }
    }
}

fn pop_array<T, const N: usize>(vec: &mut Vec<T>) -> [T; N] {
    let mut iter = vec.drain(vec.len() - N..);
    array::from_fn(|_| iter.next().unwrap())
}

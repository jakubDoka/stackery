use std::array;

use cranelift_codegen::{
    entity::SecondaryMap,
    ir::{self, condcodes::IntCC, InstBuilder, MemFlags},
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

#[derive(Clone, Copy, Debug)]
enum SlotKind {
    Value(ir::Value),
    Stack(ir::StackSlot),
    Uninit,
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

#[derive(Clone, Copy, Debug)]
struct Slot {
    kind: SlotKind,
    spec: TypeSpec,
    offset: u32,
}

impl Slot {
    fn new(slot: SlotKind, spec: TypeSpec) -> Self {
        Self {
            kind: slot,
            spec,
            offset: 0,
        }
    }

    fn with_offset(mut self, offset: u32) -> Self {
        self.offset = offset;
        self
    }
}

#[derive(Clone, Copy, Debug)]
enum SlotError {
    Uninit,
    BigStack(ir::StackSlot),
}

struct Builder<'ctx> {
    fb: FunctionBuilder<'ctx>,
    current_block: ir::Block,
    types: &'ctx IrTypes,
    arch: Layout,
    locals: Vec<Slot>,
    stack: Vec<Slot>,
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
        let mut locals = vec![];

        fb.append_block_params_for_function_params(current_block);
        for (&param, spec) in fb.block_params(current_block).iter().zip(arg_specs) {
            locals.push(Slot::new(SlotKind::Value(param), spec));
        }
        fb.switch_to_block(current_block);

        Self {
            fb,
            current_block,
            types,
            arch,
            locals,

            stack: vec![],
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

    fn try_slot_as_value(&mut self, slot: Slot) -> Result<ir::Value, SlotError> {
        let mut value = match slot.kind {
            SlotKind::Value(v) => v,
            SlotKind::Var(v) => self.fb.use_var(v),
            SlotKind::Stack(s) if !slot.spec.layout.fits_register(self.arch) => {
                return Err(SlotError::BigStack(s))
            }
            SlotKind::Stack(s) => {
                let value = self
                    .fb
                    .ins()
                    .stack_load(slot.spec.repr, s, slot.offset as i32);
                return Ok(value);
            }
            SlotKind::Uninit => return Err(SlotError::Uninit),
        };

        let val_ty = self.fb.func.dfg.value_type(value);
        let actual_ty = slot.spec.repr;

        if slot.offset != 0 {
            value = self.fb.ins().ushr_imm(value, (slot.offset * 8) as i64);
        }

        if val_ty != actual_ty {
            value = match val_ty.bytes().cmp(&actual_ty.bytes()) {
                std::cmp::Ordering::Less => self.fb.ins().uextend(actual_ty, value),
                std::cmp::Ordering::Greater => self.fb.ins().ireduce(actual_ty, value),
                std::cmp::Ordering::Equal => {
                    self.fb.ins().bitcast(actual_ty, MemFlags::new(), value)
                }
            };
        }

        Ok(value)
    }

    #[track_caller]
    fn slot_as_value(&mut self, slot: Slot) -> ir::Value {
        self.try_slot_as_value(slot).unwrap()
    }

    fn try_pop_as_value(&mut self) -> (Result<ir::Value, SlotError>, TypeSpec) {
        let slot = self.stack.pop().unwrap();
        (self.try_slot_as_value(slot), slot.spec)
    }

    fn pop_as_value(&mut self) -> (ir::Value, TypeSpec) {
        let (value, spec) = self.try_pop_as_value();
        (value.unwrap(), spec)
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

        for &Finst {
            ref kind, ty, span, ..
        } in ir.instrs.iter()
        {
            match kind {
                &FinstKind::Sym(sym) => {
                    let slot = builder.locals[sym as usize];
                    builder.stack.push(slot);
                }
                FinstKind::Const(c) => match c {
                    stac::LitKindAst::Int(i) => {
                        let spec = builder.spec(ty, self.types);
                        let value = builder.fb.ins().iconst(spec.repr, i.value() as i64);
                        builder.stack.push(Slot::new(SlotKind::Value(value), spec));
                    }
                    &stac::LitKindAst::Bool(b) => {
                        let spec = builder.spec(ty, self.types);
                        let value = builder.fb.ins().iconst(spec.repr, b as i64);
                        builder.stack.push(Slot::new(SlotKind::Value(value), spec));
                    }
                },
                FinstKind::BinOp(op) => {
                    let [lhs, rhs] = pop_array(&mut builder.stack);

                    use ir::types::*;
                    use OpCode::*;
                    match (op, rhs.spec.repr) {
                        (stac::op_group!(math), I8 | I16 | I32 | I64) => {
                            let rv = builder.slot_as_value(rhs);
                            let lv = builder.slot_as_value(lhs);
                            let value = match op {
                                Mul => builder.fb.ins().imul(lv, rv),
                                Div if rhs.spec.signed => builder.fb.ins().sdiv(lv, rv),
                                Div => builder.fb.ins().udiv(lv, rv),
                                Mod if rhs.spec.signed => builder.fb.ins().srem(lv, rv),
                                Mod => builder.fb.ins().urem(lv, rv),
                                Add => builder.fb.ins().iadd(lv, rv),
                                Sub => builder.fb.ins().isub(lv, rv),
                                Shl => builder.fb.ins().ishl(lv, rv),
                                Shr if rhs.spec.signed => builder.fb.ins().sshr(lv, rv),
                                Shr => builder.fb.ins().ushr(lv, rv),
                                BitAnd => builder.fb.ins().band(lv, rv),
                                BitOr => builder.fb.ins().bor(lv, rv),
                                BitXor => builder.fb.ins().bxor(lv, rv),
                                _ => unreachable!(),
                            };
                            builder
                                .stack
                                .push(Slot::new(SlotKind::Value(value), rhs.spec));
                        }
                        (stac::op_group!(assign), _) => 'a: {
                            let mut rv = match builder.try_slot_as_value(rhs) {
                                Ok(v) => v,
                                Err(SlotError::Uninit) => unreachable!(),
                                Err(SlotError::BigStack(source)) => {
                                    let SlotKind::Stack(ss) = lhs.kind else {
                                        todo!("this might be a pointer")
                                    };
                                    let dest = builder.fb.ins().stack_addr(
                                        lhs.spec.repr,
                                        ss,
                                        lhs.offset as i32,
                                    );
                                    let src = builder.fb.ins().stack_addr(
                                        rhs.spec.repr,
                                        source,
                                        rhs.offset as i32,
                                    );
                                    let size = rhs.spec.layout.size() as u64;
                                    let dest_align = lhs.spec.layout.align();
                                    let src_align = rhs.spec.layout.align();
                                    let non_overlapping = source != ss;
                                    let flags = MemFlags::new();
                                    builder.fb.emit_small_memory_copy(
                                        self.gen.module.target_config(),
                                        dest,
                                        src,
                                        size,
                                        dest_align,
                                        src_align,
                                        non_overlapping,
                                        flags,
                                    );
                                    break 'a;
                                }
                            };

                            match lhs.kind {
                                SlotKind::Stack(ss) => {
                                    builder.fb.ins().stack_store(rv, ss, lhs.offset as i32);
                                }
                                SlotKind::Uninit | SlotKind::Value(_) => unreachable!(),
                                SlotKind::Var(var) => {
                                    let var_ty = builder.locals[var.as_u32() as usize].spec.repr;

                                    if var_ty != rhs.spec.repr {
                                        rv = match var_ty.bytes().cmp(&rhs.spec.repr.bytes()) {
                                            std::cmp::Ordering::Less => {
                                                builder.fb.ins().ireduce(var_ty, rv)
                                            }
                                            std::cmp::Ordering::Greater => {
                                                builder.fb.ins().uextend(var_ty, rv)
                                            }
                                            std::cmp::Ordering::Equal => builder.fb.ins().bitcast(
                                                var_ty,
                                                MemFlags::new(),
                                                rv,
                                            ),
                                        };
                                    }

                                    if lhs.offset != 0 {
                                        let clear_mask = 1u64
                                            .checked_shl(rhs.spec.repr.bits())
                                            .unwrap_or(0)
                                            .wrapping_sub(1);
                                        let clear_mask = clear_mask << lhs.offset * 8;
                                        let clear_mask = !clear_mask as i64;

                                        rv = builder.fb.ins().ishl_imm(rv, (lhs.offset * 8) as i64);

                                        if let Ok(prev_value) = builder.fb.try_use_var(var) {
                                            let prev_value =
                                                builder.fb.ins().bor_imm(prev_value, clear_mask);
                                            rv = builder.fb.ins().bor(rv, prev_value);
                                        }
                                    }

                                    builder.fb.def_var(var, rv);
                                }
                            }
                        }
                        (stac::op_group!(cmp), I8 | I16 | I32 | I64) => {
                            let rv = builder.slot_as_value(rhs);
                            let lv = builder.slot_as_value(lhs);
                            let strategy = match op {
                                Eq => IntCC::Equal,
                                Ne => IntCC::NotEqual,
                                Lt if rhs.spec.signed => IntCC::SignedLessThan,
                                Lt => IntCC::UnsignedLessThan,
                                Le if rhs.spec.signed => IntCC::SignedLessThanOrEqual,
                                Le => IntCC::UnsignedLessThanOrEqual,
                                Gt if rhs.spec.signed => IntCC::SignedGreaterThan,
                                Gt => IntCC::UnsignedGreaterThan,
                                Ge if rhs.spec.signed => IntCC::SignedGreaterThanOrEqual,
                                Ge => IntCC::UnsignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            let value = builder.fb.ins().icmp(strategy, lv, rv);
                            let spec = builder.spec(ty, self.types);
                            builder.stack.push(Slot::new(SlotKind::Value(value), spec));
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
                        .map(|slot| builder.slot_as_value(slot))
                        .collect::<Vec<_>>();

                    let call = builder.fb.ins().call(fref, &args);
                    let rets = builder.fb.inst_results(call);
                    if let &[ret] = rets {
                        let spec = builder.spec(ty, self.types);
                        builder.stack.push(Slot::new(SlotKind::Value(ret), spec));
                    }
                }
                &FinstKind::Field(f) => {
                    let slot = builder.stack.pop().unwrap();
                    if slot.spec.ty == Type::BuiltIn(BuiltInType::Uint) {
                        self.diags.builder(self.modules.files()).annotation(
                            Severty::Note,
                            span,
                            "field access",
                        );
                        println!("{}", self.diags);
                        println!("{}", builder.fb.func.display());
                    }
                    let offset = Layout::field_offset(f, slot.spec.ty, self.arch, self.types);
                    let spec = builder.spec(ty, self.types);
                    builder
                        .stack
                        .push(Slot::new(slot.kind, spec).with_offset(slot.offset + offset));
                }
                &FinstKind::Decl(mutable) => {
                    let slot = builder.stack.pop().unwrap();
                    let var = Variable::from_u32(builder.stack.len() as u32);
                    let new_slot = match builder.try_slot_as_value(slot) {
                        Ok(value) if !mutable => Slot::new(SlotKind::Value(value), slot.spec),
                        Ok(value) => {
                            builder.fb.declare_var(var, slot.spec.repr);
                            builder.fb.def_var(var, value);
                            Slot::new(SlotKind::Var(var), slot.spec)
                        }
                        Err(SlotError::Uninit) => {
                            builder.fb.declare_var(var, slot.spec.repr);
                            Slot::new(SlotKind::Var(var), slot.spec)
                        }
                        Err(SlotError::BigStack(_)) => slot,
                    };
                    builder.locals.push(new_slot);
                }
                FinstKind::Drop => {
                    builder.stack.pop().unwrap();
                }
                &FinstKind::DropScope(size, returns) => {
                    let return_slot = returns.then(|| builder.stack.pop().unwrap());

                    builder
                        .locals
                        .truncate(builder.locals.len() - size as usize);
                    if let Some(slot) = return_slot {
                        let new_slot = match builder.try_slot_as_value(slot) {
                            Ok(val) => Slot::new(SlotKind::Value(val), slot.spec),
                            Err(SlotError::BigStack(_)) => slot,
                            Err(SlotError::Uninit) => unreachable!(),
                        };
                        builder.stack.push(new_slot);
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
                        builder.stack.push(Slot::new(SlotKind::Value(val), spec));
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
                    if !spec.layout.fits_register(self.arch) {
                        let ss = builder.fb.create_sized_stack_slot(ir::StackSlotData {
                            kind: ir::StackSlotKind::ExplicitSlot,
                            size: spec.layout.size(),
                        });
                        builder.stack.push(Slot::new(SlotKind::Stack(ss), spec));
                    } else {
                        builder.stack.push(Slot::new(SlotKind::Uninit, spec));
                    }
                }
            }
        }

        if let Some(slot) = builder.stack.pop() {
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
            returns: self
                .load_local(&stac::Local::Rt(entry.ret))
                .map(|spec| spec.repr)
                .map(ir::AbiParam::new)
                .into_iter()
                .collect(),
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

#[derive(Clone, Copy, Debug)]
struct TypeSpec {
    signed: bool,
    repr: ir::Type,
    layout: Layout,
    ty: Type,
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
            ty: ty.clone(),
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
            ty: BuiltInType::Unknown.into(),
        }
    }
}

fn pop_array<T, const N: usize>(vec: &mut Vec<T>) -> [T; N] {
    let mut iter = vec.drain(vec.len() - N..);
    array::from_fn(|_| iter.next().unwrap())
}

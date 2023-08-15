use std::ops::Deref;

use crate::{
    lexer, ArgCount, BuiltInType, Const, ConstRef, CtxSym, Finst, FinstKind, FrameSize, FuncId,
    FuncType, InstrBody, InstrKind, InstrRef, IntLit, IrTypes, LitKindAst, ModuleDecl, ModuleRef,
    Mutable, OpCode, Resolved, Returns, Severty, ShadowStore, Span, SubsRef, SubsTable, Sym, Type,
    TypeRef, UnificationError,
};

use super::{Entry, Interpreter, Ir, Layout, Local, SubsRepr};

#[derive(Clone, Debug)]
enum DeclValue {
    Const(Const),
    Module(ModuleRef),
    Type(TypeRef),
}

#[derive(Clone, Debug)]
struct Decl {
    value: Result<DeclValue, usize>,
    mutable: bool,
    span: Span,
    ty: SubsRef,
}

#[derive(Clone, Debug)]
enum OperandValue {
    Const { value: Const, pos: u16 },
    Decl(usize),
    Module(ModuleRef),
    Type(TypeRef),
}

#[derive(Clone, Debug)]
struct Operand {
    value: Option<OperandValue>,
    ty: SubsRef,
}

enum If {
    Const,
    Runtime(SubsRef),
}

#[derive(Default)]
struct Res<'a> {
    _module: ModuleRef,
    enfoce_const: bool,
    params: Vec<Local>,
    body: InstrBody<'a>,
    _decls: ModuleDecl<'a>,
    this: Option<FuncId>,

    decls: Vec<Decl>,
    stack: Vec<Operand>,
    subs_table: SubsTable<SubsRepr>,
    type_mapping: ShadowStore<Type, SubsRepr, TypeRef>,
    ip: usize,
    type_group_seq: SubsRepr,

    if_stack: Vec<If>,

    ir: Ir,
}

impl<'a> Res<'a> {
    fn new(
        module: ModuleRef,
        enforce_const: bool,
        body: InstrBody<'a>,
        decls: ModuleDecl<'a>,
    ) -> Self {
        Self {
            _module: module,
            enfoce_const: enforce_const,
            params: Vec::new(),
            body,
            _decls: decls,

            ir: Ir {
                instrs: Vec::new(),
                types: {
                    let mut store = IrTypes::new();
                    store.extend(body.types.iter().cloned());
                    store
                },
            },

            ..Default::default()
        }
    }

    fn create_group(&mut self, ty: TypeRef) -> SubsRef {
        let id = self.type_group_seq;
        self.type_group_seq += 1;
        let id = SubsRef::from_repr(id);
        if ty != TypeRef::default() {
            self.type_mapping[id] = ty;
        }
        id
    }

    fn intern_ty(&mut self, ty: &Type) -> TypeRef {
        self.ir.intern_ty(ty)
    }

    fn intern_and_create_group(&mut self, ty: &Type) -> SubsRef {
        let ty = self.intern_ty(ty);
        self.create_group(ty)
    }

    fn remap_types(mut self) -> Ir {
        for instr in self.ir.instrs.iter_mut() {
            instr.ty = self.subs_table.root_of_ref(instr.ty);
            instr.ty = self.type_mapping[instr.ty];
        }

        self.ir
    }

    fn get_subs(&mut self, ty: SubsRef) -> Type {
        let ty = self.subs_table.root_of_ref(ty);
        let ty = self.type_mapping[ty];
        self.ir.get_ty(ty)
    }

    fn set_subs(&mut self, ty: SubsRef, new: Type) {
        let ty = self.subs_table.root_of_ref(ty);
        self.type_mapping[ty] = self.ir.intern_ty(&new);
    }

    fn root_value(&self, value: OperandValue) -> DeclValue {
        match value {
            OperandValue::Const { value, .. } => DeclValue::Const(value),
            OperandValue::Decl(index) => return self.decls[index as usize].value.clone().unwrap(),
            OperandValue::Module(value) => DeclValue::Module(value),
            OperandValue::Type(value) => DeclValue::Type(value),
        }
    }

    fn decl_as_operand(&self, result: DeclValue) -> OperandValue {
        let pos = self.ir.instrs.len() as u16;
        match result {
            DeclValue::Const(value) => OperandValue::Const { value, pos },
            DeclValue::Module(value) => OperandValue::Module(value),
            DeclValue::Type(value) => OperandValue::Type(value),
        }
    }

    fn operand_as_res(&self, value: OperandValue) -> Resolved {
        let decl = self.root_value(value);
        self.decl_as_res(decl)
    }

    fn decl_as_res(&self, value: DeclValue) -> Resolved {
        match value {
            DeclValue::Const(c) => Resolved::Const(c),
            DeclValue::Module(m) => Resolved::Module(m),
            DeclValue::Type(t) => Resolved::Type(self.ir.get_ty(t)),
        }
    }

    fn res_as_operand(&mut self, res: Resolved) -> OperandValue {
        match res {
            Resolved::Const(c) => OperandValue::Const {
                value: c,
                pos: self.ir.instrs.len() as u16,
            },
            Resolved::Module(m) => OperandValue::Module(m),
            Resolved::Type(t) => OperandValue::Type(self.ir.intern_ty(&t)),
        }
    }

    fn copy_last_op(&mut self, span: Span) -> SubsRef {
        let mut op = self.stack.pop().unwrap();
        if let Some(OperandValue::Const { value, .. }) = op.value.take() {
            self.ir.instrs.push(Finst {
                kind: FinstKind::Const(value),
                ty: op.ty,
                span,
            });
        }
        let ty = op.ty;
        self.stack.push(op);
        ty
    }
}

impl<'ctx> Interpreter<'ctx> {
    pub fn ct_eval(
        &mut self,
        span: Span,
        module: ModuleRef,
        body: InstrBody,
        decls: ModuleDecl,
    ) -> Option<Resolved> {
        let prev_count = self.diags.error_count();

        let mut res = Res::new(module, true, body, decls);

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(module), &mut res);
        }

        if self.diags.error_count() > prev_count {
            return None;
        }

        let Some(Operand {
            value: Some(value), ..
        }) = res.stack.pop()
        else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "expected result from expression")
                .annotation(Severty::Error, span, "this expression does not evaluate");
            return None;
        };

        Some(res.operand_as_res(value))
    }

    fn eval_signature(
        &mut self,
        f: FuncId,
        func: crate::Func,
        view: crate::InstrBody<'_>,
        _span: Span,
    ) -> (Vec<Type>, Type) {
        let mut res = Res::new(f.module, true, view, self.instrs.decls_of(f.module));

        while let Some(instr) = res.body.instrs.deref()[..func.signature_len as usize].get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(f.module), &mut res);
        }

        let ret = match res.stack.pop() {
            Some(Operand {
                value: Some(OperandValue::Type(t)),
                ..
            }) => res.ir.get_ty(t),
            f => todo!("error handilng: {f:?}"),
        };

        let params = res
            .decls
            .drain(..)
            .map(|d| match d.value {
                Ok(DeclValue::Type(t)) => res.ir.get_ty(t),
                f => todo!("error handilng: {f:?}"),
            })
            .collect();

        (params, ret)
    }

    pub fn eval(&mut self, entry: &Entry) -> Ir {
        let mut res = Res::new(
            entry.id.module,
            false,
            self.instrs.body_of(entry.id),
            self.instrs.decls_of(entry.id.module),
        );

        res.params = entry.inputs.clone();
        res.ip = entry.ip as _;
        res.this = Some(entry.id);

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(entry.id.module), &mut res);
        }

        if let Some(operand) = res.stack.pop() {
            let ret = res.intern_and_create_group(&entry.ret);
            let ty = self.unify_subs(
                operand.ty,
                ret,
                res.body
                    .instrs
                    .last()
                    .unwrap()
                    .span
                    .to_span(entry.id.module),
                &mut res,
            );
            if let Some(OperandValue::Const { value, pos }) = operand.value {
                res.ir.instrs.insert(
                    pos as usize,
                    Finst {
                        kind: FinstKind::Const(value),
                        ty,
                        span: Span::new(0, 0, Default::default()),
                    },
                );
            }
        }

        res.remap_types()
    }

    fn eval_instr(&mut self, instr: &InstrKind, span: Span, res: &mut Res) {
        match instr.clone() {
            InstrKind::Uninit(ty) => self.eval_uninit(ty, res),
            InstrKind::Sym(s) => self.eval_sym(s, span, res),
            InstrKind::BinOp(op) => self.eval_binop(op, span, res),
            InstrKind::Call(ac) => self.eval_call(ac, span, res),
            InstrKind::Field(field) => self.eval_field(field, span, res),
            InstrKind::Decl(mutable) => self.eval_decl(span, mutable, res),
            InstrKind::Drop => self.eval_drop(span, res),
            InstrKind::DropScope(s, r) => self.eval_drop_scope(s, r, span, res),
            InstrKind::Module(m) => self.eval_module(m, span, res),
            InstrKind::Const(c) => self.eval_const(c, span, res),
            InstrKind::Type(t) => self.eval_type(t, span, res),
            InstrKind::If(e) => self.eval_if(e, span, res),
            InstrKind::Else(e) => self.eval_else(e, span, res),
            InstrKind::EndIf => self.eval_endif(span, res),
            InstrKind::Rt => self.eval_rt(span, res),
        }
    }

    fn eval_rt(&mut self, span: Span, res: &mut Res) {
        let op = res.stack.pop().unwrap();
        let Some(value) = op.value else {
            res.stack.push(op);
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Warning, "useles forced runtime value")
                .annotation(
                    Severty::Warning,
                    span,
                    "this expression cannot be folded anyway",
                );
            return;
        };

        let OperandValue::Const { value, .. } = value else {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "cannot force runtime onto compile time exclusive value",
                )
                .annotation(Severty::Error, span, "this does not have a runtime value");
            return;
        };

        res.stack.push(Operand {
            ty: op.ty,
            value: None,
        });
        res.ir.instrs.push(Finst {
            kind: FinstKind::Const(value),
            ty: op.ty,
            span,
        });
    }

    fn eval_module(&mut self, m: ModuleRef, _span: Span, res: &mut Res) {
        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Module));
        res.stack.push(Operand {
            ty,
            value: Some(OperandValue::Module(m)),
        });
    }

    fn eval_const(&mut self, c: ConstRef, _span: Span, res: &mut Res) {
        let value = res.body.consts[c].clone();
        let pos = res.ir.instrs.len() as _;
        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::from_const(&value)));
        res.stack.push(Operand {
            ty,
            value: Some(OperandValue::Const { value, pos }),
        });
    }

    fn eval_type(&mut self, val: TypeRef, _span: Span, res: &mut Res) {
        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Type));
        res.stack.push(Operand {
            ty,
            value: Some(OperandValue::Type(val)),
        });
    }

    fn eval_if(&mut self, end_block: InstrRef, span: Span, res: &mut Res) {
        let Operand { value, ty } = res.stack.pop().unwrap();

        let bool = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Bool));
        self.unify_subs(ty, bool, span, res);

        if let Some(DeclValue::Const(LitKindAst::Bool(b))) = value.map(|v| res.root_value(v)) {
            res.if_stack.push(If::Const);
            if !b {
                res.ip = end_block.index() + 1;
            }
            return;
        }

        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Unknown));
        res.if_stack.push(If::Runtime(ty));
        res.ir.instrs.push(Finst {
            kind: FinstKind::If,
            ty,
            span,
        });
    }

    fn eval_else(&mut self, end_block: InstrRef, span: Span, res: &mut Res) {
        let If::Runtime(ty) = res.if_stack.pop().unwrap() else {
            res.ip = end_block.index() + 1;
            return;
        };

        let last_ty = res.copy_last_op(span);
        let ty = self.unify_subs(last_ty, ty, span, res);

        res.ir.instrs.push(Finst {
            kind: FinstKind::Else,
            ty,
            span,
        });
        res.if_stack.push(If::Runtime(ty));
    }

    fn eval_endif(&mut self, span: Span, res: &mut Res) {
        let If::Runtime(ty) = res.if_stack.pop().unwrap() else {
            return;
        };

        let last_instr = res.copy_last_op(span);
        let ty = self.unify_subs(last_instr, ty, span, res);

        res.ir.instrs.push(Finst {
            kind: FinstKind::EndIf,
            ty,
            span,
        });
    }

    fn eval_uninit(&mut self, _typ: TypeRef, _res: &mut Res) {
        todo!("not a priority");
    }

    fn res_ty(&mut self, c: &Resolved, _res: &mut Res) -> Type {
        match c {
            Resolved::Type(Type::Func(f)) => Type::Func(f.clone()),
            Resolved::Type(_) => Type::BuiltIn(BuiltInType::Type),
            Resolved::Module(_) => Type::BuiltIn(BuiltInType::Module),
            Resolved::Const(c) => match c {
                LitKindAst::Int(_) => Type::BuiltIn(BuiltInType::Integer),
                LitKindAst::Bool(_) => Type::BuiltIn(BuiltInType::Bool),
            },
        }
    }

    fn eval_sym(&mut self, sym: Sym, span: Span, res: &mut Res) {
        if res.params.len() > sym as usize {
            let local = res.params[sym as usize].clone();
            match local {
                Local::Ct(c) => {
                    let ty = self.res_ty(&c, res);
                    let ty = res.intern_and_create_group(&ty);
                    let value = Some(res.res_as_operand(c));
                    res.stack.push(Operand { value, ty });
                }
                Local::Rt(_) if res.enfoce_const => {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "cannot access runtime local in const eval")
                        .annotation(Severty::Error, span, "runtime local accessed here");
                    return;
                }
                Local::Rt(ty) => {
                    let ty = res.intern_and_create_group(&ty);
                    res.stack.push(Operand { value: None, ty });
                    let final_sym = res.params[..sym as usize]
                        .iter()
                        .filter(|l| matches!(l, Local::Rt(_)))
                        .count();
                    res.ir.instrs.push(Finst {
                        kind: FinstKind::Sym(final_sym as Sym),
                        ty,
                        span,
                    });
                }
            };
            return;
        }

        let operand_index = sym as usize - res.params.len();

        let Decl {
            value, mutable, ty, ..
        } = res.decls[operand_index].clone();

        let value = match value.map(|c| res.decl_as_operand(c)) {
            Ok(_) if mutable => Some(OperandValue::Decl(operand_index)),
            Ok(value) => Some(value),
            Err(sym) => {
                res.ir.instrs.push(Finst {
                    kind: FinstKind::Sym(sym as _),
                    ty,
                    span,
                });
                None
            }
        };
        res.stack.push(Operand { value, ty });
    }

    fn eval_binop(&mut self, op: OpCode, span: Span, res: &mut Res) {
        let [rhs, lhs] = [res.stack.pop().unwrap(), res.stack.pop().unwrap()];

        match (rhs.value, lhs.value) {
            (Some(a), Some(b)) => self.fold_binop(op, span, rhs.ty, [b, a], res),
            (r, l) => {
                if let Some(OperandValue::Const { value, pos }) = l {
                    res.ir.instrs.insert(
                        pos as _,
                        Finst {
                            kind: FinstKind::Const(value),
                            ty: lhs.ty,
                            span,
                        },
                    );
                }
                if let Some(OperandValue::Const { value, pos }) = r {
                    res.ir.instrs.insert(
                        pos as _,
                        Finst {
                            kind: FinstKind::Const(value),
                            ty: rhs.ty,
                            span,
                        },
                    );
                }
                let ty = self.infer_binop_type(op, span, rhs.ty, lhs.ty, res);
                res.stack.push(Operand { value: None, ty });
                res.ir.instrs.push(Finst {
                    kind: FinstKind::BinOp(op),
                    ty,
                    span,
                });
            }
        }
    }

    fn fold_binop(
        &mut self,
        op: OpCode,
        span: Span,
        ty: SubsRef,
        [lhs, rhs]: [OperandValue; 2],
        res: &mut Res,
    ) {
        match (op, lhs, rhs) {
            (OpCode::Assign, OperandValue::Decl(sym), new) => {
                let new = res.root_value(new);

                let &mut Decl {
                    ref mut value,
                    mutable,
                    span,
                    ..
                } = &mut res.decls[sym as usize];

                if !mutable {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "cannot assign to immutable symbol")
                        .annotation(Severty::Error, span, "assignment happens here");
                } else {
                    *value = Ok(new);
                }
            }
            (_, lhs, rhs) => {
                let [lhs_r, rhs_r] = [res.root_value(lhs), res.root_value(rhs)];
                let (result, ty) = self.fold_resolved_binop(op, span, ty, lhs_r, rhs_r, res);
                let operand = res.decl_as_operand(result);
                res.stack.push(Operand {
                    value: Some(operand),
                    ty,
                });
            }
        }
    }

    fn fold_resolved_binop(
        &mut self,
        op: OpCode,
        _span: Span,
        ty: SubsRef,
        lhs: DeclValue,
        rhs: DeclValue,
        res: &mut Res,
    ) -> (DeclValue, SubsRef) {
        match (op, lhs, rhs) {
            (
                crate::op_group!(math),
                DeclValue::Const(LitKindAst::Int(a)),
                DeclValue::Const(LitKindAst::Int(b)),
            ) => {
                let (a, b) = (a.value(), b.value());
                let result = match op {
                    OpCode::Add => a + b,
                    OpCode::Sub => a - b,
                    OpCode::Mul => a * b,
                    OpCode::Div => a / b,
                    OpCode::Mod => a % b,
                    OpCode::BitAnd => a & b,
                    OpCode::BitOr => a | b,
                    OpCode::BitXor => a ^ b,
                    OpCode::Shl => a << b,
                    OpCode::Shr => a >> b,
                    _ => unreachable!(),
                };
                (DeclValue::Const(LitKindAst::Int(IntLit::new(result))), ty)
            }
            (
                crate::op_group!(cmp),
                DeclValue::Const(LitKindAst::Int(a)),
                DeclValue::Const(LitKindAst::Int(b)),
            ) => {
                let (a, b) = (a.value(), b.value());
                let result = match op {
                    OpCode::Eq => a == b,
                    OpCode::Ne => a != b,
                    OpCode::Lt => a < b,
                    OpCode::Le => a <= b,
                    OpCode::Gt => a > b,
                    OpCode::Ge => a >= b,
                    _ => unreachable!(),
                };
                let ty = res.intern_and_create_group(&BuiltInType::Bool.into());
                (DeclValue::Const(LitKindAst::Bool(result)), ty)
            }
            _ => unreachable!(),
        }
    }

    fn infer_binop_type(
        &mut self,
        op: OpCode,
        span: Span,
        lhs: SubsRef,
        rhs: SubsRef,
        res: &mut Res,
    ) -> SubsRef {
        let root = self.unify_subs(lhs, rhs, span, res);
        match op {
            lexer::op_group!(math) => root,
            lexer::op_group!(cmp) => res.intern_and_create_group(&BuiltInType::Bool.into()),
            crate::op_group!(logic) => todo!(),
            lexer::op_group!(assign) => res.intern_and_create_group(&BuiltInType::Unit.into()),
        }
    }

    fn eval_call(&mut self, ac: ArgCount, span: Span, res: &mut Res) {
        let Some(OperandValue::Type(t)) = res.stack.pop().and_then(|o| o.value) else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "cannot call this expression (yet)",
            );
            return;
        };

        let Type::Func(FuncType::Static(id)) = res.ir.get_ty(t) else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                format_args!("cannot call this expression (yet) mlc {}", res.ir.get_ty(t)),
            );
            return;
        };

        let (func, view) = self.instrs.func_data(id);
        let (params, ret) = self.eval_signature(id, func.clone(), view, span);

        let mut entry = Entry {
            id,
            ip: func.signature_len,
            inputs: params.iter().cloned().map(Local::Rt).collect(),
            ret: ret.clone(),
        };

        let param_range = res.stack.len() - ac as usize..;

        let operands = res.stack.drain(param_range.clone()).collect::<Vec<_>>();

        for ((param, operand), local) in params.iter().zip(operands).zip(&mut entry.inputs).rev() {
            let param_ty = res.intern_and_create_group(param);
            self.unify_subs(param_ty, operand.ty, span, res);
            if let Some(value) = operand.value {
                if res.this != Some(id) || res.enfoce_const {
                    *local = Local::Ct(res.operand_as_res(value));
                } else {
                    if let OperandValue::Const { value, pos } = value {
                        res.ir.instrs.insert(
                            pos as _,
                            Finst {
                                kind: FinstKind::Const(value),
                                ty: param_ty,
                                span,
                            },
                        );
                    }
                }
            }
        }

        if entry.inputs.iter().all(|l| matches!(l, Local::Ct(_))) {
            let ty = res.intern_and_create_group(&ret);
            let value = self.fold_func(span, entry).map(|r| res.res_as_operand(r));
            res.stack.push(Operand { value, ty });
            return;
        }

        let inst_id = self.instances.project(entry);

        let ty = res.intern_and_create_group(&ret);
        res.stack.push(Operand { value: None, ty });
        res.ir.instrs.push(Finst {
            kind: FinstKind::Call(inst_id),
            ty,
            span,
        });
    }

    pub fn fold_func(
        &mut self,
        span: Span,
        Entry { id, ip, inputs, .. }: Entry,
    ) -> Option<Resolved> {
        let prev_count = self.diags.error_count();

        let mut res = Res::new(
            id.module,
            true,
            self.instrs.body_of(id),
            self.instrs.decls_of(id.module),
        );

        res.ip = ip as usize;
        res.params.extend(inputs);
        res.this = Some(id);

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(id.module), &mut res);
        }

        if self.diags.error_count() > prev_count {
            return None;
        }

        let Some(r) = res
            .stack
            .pop()
            .and_then(|o| o.value)
            .map(|o| res.operand_as_res(o))
        else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "expected result from expression")
                .annotation(Severty::Error, span, "this expression does not evaluate");
            return None;
        };

        Some(r)
    }

    fn unify_subs(&mut self, a: SubsRef, b: SubsRef, span: Span, res: &mut Res) -> SubsRef {
        let [ra, rb] = [res.get_subs(a), res.get_subs(b)];
        match ra.unify(&rb) {
            Ok(ty) => {
                let root = res.subs_table.join_refs(a, b);
                res.set_subs(root, ty);
                root
            }
            Err(UnificationError::Equal) => res.subs_table.join_refs(a, b),
            Err(UnificationError::Incompatible) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "cannot unify types")
                    .footer(
                        Severty::Note,
                        format_args!("the types are {} and {}", ra, rb),
                    )
                    .annotation(Severty::Error, span, "of this")
                    .terminate();
                res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Unknown))
            }
        }
    }

    fn eval_field(&mut self, field: CtxSym, span: Span, res: &mut Res) {
        let operand = res.stack.pop().unwrap();
        let Some(value) = operand.value else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "runtime field access not supported yet",
            );
            return;
        };

        let field = &res.body.idents[field];

        match res.root_value(value) {
            DeclValue::Type(t) => {
                let t = res.ir.get_ty(t);
                let result = match field.as_str() {
                    "size" => Layout::from_ty(&t, self.arch).size(),
                    "align" => Layout::from_ty(&t, self.arch).expanded_align(),
                    _ => {
                        self.diags
                            .builder(self.modules.files())
                            .footer(Severty::Error, "no such field on the type")
                            .annotation(Severty::Error, span, "accessed here");
                        return;
                    }
                };
                let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Uint));
                res.stack.push(Operand {
                    value: Some(OperandValue::Const {
                        value: LitKindAst::Int(IntLit::new(result as u64)),
                        pos: res.ir.instrs.len() as _,
                    }),
                    ty,
                });
            }
            DeclValue::Module(m) => {
                let decls = self.instrs.decls_of(m);
                let Some(item) = decls.decls.iter().rfind(|d| d.name.ident == *field) else {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "no such field on the module")
                        .annotation(Severty::Error, span, "accessed here");
                    return;
                };
                let ty = self.res_ty(&item.data, res);
                let ty = res.intern_and_create_group(&ty);
                let operand = res.res_as_operand(item.data.clone());
                res.stack.push(Operand {
                    value: Some(operand),
                    ty,
                });
            }
            DeclValue::Const { .. } => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "no such field on the constant")
                    .annotation(Severty::Error, span, "accessed here");
            }
        }
    }

    fn eval_decl(&mut self, span: Span, mutable: Mutable, res: &mut Res) {
        let Operand { value, ty } = res.stack.pop().unwrap();

        let decl_value = if let Some(resolved) = value {
            Ok(res.root_value(resolved))
        } else {
            res.ir.instrs.push(Finst {
                kind: FinstKind::Decl(mutable),
                ty,
                span,
            });

            let next_index = res
                .decls
                .iter()
                .rev()
                .find_map(|d| match d.value {
                    Err(index) => Some(index + 1),
                    _ => None,
                })
                .unwrap_or(0);

            Err(next_index)
        };

        res.decls.push(Decl {
            value: decl_value,
            mutable,
            ty,
            span,
        });
    }

    fn eval_drop(&mut self, span: Span, res: &mut Res) {
        if res.stack.pop().unwrap().value.is_none() {
            let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Unit));
            res.ir.instrs.push(Finst {
                kind: FinstKind::Drop,
                ty,
                span,
            });
        }
    }

    fn eval_drop_scope(&mut self, size: FrameSize, ret: Returns, span: Span, res: &mut Res) {
        if ret {
            let mut operand = res.stack.pop().unwrap();
            // todo: load the value if at runtime
            if let Some(value) = operand.value {
                let root = res.root_value(value);
                operand.value = Some(res.decl_as_operand(root));
            }
            res.stack.push(operand);
        }

        let final_drop_size = res
            .decls
            .drain(res.decls.len() - size as usize..)
            .filter(|d| d.value.is_err())
            .count();

        if final_drop_size == 0 {
            return;
        }

        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Unit));
        res.ir.instrs.push(Finst {
            kind: FinstKind::DropScope(final_drop_size as FrameSize, ret),
            ty,
            span,
        });
    }
}

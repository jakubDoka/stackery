use std::{fmt::Display, mem, ops::Deref};

use crate::{
    lexer, ArgCount, BuiltInType, CtxSym, Def, DefId, DefKind, DefRef, EvalResult, Finst,
    FinstKind, FrameSize, InstrBody, InstrKind, InstrRef, IntLit, LitKindAst, ModuleDecl,
    ModuleRef, OpCode, Record, RecordKind, Resolved, Returns, Severty, ShadowStore, Span,
    SubsTable, Sym, Type, TypeRef, UnificationError,
};

use super::{Entry, Interpreter, Ir, Layout, Local, SubsRepr};

impl<'ctx> Interpreter<'ctx> {
    pub fn eval(&mut self, entry: &Entry) -> EvalResult {
        let mut res = Res::new(
            entry.id.ns,
            self.instrs.body_of(entry.id),
            self.instrs.decls_of(entry.id.ns),
            &entry.ret,
        );

        res.params = entry.inputs.clone();
        res.ip = entry.ip as _;
        res.this = Some(entry.id);

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(entry.id.ns), &mut res);
        }

        if let Some(operand) = res.stack.pop() {
            if !res.is_runtime && let Some(r) = operand.value.clone().map(|v| res.root_value(v)) {
                return EvalResult::Const(r);
            }

            let ret = res.intern_and_create_group(&entry.ret);
            self.unify_subs(
                operand.ty,
                ret,
                res.body.instrs.last().unwrap().span.to_span(entry.id.ns),
                &mut res,
            );
            res.emmit_op_const(&operand);
        }

        EvalResult::Rt(res.remap_types())
    }

    pub fn eval_signature(
        &mut self,
        def: DefId,
        len: u16,
        view: crate::InstrBody<'_>,
        _span: Span,
    ) -> (Vec<Type>, Type) {
        let mut res = Res::new(
            def.ns,
            view,
            self.instrs.decls_of(def.ns),
            &BuiltInType::Type.into(),
        );

        while let Some(instr) = res.body.instrs.deref()[..len as usize].get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(def.ns), &mut res);
        }

        let ret = match res.stack.pop() {
            Some(op) => match op.value {
                Some(OperandValue::Const(Resolved::Type(t))) => t,
                f => todo!("error handilng: {f:?}"),
            },
            None => BuiltInType::Unit.into(),
        };

        let params = res
            .decls
            .drain(..)
            .map(|d| match d.value {
                DeclValue::Const(Resolved::Type(t)) => t,
                f => todo!("error handilng: {f:?}"),
            })
            .collect();

        (params, ret)
    }

    fn eval_instr(&mut self, instr: &InstrKind, span: Span, res: &mut Res) {
        match instr.clone() {
            InstrKind::Else(e) => self.eval_else(e, span, res),
            InstrKind::EndIf => self.eval_endif(span, res),
            _ if res.terminated => {}
            InstrKind::Sym(s) => self.eval_sym(s, span, res),
            InstrKind::BinOp(op) => self.eval_binop(op, span, res),
            InstrKind::Call(ac) => self.eval_call(ac, span, res),
            InstrKind::Field(field) => self.eval_field(field, span, res),
            InstrKind::Decl {
                mutable,
                inited,
                typed,
            } => self.eval_decl(span, mutable, inited, typed, res),
            InstrKind::Drop => self.eval_drop(span, res),
            InstrKind::DropScope(s, r) => self.eval_drop_scope(s, r, span, res),
            InstrKind::Module(m) => res.push_const_op(Resolved::Module(m), span),
            InstrKind::Const(c) => {
                res.push_const_op(Resolved::Const(res.body.consts[c].clone()), span)
            }
            InstrKind::If(e) => self.eval_if(e, span, res),
            InstrKind::Rt => self.eval_rt(span, res),
            InstrKind::Return(r) => self.eval_return(r, span, res),
            InstrKind::Def(def) => self.eval_def(def, span, res),
        }
    }

    fn eval_def(&mut self, id: DefRef, span: Span, res: &mut Res) {
        let id = DefId {
            ns: res.module,
            index: id,
        };
        let def = self.instrs.get_def(id);
        let DefKind::Record { kind, fields } = def.kind else {
            res.push_const_op(Resolved::Def(id), span);
            return;
        };
        let body = self.instrs.body_of(id);
        let (field_types, _) = self.eval_signature(id, body.instrs.len() as _, body, span);
        let rec = Record {
            fields: self
                .instrs
                .get_fields(id.ns, fields)
                .iter()
                .cloned()
                .zip(field_types)
                .map(|(name, ty)| crate::RecordField { name, ty })
                .collect(),
        };
        let rec_id = self.types.create_record();
        self.types[rec_id] = rec;
        res.push_const_op(Resolved::Type(Type::Record(kind, rec_id)), span);
    }

    fn eval_return(&mut self, r: Returns, span: Span, res: &mut Res) {
        let ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Never));
        if r {
            let ret_ty = res.copy_last_op(false);
            self.unify_subs(ret_ty, res.return_ty, span, res);
        } else {
            let ret_ty = res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Uint));
            self.unify_subs(ret_ty, res.return_ty, span, res);
        }

        res.ir.instrs.push(Finst {
            kind: FinstKind::Return(r),
            ty,
            span,
        });
        res.terminated = true;
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

        let OperandValue::Const(Resolved::Const(value)) = value else {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "cannot force runtime onto compile time exclusive value",
                )
                .annotation(Severty::Error, span, "this does not have a runtime value");
            return;
        };

        res.push_rt_op_and_instr(FinstKind::Const(value), op.ty, op.span);

        res.is_runtime = true;
    }

    fn eval_if(&mut self, end_block: InstrRef, span: Span, res: &mut Res) {
        let Operand { value, ty, .. } = res.stack.pop().unwrap();

        let bool = res.builtin_group(BuiltInType::Bool);
        self.unify_subs(ty, bool, span, res);

        if let Some(Resolved::Const(LitKindAst::Bool(b))) = value.map(|v| res.root_value(v)) {
            res.if_stack.push(If::Const);
            if !b {
                res.ip = end_block.index() + 1;
            }
            return;
        }

        let unknown = res.builtin_group(BuiltInType::Unknown);
        res.if_stack.push(If::Runtime(unknown));
        res.ir.instrs.push(Finst {
            kind: FinstKind::If,
            ty: unknown,
            span,
        });
    }

    fn eval_else(&mut self, end_block: InstrRef, span: Span, res: &mut Res) {
        let If::Runtime(ty) = res.if_stack.pop().unwrap() else {
            res.ip = end_block.index() + 1;
            return;
        };

        let ty = self.end_control_flow(ty, span, res);

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

        self.end_control_flow(ty, span, res);

        res.ir.instrs.push(Finst {
            kind: FinstKind::EndIf,
            ty,
            span,
        });
    }

    fn eval_sym(&mut self, sym: Sym, span: Span, res: &mut Res) {
        let Some(param_or_decl_index) = (sym as usize).checked_sub(res.defs.decls.len()) else {
            let def_ref = DefRef::from_repr(sym);
            let resolved = self.access_module_decl(res.module, def_ref);
            res.push_const_op(resolved, span);
            return;
        };

        let Some(decl_index) = param_or_decl_index.checked_sub(res.params.len()) else {
            let local = res.params[param_or_decl_index as usize].clone();
            match local {
                Local::Ct(c) => res.push_const_op(c, span),
                Local::Rt(ty) => {
                    res.is_runtime = true;
                    let ty = res.intern_and_create_group(&ty);
                    let final_sym = res.params[..param_or_decl_index]
                        .iter()
                        .filter(|l| matches!(l, Local::Rt(_)))
                        .count();
                    res.push_rt_op_and_instr(FinstKind::Sym(final_sym as Sym), ty, span)
                }
            };
            return;
        };

        let Decl {
            value, mutable, ty, ..
        } = res.decls[decl_index].clone();

        let value = Some(match value {
            DeclValue::Const(_) if mutable => OperandValue::Decl(decl_index),
            DeclValue::Const(c) => OperandValue::Const(c),
            DeclValue::Runtime(sym) => {
                res.push_rt_op_and_instr(FinstKind::Sym(sym as _), ty, span);
                return;
            }
        });
        let pos = res.ir.instrs.len();
        res.stack.push(Operand {
            value,
            ty,
            span,
            pos,
        });
    }

    fn eval_binop(&mut self, op: OpCode, span: Span, res: &mut Res) {
        //self.log_stack("binop", span, res);log_sta

        let [mut rhs, mut lhs] = [res.stack.pop().unwrap(), res.stack.pop().unwrap()];

        match (rhs.value, lhs.value) {
            (Some(a), Some(b)) => self.fold_binop(op, span, rhs.ty, [b, a], res),
            (r, l) => {
                rhs.value = r;
                lhs.value = l;
                res.emmit_op_const(&lhs);
                res.emmit_op_const(&rhs);
                let ty = self.infer_binop_type(op, span, lhs.ty, rhs.ty, res);
                if op == OpCode::Assign {
                    res.push_rt_instr(FinstKind::BinOp(op), ty, span)
                } else {
                    res.push_rt_op_and_instr(FinstKind::BinOp(op), ty, span)
                }
            }
        }
    }

    fn fold_binop(
        &mut self,
        op: OpCode,
        span: Span,
        ty: TypeRef,
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
                    *value = DeclValue::Const(new);
                }
            }
            (_, lhs, rhs) => {
                let [lhs_r, rhs_r] = [res.root_value(lhs), res.root_value(rhs)];
                let result = self.fold_resolved_binop(op, span, ty, lhs_r, rhs_r, res);
                res.push_const_op(result, span);
            }
        }
    }

    fn fold_resolved_binop(
        &mut self,
        op: OpCode,
        _span: Span,
        _ty: TypeRef,
        lhs: Resolved,
        rhs: Resolved,
        _res: &mut Res,
    ) -> Resolved {
        match (op, lhs, rhs) {
            (
                crate::op_group!(math),
                Resolved::Const(LitKindAst::Int(a)),
                Resolved::Const(LitKindAst::Int(b)),
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
                Resolved::Const(LitKindAst::Int(IntLit::new(result)))
            }
            (
                crate::op_group!(cmp),
                Resolved::Const(LitKindAst::Int(a)),
                Resolved::Const(LitKindAst::Int(b)),
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
                Resolved::Const(LitKindAst::Bool(result))
            }
            _ => unreachable!(),
        }
    }

    fn infer_binop_type(
        &mut self,
        op: OpCode,
        span: Span,
        lhs: TypeRef,
        rhs: TypeRef,
        res: &mut Res,
    ) -> TypeRef {
        let root = self.unify_subs(lhs, rhs, span, res);
        match op {
            lexer::op_group!(math) => root,
            lexer::op_group!(cmp) => res.intern_and_create_group(&BuiltInType::Bool.into()),
            crate::op_group!(logic) => todo!(),
            lexer::op_group!(assign) => res.intern_and_create_group(&BuiltInType::Unit.into()),
        }
    }

    fn eval_call(&mut self, ac: ArgCount, span: Span, res: &mut Res) {
        let Some(OperandValue::Const(Resolved::Def(id))) = res.stack.pop().and_then(|o| o.value)
        else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "cannot call this expression (yet)",
            );
            return;
        };

        let Def {
            kind:
                DefKind::Func {
                    arg_count: _,
                    signature_len,
                },
            ..
        } = self.instrs.get_def(id)
        else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "cannot call this expression (yet)",
            );
            return;
        };

        let view = self.instrs.body_of(id);
        let (params, ret) = self.eval_signature(id, signature_len, view, span);

        let mut entry = Entry {
            id,
            ip: signature_len,
            inputs: params.iter().cloned().map(Local::Rt).collect(),
            ret: ret.clone(),
        };

        let param_range = res.stack.len() - ac as usize..;

        let operands = res.stack.drain(param_range.clone()).collect::<Vec<_>>();

        for ((param, operand), local) in params.iter().zip(operands).zip(&mut entry.inputs).rev() {
            let param_ty = res.intern_and_create_group(param);
            self.unify_subs(param_ty, operand.ty, span, res);
            if let Some(value) = &operand.value {
                if res.this != Some(id) {
                    *local = Local::Ct(res.root_value(value.clone()));
                } else {
                    res.emmit_op_const(&operand)
                }
            }
        }

        if entry.inputs.iter().all(|l| matches!(l, Local::Ct(_))) {
            // TODO: Save the ir so we dont recompute it later
            if let EvalResult::Const(value) = self.eval(&entry) {
                res.push_const_op(value, span);
                return;
            }
        }

        let inst_id = self.instances.project_entry(entry);

        let ty = res.intern_and_create_group(&ret);
        res.push_rt_op_and_instr(FinstKind::Call(inst_id), ty, span)
    }

    fn unify_subs(&mut self, a: TypeRef, b: TypeRef, span: Span, res: &mut Res) -> TypeRef {
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
                res.builtin_group(BuiltInType::Unknown)
            }
        }
    }

    fn eval_field(&mut self, field: CtxSym, span: Span, res: &mut Res) {
        let field = &res.defs.idents[field];

        let operand = res.stack.pop().unwrap();
        let Some(value) = operand.value else {
            let Type::Record(kind, id) = res.get_subs(operand.ty) else {
                todo!(
                    "error handling, {} is not a record",
                    res.get_subs(operand.ty)
                );
            };

            if kind != RecordKind::Prod {
                todo!("handle other record types");
            }

            let record = &self.types[id];
            let Some(field) = record.field_id_of(field.as_str()) else {
                todo!("error handling");
            };

            let ty = res.intern_and_create_group(&record.fields[field as usize].ty);
            res.push_rt_op_and_instr(FinstKind::Field(field), ty, span);
            return;
        };

        match res.root_value(value) {
            Resolved::Type(t) => {
                let result = match field.as_str() {
                    "size" => Layout::from_ty(&t, self.arch, self.types).size(),
                    "align" => Layout::from_ty(&t, self.arch, self.types).expanded_align(),
                    _ => {
                        self.diags
                            .builder(self.modules.files())
                            .footer(Severty::Error, "no such field on the type")
                            .annotation(Severty::Error, span, "accessed here");
                        return;
                    }
                };
                res.push_const_op(
                    Resolved::Const(LitKindAst::Int(IntLit::new(result as u64))),
                    span,
                );
            }
            Resolved::Module(ns) => {
                let decls = self.instrs.decls_of(ns);
                let Some(index) = decls.lookup_def(field.as_str()) else {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "no such field on the module")
                        .annotation(Severty::Error, span, "accessed here");
                    return;
                };

                let resolved = self.access_module_decl(ns, index);

                res.push_const_op(resolved, span);
            }
            Resolved::Const(_) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "no such field on the constant")
                    .annotation(Severty::Error, span, "accessed here");
            }
            Resolved::Def(_) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "no such field on the definition")
                    .annotation(Severty::Error, span, "accessed here");
            }
        }
    }

    pub fn access_module_decl(&mut self, ns: ModuleRef, index: DefRef) -> Resolved {
        match self.instances.decls[ns].computed[index].clone() {
            Some(r) => r,
            None => {
                let id = DefId { ns, index };
                let entry = Entry {
                    id,
                    ip: 0,
                    inputs: vec![],
                    ret: BuiltInType::Unknown.into(),
                };
                let EvalResult::Const(value) = self.eval(&entry) else {
                    todo!("error handling");
                };
                self.instances.decls[ns].computed[index] = Some(value.clone());
                value
            }
        }
    }

    fn eval_decl(
        &mut self,
        decl_span: Span,
        mutable: bool,
        inited: bool,
        typed: bool,
        res: &mut Res,
    ) {
        let provided_ty = if typed {
            let Operand { value, .. } = res.stack.pop().unwrap();
            let value = match value.map(|v| res.root_value(v)) {
                Some(Resolved::Type(ty)) => ty,
                v => todo!("error handling {v:?}"),
            };
            res.intern_and_create_group(&value)
        } else {
            res.builtin_group(BuiltInType::Unknown)
        };

        let (value, ty, span) = if inited {
            let Operand {
                value, ty, span, ..
            } = res.stack.pop().unwrap();
            let ty = self.unify_subs(ty, provided_ty, span, res);
            (value, ty, span)
        } else {
            res.push_rt_instr(FinstKind::Uninit, provided_ty, decl_span);
            (None, provided_ty, decl_span)
        };

        let value = if let Some(resolved) = value {
            DeclValue::Const(res.root_value(resolved))
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
                    DeclValue::Runtime(index) => Some(index + 1),
                    _ => None,
                })
                .unwrap_or(0);

            DeclValue::Runtime(next_index)
        };

        res.decls.push(Decl {
            value,
            mutable,
            ty,
            span,
        });
    }

    fn eval_drop(&mut self, span: Span, res: &mut Res) {
        if res.stack.pop().map_or(false, |o| o.value.is_none()) {
            let ty = res.builtin_group(BuiltInType::Unit);
            res.ir.instrs.push(Finst {
                kind: FinstKind::Drop,
                ty,
                span,
            });
        }
    }

    fn eval_drop_scope(&mut self, size: FrameSize, ret: Returns, span: Span, res: &mut Res) {
        if ret {
            res.copy_last_op(false);
        }

        let final_drop_size = res
            .decls
            .drain(res.decls.len() - size as usize..)
            .filter(|d| matches!(d.value, DeclValue::Runtime(_)))
            .count();

        if final_drop_size == 0 {
            return;
        }

        let ty = res.builtin_group(BuiltInType::Unit);
        res.ir.instrs.push(Finst {
            kind: FinstKind::DropScope(final_drop_size as FrameSize, ret),
            ty,
            span,
        });

        //self.log_stack("drop scope", span, res);
    }

    fn end_control_flow(&mut self, ty: TypeRef, span: Span, res: &mut Res) -> TypeRef {
        if mem::take(&mut res.terminated) {
            res.intern_and_create_group(&Type::BuiltIn(BuiltInType::Never))
        } else {
            let last_instr = res.copy_last_op(true);
            self.unify_subs(last_instr, ty, span, res)
        }
    }

    #[allow(unused)]
    fn log_stack(&mut self, arg: impl Display, span: Span, res: &mut Res) {
        let stack = res
            .stack
            .clone()
            .into_iter()
            .map(|op| res.get_subs(op.ty))
            .map(|t| t.to_string())
            .collect::<Vec<_>>();
        self.diags
            .builder(self.modules.files())
            .footer(Severty::Note, arg)
            .annotation(Severty::Note, span, "here")
            .footer(Severty::Note, format_args!("stack: {stack:?}"));
    }
}

#[derive(Default)]
struct Res<'a> {
    module: ModuleRef,
    params: Vec<Local>,
    body: InstrBody<'a>,
    defs: ModuleDecl<'a>,
    this: Option<DefId>,
    return_ty: TypeRef,

    decls: Vec<Decl>,
    stack: Vec<Operand>,
    subs_table: SubsTable<SubsRepr>,
    type_mapping: ShadowStore<Type, SubsRepr, TypeRef>,
    ip: usize,
    type_group_seq: SubsRepr,
    terminated: bool,
    is_runtime: bool,

    if_stack: Vec<If>,

    ir: Ir,
}

impl<'a> Res<'a> {
    fn new(
        module: ModuleRef,
        body: InstrBody<'a>,
        decls: ModuleDecl<'a>,
        return_ty: &Type,
    ) -> Self {
        let mut s = Self {
            module,
            body,
            defs: decls,
            ..Default::default()
        };

        s.return_ty = s.intern_and_create_group(return_ty);

        s
    }

    fn builtin_group(&mut self, ty: BuiltInType) -> TypeRef {
        self.create_group(ty.into_ref())
    }

    fn create_group(&mut self, ty: TypeRef) -> TypeRef {
        let id = self.type_group_seq;
        self.type_group_seq += 1;
        let id = TypeRef::from_repr(id);
        if ty != TypeRef::default() {
            self.type_mapping[id] = ty;
        }
        id
    }

    fn intern_ty(&mut self, ty: &Type) -> TypeRef {
        self.ir.intern_ty(ty)
    }

    fn intern_and_create_group(&mut self, ty: &Type) -> TypeRef {
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

    fn get_subs(&mut self, ty: TypeRef) -> Type {
        let ty = self.subs_table.root_of_ref(ty);
        let ty = self.type_mapping[ty];
        self.ir.get_ty(ty)
    }

    fn set_subs(&mut self, ty: TypeRef, new: Type) {
        let ty = self.subs_table.root_of_ref(ty);
        self.type_mapping[ty] = self.ir.intern_ty(&new);
    }

    fn root_value(&self, value: OperandValue) -> Resolved {
        match value {
            OperandValue::Const(value) => value,
            OperandValue::Decl(index) => match self.decls[index as usize].value.clone() {
                DeclValue::Const(r) => r,
                DeclValue::Runtime(_) => unreachable!("because decl always points to const"),
            },
        }
    }

    fn copy_last_op(&mut self, no_const: bool) -> TypeRef {
        let mut op = self.stack.pop().unwrap();
        op.value = op.value.map(|v| OperandValue::Const(self.root_value(v)));
        self.emmit_op_const(&op);
        let ty = op.ty;
        if no_const {
            op.value = None;
        }
        self.stack.push(op);
        ty
    }

    fn emmit_op_const(&mut self, op: &Operand) {
        if let Some(OperandValue::Const(Resolved::Const(value))) = &op.value {
            self.ir.instrs.insert(
                op.pos,
                Finst {
                    kind: FinstKind::Const(value.clone()),
                    ty: op.ty,
                    span: op.span,
                },
            );
        }
    }

    fn push_rt_op(&mut self, ty: TypeRef, span: Span) {
        self.stack.push(Operand {
            pos: self.ir.instrs.len(),
            value: None,
            ty,
            span,
        });
    }

    fn push_rt_op_and_instr(&mut self, kind: FinstKind, ty: TypeRef, span: Span) {
        self.push_rt_op(ty, span);
        self.ir.instrs.push(Finst { kind, ty, span });
    }

    fn push_const_op(&mut self, value: Resolved, span: Span) {
        let ty = self.intern_and_create_group(&value.ty().into());
        self.stack.push(Operand {
            pos: self.ir.instrs.len(),
            value: Some(OperandValue::Const(value)),
            ty,
            span,
        });
    }

    fn push_rt_instr(&mut self, kind: FinstKind, ty: TypeRef, span: Span) {
        self.ir.instrs.push(Finst { kind, ty, span });
    }
}

#[derive(Clone, Debug)]
enum DeclValue {
    Const(Resolved),
    Runtime(usize),
}

#[derive(Clone, Debug)]
struct Decl {
    value: DeclValue,
    mutable: bool,
    span: Span,
    ty: TypeRef,
}

#[derive(Clone, Debug)]
enum OperandValue {
    Const(Resolved),
    Decl(usize),
}

#[derive(Clone, Debug)]
struct Operand {
    pos: usize,
    value: Option<OperandValue>,
    ty: TypeRef,
    span: Span,
}

enum If {
    Const,
    Runtime(TypeRef),
}

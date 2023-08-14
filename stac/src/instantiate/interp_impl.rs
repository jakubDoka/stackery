use crate::{
    lexer, ArgCount, BuiltInType, CtxSym, Finst, FinstKind, FrameSize, FuncId, FuncType, InstrBody,
    InstrKind, IntLit, IrTypes, LitKindAst, ModuleDecl, ModuleRef, Mutable, OpCode, Resolved,
    Returns, Severty, ShadowStore, Span, SubsRef, SubsTable, Sym, Type, TypeRef, UnificationError,
};

use super::{Entry, Interpreter, Ir, Layout, Local, SubsRepr};

#[derive(Clone, Debug)]
enum DeclValue {
    Const(Resolved),
    Runtime(usize),
}

struct Decl {
    value: DeclValue,
    mutable: bool,
    span: Span,
    ty: SubsRef,
}

#[derive(Clone, Debug)]
enum OperandValue {
    AsInstr,
    Decl(usize),
    Module(ModuleRef),
    Type(TypeRef),
    Func(FuncId),
}

#[derive(Clone, Debug)]
struct Operand {
    value: OperandValue,
    ty: SubsRef,
}

#[derive(Default)]
struct Res<'a> {
    _module: ModuleRef,
    enfoce_const: bool,
    params: Vec<Local>,
    body: InstrBody<'a>,
    _decls: ModuleDecl<'a>,

    decls: Vec<Decl>,
    stack: Vec<Operand>,
    subs_table: SubsTable<SubsRepr>,
    type_mapping: ShadowStore<Type, SubsRepr, TypeRef>,
    ip: usize,
    type_group_seq: SubsRepr,

    ir: Ir,
    call_frontier: Vec<Entry>,
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

    fn pop_operand(&mut self) -> (SubsRef, Option<Result<Resolved, usize>>) {
        let operand = self.stack.pop().unwrap();
        let res = match operand.value {
            OperandValue::Func(f) => Resolved::Type(Type::Func(FuncType::Static(f))),
            OperandValue::Module(m) => Resolved::Module(m),
            OperandValue::Decl(u) => return (operand.ty, Some(Err(u))),
            OperandValue::Type(t) => Resolved::Type(self.ir.get_ty(t)),
            OperandValue::AsInstr => match self.ir.instrs.pop().unwrap() {
                Finst {
                    kind: FinstKind::Const(c),
                    ..
                } => Resolved::Const(c),
                instr => {
                    self.ir.instrs.push(instr);
                    return (operand.ty, None);
                }
            },
        };

        (operand.ty, Some(Ok(res)))
    }

    fn pop_operand_as_value(&mut self) -> (SubsRef, Option<Resolved>) {
        let (ty, res) = self.pop_operand();
        let res = res.and_then(|r| match r {
            Ok(r) => Some(r),
            Err(u) => match self.decls[u as usize].value.clone() {
                DeclValue::Const(c) => Some(c),
                DeclValue::Runtime(_) => None,
            },
        });
        (ty, res)
    }

    fn push_res(&mut self, ty: SubsRef, r: Resolved, span: Span) {
        match r {
            Resolved::Type(Type::Func(FuncType::Static(f))) => self.stack.push(Operand {
                value: OperandValue::Func(f),
                ty,
            }),
            Resolved::Type(t) => self.stack.push(Operand {
                value: OperandValue::Type(self.ir.intern_ty(&t)),
                ty,
            }),
            Resolved::Module(m) => self.stack.push(Operand {
                value: OperandValue::Module(m),
                ty,
            }),
            Resolved::Const(c) => {
                self.stack.push(Operand {
                    value: OperandValue::AsInstr,
                    ty,
                });
                self.ir.instrs.push(Finst {
                    kind: FinstKind::Const(c),
                    ty,
                    span,
                });
            }
        }
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

        let (_, Some(r)) = res.pop_operand_as_value() else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "expected result from expression")
                .annotation(Severty::Error, span, "this expression does not evaluate");
            return None;
        };

        Some(r)
    }

    pub fn eval(&mut self, entry: &Entry, call_frontier: &mut Vec<Entry>) -> Ir {
        let mut res = Res::new(
            entry.id.module,
            false,
            self.instrs.body_of(entry.id),
            self.instrs.decls_of(entry.id.module),
        );

        res.params = entry.inputs.clone();

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span.to_span(entry.id.module), &mut res);
        }

        if let Some(operand) = res.stack.pop() {
            let ret = res.intern_and_create_group(&entry.ret);
            self.unify_subs(
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
        }

        call_frontier.append(&mut res.call_frontier);

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
            InstrKind::Module(m) => self.eval_res(Resolved::Module(m), span, res),
            InstrKind::Const(c) => {
                self.eval_res(Resolved::Const(res.body.consts[c].clone()), span, res)
            }
            InstrKind::Type(t) => self.eval_res(Resolved::Type(res.body.get_ty(t)), span, res),
        }
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
            },
        }
    }

    fn eval_res(&mut self, r: Resolved, span: Span, res: &mut Res) {
        let typ = self.res_ty(&r, res);
        let ty = res.intern_and_create_group(&typ);
        res.push_res(ty, r, span);
    }

    fn eval_sym(&mut self, sym: Sym, span: Span, res: &mut Res) {
        if res.params.len() > sym as usize {
            let local = res.params[sym as usize].clone();
            match local {
                Local::Ct(c) => self.eval_res(c, span, res),
                Local::Rt(_) if res.enfoce_const => {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "cannot access runtime local in const eval")
                        .annotation(Severty::Error, span, "runtime local accessed here");
                    return;
                }
                Local::Rt(ty) => {
                    let ty = res.intern_and_create_group(&ty);
                    res.stack.push(Operand {
                        value: OperandValue::AsInstr,
                        ty,
                    });
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
            ref value,
            mutable,
            ty,
            ..
        } = res.decls[operand_index];

        match value {
            DeclValue::Const(r) => {
                if mutable {
                    res.stack.push(Operand {
                        value: OperandValue::Decl(operand_index),
                        ty,
                    });
                } else {
                    self.eval_res(r.clone(), span, res)
                }
            }
            &DeclValue::Runtime(index) => {
                res.stack.push(Operand {
                    value: OperandValue::AsInstr,
                    ty,
                });
                res.ir.instrs.push(Finst {
                    kind: FinstKind::Sym(index as Sym),
                    ty,
                    span,
                });
            }
        }
    }

    fn eval_binop(&mut self, op: OpCode, span: Span, res: &mut Res) {
        let [(rty, rhs), (lty, lhs)] = [res.pop_operand(), res.pop_operand()];

        match (rhs, lhs) {
            (Some(rhs), Some(lhs)) => self.fold_binop(op, span, [lhs, rhs], res),
            (rhs, _) => {
                if let Some(Ok(rhs)) = rhs {
                    res.push_res(rty, rhs, span);
                }
                let ty = self.infer_binop_type(op, span, rty, lty, res);
                res.stack.push(Operand {
                    value: OperandValue::AsInstr,
                    ty,
                });
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
        [lhs, rhs]: [Result<Resolved, usize>; 2],
        res: &mut Res,
    ) {
        let result = match (op, lhs, rhs) {
            (OpCode::Assign, Err(sym), Ok(new)) => 'b: {
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
                    break 'b None;
                }

                *value = DeclValue::Const(new);

                None
            }
            (_, lhs, rhs) => {
                let [lhs_r, rhs_r] = [self.root_value(lhs, res), self.root_value(rhs, res)];
                self.fold_resolved_binop(op, span, lhs_r, rhs_r)
            }
        };

        let Some(result) = result else {
            return;
        };

        self.eval_res(result, span, res);
    }

    fn fold_resolved_binop(
        &mut self,
        op: OpCode,
        span: Span,
        lhs: Resolved,
        rhs: Resolved,
    ) -> Option<Resolved> {
        match (lhs, rhs) {
            (Resolved::Const(LitKindAst::Int(a)), Resolved::Const(LitKindAst::Int(b))) => {
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
                    _ => {
                        self.diags.todo_error(
                            self.modules.files(),
                            span,
                            "operators with non integer output are not handeled yet",
                        )?;
                    }
                };
                Some(Resolved::Const(LitKindAst::Int(IntLit::new(result))))
            }
            _ => {
                self.diags.todo_error(
                    self.modules.files(),
                    span,
                    "non integer operands are not handeled yet",
                )?;
            }
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

    fn eval_call(&mut self, _ac: ArgCount, _span: Span, _res: &mut Res) {
        //let func = res.stack.pop().unwrap();
        //let args_range = res.stack.len() - ac as usize..;

        //let Type::Func(func_ty) = &res.types[func.ty] else {
        //    self.diags
        //        .builder(self.modules.files())
        //        .footer(Severty::Error, "cannot call this expression")
        //        .annotation(Severty::Error, span, "this expression is not callable");
        //    return;
        //};

        //let sig = match func_ty {
        //    &FuncType::Static(func) => self.instrs.sig_of(func),
        //    FuncType::Dynamic(func) => &func,
        //};

        //if let &FuncType::Static(func) = func_ty {
        //    let inputs = res.stack[args_range.clone()]
        //        .iter()
        //        .map(|op| {
        //            if op.runtime {
        //                Local::Rt(res.types[op.ty].clone())
        //            } else {
        //                Local::Ct(self.root_value(res.ir.instrs[op.start].0.clone(), res))
        //            }
        //        })
        //        .collect::<Vec<_>>();

        //    let entry = Entry {
        //        id: func,
        //        inputs: inputs.clone(),
        //        ret: sig.ret.clone(),
        //    };

        //    res.call_frontier.push(entry);
        //}

        //if sig.args.len() != ac as usize {
        //    self.diags
        //        .builder(self.modules.files())
        //        .footer(Severty::Error, "wrong number of arguments")
        //        .annotation(
        //            Severty::Error,
        //            span,
        //            format_args!(
        //                "expected {} arguments, found {}",
        //                sig.args.len(),
        //                ac as usize
        //            ),
        //        );
        //    return;
        //}

        //let has_side_effects = true; // TODO: use value from the type
        //if func.runtime
        //    || has_side_effects
        //    || res.stack[args_range.clone()].iter().any(|a| a.runtime)
        //{
        //    let base_operand = res.stack.drain(args_range).next().unwrap_or(func);

        //    let ty = res.create_group(sig.ret.clone());
        //    res.stack.push(Operand {
        //        start: base_operand.start,
        //        runtime: true,
        //        mutable: false,
        //        ty,
        //    });

        //    res.ir.instrs.push((InstrKind::Call(ac), ty));

        //    return;
        //}

        todo!()
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
        let (_, operand) = res.pop_operand();
        let Some(Ok(resolved)) = operand else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "runtime field access not supported yet",
            );
            return;
        };

        let field = &res.body.idents[field];

        match resolved {
            Resolved::Type(t) => {
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
                self.eval_res(
                    Resolved::Const(LitKindAst::Int(IntLit::new(result as u64))),
                    span,
                    res,
                );
            }
            Resolved::Module(m) => {
                let decls = self.instrs.decls_of(m);
                let Some(item) = decls.decls.iter().rfind(|d| d.name.ident == *field) else {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "no such field on the module")
                        .annotation(Severty::Error, span, "accessed here");
                    return;
                };
                self.eval_res(item.data.clone(), span, res);
            }
            Resolved::Const(_) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "no such field on the constant")
                    .annotation(Severty::Error, span, "accessed here");
            }
        }
    }

    fn eval_decl(&mut self, span: Span, mutable: Mutable, res: &mut Res) {
        let (ty, resolved) = res.pop_operand_as_value();

        if resolved.is_none() && mutable == Mutable::CtTrue {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "cannot declare a runtime variable as compile time",
                )
                .annotation(Severty::Error, span, "declared here");
            return;
        }

        let decl_value = if mutable != Mutable::True && let Some(resolved) = resolved {
            DeclValue::Const(resolved)
        } else {
            if let Some(r) = resolved {
                res.push_res(ty, r, span)
            }
            
            res.ir.instrs.push(Finst {
                kind: FinstKind::Decl(mutable == Mutable::True),
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
            value: decl_value,
            mutable: mutable != Mutable::False,
            ty,
            span,
        });
    }

    fn eval_drop(&mut self, span: Span, res: &mut Res) {
        if res.pop_operand().1.is_some() {
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
            let (ty, val) = res.pop_operand_as_value();
            if let Some(val) = val {
                res.push_res(ty, val, span);
            } else {
                res.stack.push(Operand {
                    value: OperandValue::AsInstr,
                    ty,
                })
            }
        }

        let final_drop_size = res
            .decls
            .drain(res.decls.len() - size as usize..)
            .filter(|d| matches!(d.value, DeclValue::Runtime(_)))
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

    fn root_value(&mut self, instr: Result<Resolved, usize>, res: &Res) -> Resolved {
        match instr {
            Err(s) => match res.decls[s].value.clone() {
                DeclValue::Runtime(_) => unreachable!(),
                DeclValue::Const(v) => v,
            },
            Ok(r) => r,
        }
    }
}

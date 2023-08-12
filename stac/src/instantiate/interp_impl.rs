use std::array;

use mini_alloc::IdentStr;

use crate::{
    lexer, BuiltInType, FrameSize, InstrBody, InstrKind, IntLit, LitKindAst, ModuleDecl, ModuleRef,
    Mutable, OpCode, Resolved, Returns, Severty, Span, SubsRef, SubsTable, Sym, Type, VecStore,
};

use super::{Entry, Interpreter, Ir, Layout, Local, SubsRepr};

struct Decl {
    _offset: usize,
    _span: Span,
}

#[derive(Clone, Copy, Debug)]
struct Operand {
    start: usize,
    runtime: bool,
    mutable: bool,
    ty: SubsRef,
}

#[derive(Default)]
struct Res<'a> {
    module: ModuleRef,
    ir: Ir,
    stack: Vec<Operand>,
    types: VecStore<Type, SubsRepr>,
    params: Vec<Local>,
    ip: usize,
    body: InstrBody<'a>,
    decls: ModuleDecl<'a>,
    subs_table: SubsTable<SubsRepr>,
    enfoce_const: bool,
    scope: Vec<Decl>,
}

impl<'a> Res<'a> {
    fn push_type(&mut self, ty: Type) -> SubsRef {
        self.types.push(ty)
    }

    fn push_instr(&mut self, ty: SubsRef, runtime: bool, mutable: bool, kind: InstrKind) {
        self.stack.push(Operand {
            start: self.ir.instrs.len(),
            runtime,
            mutable,
            ty,
        });
        self.ir.instrs.push((kind, ty));
    }

    fn remap_types(mut self) -> Ir {
        self.ir.instrs.iter_mut().for_each(|(_, ty)| {
            *ty = self.subs_table.root_of_ref(*ty);
        });

        self.ir.instrs.iter_mut().for_each(|(_, ty)| {
            let tyv = &self.types[*ty];
            let id = self
                .ir
                .types
                .iter()
                .find_map(|(id, ty)| if ty == tyv { Some(id) } else { None });
            *ty = id.unwrap_or_else(|| self.ir.types.push(tyv.clone()));
        });

        self.ir
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

        let mut res = Res {
            body,
            decls,
            enfoce_const: true,
            module,
            ..Default::default()
        };

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span, &mut res);
        }

        if self.diags.error_count() > prev_count {
            return None;
        }

        let Some((InstrKind::Res(r), _)) = res.ir.instrs.pop() else {
            self.diags
                .builder(self.modules.files())
                .footer(Severty::Error, "expected result from expression")
                .annotation(Severty::Error, span, "this expression does not evaluate");
            return None;
        };

        Some(r)
    }

    pub fn eval(&mut self, entry: Entry) -> Ir {
        let mut res = Res {
            module: entry.module,
            params: entry.inputs,
            body: self.instrs.body_of(entry.module, entry.func),
            decls: self.instrs.decls_of(entry.module),
            ..Default::default()
        };

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span, &mut res);
        }

        if let Some(operand) = res.stack.pop() {
            let ret_ty = res.subs_table.root_of_ref(operand.ty);
            if let Some(ty) = self.unify_types(
                &res.types[ret_ty],
                &entry.ret,
                res.body.instrs.last().unwrap().span,
            ) {
                res.types[ret_ty] = ty;
            }
        }

        res.remap_types()
    }

    fn eval_instr(&mut self, instr: &InstrKind, span: Span, res: &mut Res) {
        match instr {
            InstrKind::Uninit(ty) => self.eval_uninit(ty, res),
            InstrKind::Res(r) => self.eval_res(r.clone(), res),
            &InstrKind::Sym(s) => self.eval_sym(s, span, res),
            &InstrKind::BinOp(op) => self.eval_binop(op, span, res),
            InstrKind::Field(field) => self.eval_field(field, span, res),
            &InstrKind::Decl(mutable) => self.eval_decl(span, mutable, res),
            InstrKind::Drop => self.eval_drop(span, res),
            &InstrKind::DropScope(s, r) => self.eval_drop_scope(s, r, span, res),
            InstrKind::PaddingDecl => unreachable!("we dont insert these during emmit time"),
        }
    }

    fn eval_uninit(&mut self, typ: &Type, res: &mut Res) {
        let ty = res.push_type(typ.clone());
        res.stack.push(Operand {
            start: res.ir.instrs.len(),
            runtime: false,
            mutable: false,
            ty,
        });
        res.ir.instrs.push((InstrKind::Uninit(typ.clone()), ty));
    }

    fn res_ty(&mut self, c: &Resolved, res: &mut Res) -> Type {
        match c {
            Resolved::Type(_) => Type::BuiltIn(BuiltInType::Type),
            &Resolved::Func(f) => {
                let sig = if f.module == res.module {
                    &res.decls.funcs[f.func].signature
                } else {
                    self.instrs.sig_of(f)
                };
                Type::Func(Box::new(sig.clone()))
            }
            Resolved::Module(_) => Type::BuiltIn(BuiltInType::Module),
            Resolved::Const(c) => match c {
                LitKindAst::Int(_) => Type::BuiltIn(BuiltInType::Integer),
            },
        }
    }

    fn eval_res(&mut self, c: Resolved, res: &mut Res) {
        let typ = self.res_ty(&c, res);

        let ty = res.push_type(typ);
        res.stack.push(Operand {
            start: res.ir.instrs.len(),
            runtime: false,
            mutable: false,
            ty,
        });
        res.ir.instrs.push((InstrKind::Res(c), ty));
    }

    fn eval_sym(&mut self, sym: Sym, span: Span, res: &mut Res) {
        if res.params.len() > sym as usize {
            let local = res.params[sym as usize].clone();
            match local {
                Local::Ct(c) => self.eval_res(c, res),
                Local::Rt(_) if res.enfoce_const => {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "cannot access runtime local in const eval")
                        .annotation(Severty::Error, span, "runtime local accessed here");
                    return;
                }
                Local::Rt(ty) => {
                    let ty = res.push_type(ty);
                    res.stack.push(Operand {
                        start: res.ir.instrs.len(),
                        runtime: true,
                        mutable: false,
                        ty,
                    });
                    res.ir.instrs.push((InstrKind::Sym(sym), ty));

                    res.push_instr(ty, true, false, InstrKind::Sym(sym));
                }
            };
            return;
        }

        let operand_index = sym as usize - res.params.len();

        let Some(Operand {
            start,
            runtime,
            mutable,
            ty,
        }) = res.stack.get(operand_index).copied()
        else {
            println!("{}", self.diags.view());
            unreachable!();
        };

        if !runtime && !mutable {
            let (instr, ty) = res.ir.instrs[start].clone();
            res.push_instr(ty, false, false, instr);
        } else {
            res.push_instr(ty, runtime, mutable, InstrKind::Sym(sym));
        }
    }

    fn eval_binop(&mut self, op: OpCode, span: Span, res: &mut Res) {
        let [lhs, rhs] = pop_array(&mut res.stack);
        if lhs.runtime || rhs.runtime {
            res.stack.push(lhs);

            let ty = self.infer_binop_type(op, span, lhs.ty, rhs.ty, res);
            res.push_instr(ty, true, false, InstrKind::BinOp(op));

            return;
        }

        self.fold_binop(op, span, lhs, rhs, res);
    }

    fn fold_binop(&mut self, op: OpCode, span: Span, _lhs: Operand, _rhs: Operand, res: &mut Res) {
        let [lhs_i, rhs_i] = pop_array(&mut res.ir.instrs).map(|(i, _)| i);

        let result = match (op, lhs_i, rhs_i) {
            (OpCode::Assign, InstrKind::Sym(s), InstrKind::Res(rhs_r)) => 'b: {
                let base_operand = &res.stack[s as usize];

                if !base_operand.mutable {
                    self.diags
                        .builder(self.modules.files())
                        .footer(Severty::Error, "cannot assign to immutable symbol")
                        .annotation(Severty::Error, span, "assignment happens here");
                    break 'b None;
                }

                // TODO: check type
                res.ir.instrs[base_operand.start].0 = InstrKind::Res(rhs_r);

                None
            }
            (_, lhs_i, rhs_i) => {
                let [lhs_r, rhs_r] = [self.root_value(lhs_i, res), self.root_value(rhs_i, res)];
                self.fold_resolved_binop(op, span, lhs_r, rhs_r)
            }
        };

        let Some(result) = result else {
            return;
        };

        self.eval_res(result, res);
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
        let lhs_ty = &res.types[lhs];
        let rhs_ty = &res.types[rhs];

        match op {
            lexer::op_group!(math) => {
                let merged_type = self.unify_types(lhs_ty, rhs_ty, span);
                let root = res.subs_table.join_refs(lhs, rhs);
                if let Some(merged_type) = merged_type {
                    res.types[root] = merged_type;
                }
                root
            }
            lexer::op_group!(cmp) => {
                let merged_type = self.unify_types(lhs_ty, rhs_ty, span);
                let root = res.subs_table.join_refs(lhs, rhs);
                if let Some(merged_type) = merged_type {
                    res.types[root] = merged_type;
                }
                res.push_type(Type::BuiltIn(BuiltInType::Bool))
            }
            crate::op_group!(logic) => todo!(),
            lexer::op_group!(assign) => {
                let merged_type = self.unify_types(lhs_ty, rhs_ty, span);
                let root = res.subs_table.join_refs(lhs, rhs);
                if let Some(merged_type) = merged_type {
                    res.types[root] = merged_type;
                }
                res.push_type(Type::BuiltIn(BuiltInType::Unit))
            }
        }
    }

    fn unify_types(&mut self, a: &Type, b: &Type, span: Span) -> Option<Type> {
        if let Some(merged_type) = a.unify(b) {
            return Some(merged_type);
        }

        self.diags
            .builder(self.modules.files())
            .footer(Severty::Error, "cannot unify types")
            .annotation(Severty::Error, span, "of this binary operation")
            .terminate()?;
    }

    fn eval_field(&mut self, field: &IdentStr, span: Span, res: &mut Res) {
        let [operand] = pop_array(&mut res.stack);
        if operand.runtime {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "runtime field access not supported yet",
            );
            return;
        }

        let [(InstrKind::Res(resoved), ..)] = pop_array(&mut res.ir.instrs) else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "non resolved operands are not handeled yet",
            );
            return;
        };

        match resoved {
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
                    res,
                );
            }
            Resolved::Func(..) => {
                self.diags
                    .builder(self.modules.files())
                    .footer(Severty::Error, "no such field on the function")
                    .annotation(Severty::Error, span, "accessed here");
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
                self.eval_res(item.data.clone(), res);
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
        let operand = res.stack.last_mut().unwrap();
        operand.mutable = mutable != Mutable::False;
        operand.runtime |= mutable == Mutable::True;

        if operand.runtime && mutable == Mutable::CtTrue {
            self.diags
                .builder(self.modules.files())
                .footer(
                    Severty::Error,
                    "cannot declare a runtime variable as compile time",
                )
                .annotation(Severty::Error, span, "declared here");
            return;
        }

        if operand.runtime {
            let ty = res.push_type(Type::BuiltIn(BuiltInType::Unit));
            res.ir.instrs.push((InstrKind::Decl(mutable), ty));
        } else {
            let (instr, ty) = res.ir.instrs.pop().unwrap();
            let value = self.root_value(instr, res);
            res.ir.instrs.push((InstrKind::Res(value), ty));
        }

        res.scope.push(Decl {
            _offset: res.stack.len(),
            _span: span,
        });
    }

    fn eval_drop(&mut self, _span: Span, res: &mut Res) {
        if res.stack.pop().unwrap().runtime {
            let ty = res.push_type(Type::BuiltIn(BuiltInType::Unit));
            res.ir.instrs.push((InstrKind::Drop, ty));
        }
    }

    fn eval_drop_scope(&mut self, size: FrameSize, ret: Returns, _span: Span, res: &mut Res) {
        if ret {
            let last = res.stack.last().unwrap();
            if !last.runtime {
                let (instr, ty) = res.ir.instrs.pop().unwrap();
                let value = self.root_value(instr, res);
                res.ir.instrs.push((InstrKind::Res(value), ty));
            }
        }

        res.stack
            .drain(res.stack.len() - size as usize - ret as usize..res.stack.len() - ret as usize)
            .rev()
            .filter(|r| !r.runtime)
            .for_each(|r| {
                res.ir.instrs[r.start].0 = InstrKind::PaddingDecl;
            });

        let ty = res.push_type(Type::BuiltIn(BuiltInType::Unit));
        res.ir
            .instrs
            .push((InstrKind::DropScope(size as FrameSize, ret), ty));
    }

    fn root_value(&mut self, instr: InstrKind, res: &mut Res) -> Resolved {
        match instr {
            InstrKind::Sym(s) => {
                let operand = res.stack[s as usize];
                assert!(!operand.runtime);
                self.root_value(res.ir.instrs[operand.start].0.clone(), res)
            }
            InstrKind::Res(r) => r,
            _ => unreachable!(),
        }
    }
}

fn pop_array<T, const N: usize>(vec: &mut Vec<T>) -> [T; N] {
    let mut iter = vec.drain(vec.len() - N..);
    array::from_fn(|_| iter.next().unwrap())
}

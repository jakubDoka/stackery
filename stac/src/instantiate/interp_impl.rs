use std::array;

use mini_alloc::IdentStr;

use crate::{
    lexer, BuiltInType, InstrBody, InstrKind, IntLit, LitKindAst, ModuleDecl, OpCode, Ref,
    Resolved, Severty, Span, SubsTable, Sym, Type, VecStore,
};

use super::{Entry, Interpreter, Ir, Layout, Local};

type SubsRepr = u16;
type SubsRef = Ref<Type, SubsRepr>;

#[derive(Clone, Copy)]
struct Operand {
    start: usize,
    runtime: bool,
    ty: SubsRef,
}

#[derive(Default)]
struct Res<'a> {
    ir: Ir,
    stack: Vec<Operand>,
    types: VecStore<Type, SubsRepr>,
    params: Vec<Local>,
    ip: usize,
    body: InstrBody<'a>,
    decls: ModuleDecl<'a>,
    subs_table: SubsTable<SubsRepr>,
    enfoce_const: bool,
}

impl<'a> Res<'a> {
    fn push_type(&mut self, ty: Type) -> SubsRef {
        self.types.push(ty)
    }
}

impl<'ctx> Interpreter<'ctx> {
    pub fn ct_eval(&mut self, span: Span, body: InstrBody, decls: ModuleDecl) -> Option<Resolved> {
        let prev_count = self.diags.error_count();

        let mut res = Res {
            body,
            decls,
            enfoce_const: true,
            ..Default::default()
        };

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span, &mut res);
        }

        if self.diags.error_count() > prev_count {
            return None;
        }

        let Some(InstrKind::Res(r)) = res.ir.instrs.pop() else {
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
            params: entry.inputs,
            body: self.instrs.body_of(entry.module, entry.func),
            decls: self.instrs.decls_of(entry.module),
            ..Default::default()
        };

        while let Some(instr) = res.body.instrs.get(res.ip) {
            res.ip += 1;
            self.eval_instr(&instr.kind, instr.span, &mut res);
        }

        res.ir
    }

    fn eval_instr(&mut self, instr: &InstrKind, span: Span, res: &mut Res) {
        match instr {
            InstrKind::Uninit(ty) => self.eval_uninit(ty, res),
            InstrKind::Res(r) => self.eval_res(r.clone(), res),
            &InstrKind::Sym(s) => self.eval_sym(s, span, res),
            &InstrKind::BinOp(op) => self.eval_binop(op, span, res),
            InstrKind::Field(field) => self.eval_field(field, span, res),
        }
    }

    fn eval_uninit(&mut self, typ: &Type, res: &mut Res) {
        let ty = res.push_type(typ.clone());
        res.stack.push(Operand {
            start: res.ir.instrs.len(),
            runtime: false,
            ty,
        });
        res.ir.instrs.push(InstrKind::Uninit(typ.clone()));
    }

    fn eval_res(&mut self, c: Resolved, res: &mut Res) {
        let typ = match &c {
            Resolved::Type(_) => Type::BuiltIn(BuiltInType::Type),
            Resolved::Func(_) => todo!(),
            Resolved::Module(_) => Type::BuiltIn(BuiltInType::Module),
            Resolved::Const(c) => match c {
                LitKindAst::Int(_) => Type::BuiltIn(BuiltInType::Integer),
            },
        };

        let ty = res.push_type(typ);
        res.stack.push(Operand {
            start: res.ir.instrs.len(),
            runtime: false,
            ty,
        });
        res.ir.instrs.push(InstrKind::Res(c));
    }

    fn eval_sym(&mut self, s: Sym, span: Span, res: &mut Res) {
        let Some(local_index) = s.index().checked_sub(res.decls.decls.len()) else {
            let resolved = res.decls.decls[s].data.clone();
            self.eval_res(resolved, res);
            return;
        };

        let Some(_stack_index) = local_index.checked_sub(res.params.len()) else {
            let local = res.params[local_index].clone();
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
                        ty,
                    });
                    res.ir.instrs.push(InstrKind::Sym(s));
                }
            };
            return;
        };

        todo!("direct access local, cannot happen yet");
    }

    fn eval_binop(&mut self, op: OpCode, span: Span, res: &mut Res) {
        let [lhs, rhs] = pop_array(&mut res.stack);
        if lhs.runtime || rhs.runtime {
            res.stack.push(lhs);
            res.ir.instrs.push(InstrKind::BinOp(op));

            let ty = self.infer_binop_type(op, span, lhs.ty, rhs.ty, res);
            res.stack.push(Operand {
                start: res.ir.instrs.len(),
                runtime: true,
                ty,
            });

            return;
        }

        self.fold_binop(op, span, lhs, res);
    }

    fn fold_binop(&mut self, op: OpCode, span: Span, lhs: Operand, res: &mut Res) {
        let [lhs_i, rhs_i] = pop_array(&mut res.ir.instrs);
        assert_eq!(lhs.start, res.ir.instrs.len());

        let [InstrKind::Res(lhs_r), InstrKind::Res(rhs_r)] = [lhs_i, rhs_i] else {
            self.diags.todo_error(
                self.modules.files(),
                span,
                "non resolved operands are not handeled yet",
            );
            return;
        };

        let result = match (lhs_r, rhs_r) {
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
                        );
                        return;
                    }
                };
                Resolved::Const(LitKindAst::Int(IntLit::new(result)))
            }
            _ => {
                self.diags.todo_error(
                    self.modules.files(),
                    span,
                    "non integer operands are not handeled yet",
                );
                return;
            }
        };

        self.eval_res(result, res);
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
                res.types[root] = merged_type;
                root
            }
            lexer::op_group!(cmp) => {
                let merged_type = self.unify_types(lhs_ty, rhs_ty, span);
                res.types[res.subs_table.join_refs(lhs, rhs)] = merged_type;
                res.push_type(Type::BuiltIn(BuiltInType::Bool))
            }
            lexer::op_group!(logic) => todo!(),
            lexer::op_group!(assign) => todo!(),
        }
    }

    fn unify_types(&mut self, a: &Type, b: &Type, span: Span) -> Type {
        if let Some(merged_type) = a.unify(b) {
            return merged_type;
        }

        self.diags
            .builder(self.modules.files())
            .footer(Severty::Error, "cannot unify types")
            .annotation(Severty::Error, span, "of this binary operation");

        Type::BuiltIn(BuiltInType::Unknown)
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

        let [InstrKind::Res(resoved)] = pop_array(&mut res.ir.instrs) else {
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
                    "size" => Layout::from_ty(&t).size(),
                    "align" => Layout::from_ty(&t).expanded_align(),
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
            Resolved::Func(_) => {
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
}

fn pop_array<T, const N: usize>(vec: &mut Vec<T>) -> [T; N] {
    let mut iter = vec.drain(vec.len() - N..);
    array::from_fn(|_| iter.next().unwrap())
}

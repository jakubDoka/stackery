use std::mem;

use mini_alloc::*;

use super::*;

struct Interpreter<'a, G> {
    types: &'a Types,
    instrs: &'a Instrs,
    modules: &'a Modules,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
    res: &'a mut TypeCheckerRes,
    generator: &'a mut G,
}

impl<'a, G: Generator> Interpreter<'a, G> {
    pub fn interpret(mut self, instace: FuncInstId<Type>) {
        let mut frame = self.start_frame(instace.downgrade(self.types));

        loop {
            match self.interpret_frame(&mut frame) {
                Ok(()) => {
                    self.end_frame(frame);

                    let Some(prev_frame) = self.res.stack.pop() else {
                        break;
                    };

                    frame = prev_frame;
                }
                Err(Interrupt::Call(func)) => {
                    let mut new_frame = self.start_frame(func);
                    mem::swap(&mut frame, &mut new_frame);
                    self.res.stack.push(new_frame);
                }
            }
        }
    }

    fn interpret_frame(&mut self, frame: &mut Frame) -> Result<(), Interrupt> {
        let view = self.instrs.view_of(frame.instance.id);
        let mut instrs = view.instrs.iter();
        instrs.by_ref().skip(frame.ip).count();

        for &instr in instrs.by_ref() {
            self.interpret_instr(instr, frame, &view)?;
        }

        frame.ip = view.instrs.len() - instrs.as_slice().len();

        Ok(())
    }

    fn interpret_instr(
        &mut self,
        instr: Instr,
        frame: &mut Frame,
        view: &FuncMetaView,
    ) -> Result<(), Interrupt> {
        match instr {
            Instr::Const(c) => Ok(self.const_(c, view)),
            Instr::Sym(s) => Ok(self.sym(s, frame, view)),
            Instr::Mod(m) => Ok(self.mod_(m)),
            Instr::Array { item_count } => todo!(),
            Instr::FilledArray => todo!(),
            Instr::Tuple { item_count } => todo!(),
            Instr::Struct { field_count } => todo!(),
            Instr::StructField { name, has_value } => todo!(),
            Instr::Embed => todo!(),
            Instr::Enum { name, has_value } => todo!(),
            Instr::Call { arg_count } => todo!(),
            Instr::Func(_) => todo!(),
            Instr::Unary(_) => todo!(),
            Instr::Binary(_) => todo!(),
            Instr::Index => todo!(),
            Instr::Decl => todo!(),
            Instr::Jump { to, conditional } => todo!(),
            Instr::BackJumpDest { used } => todo!(),
            Instr::Field { name, is_meta } => todo!(),
            Instr::Unkown => todo!(),
            Instr::Error => todo!(),
            Instr::Drop => todo!(),
            Instr::DropDecl { decl_count } => todo!(),
        }
    }

    fn block(
        &mut self,
        expr_count: usize,
        returns: bool,
        frame: &mut Frame,
        view: &FuncMetaView,
    ) -> Result<(), Interrupt> {
        Ok(())
    }

    fn mod_(&mut self, mod_: ModuleRef) {
        let ty = TypeValue::Leaf(Type::Leaf(LeafType::Module(mod_)));
        self.push_value(ty);
    }

    fn const_(&mut self, const_: Const, view: &FuncMetaView) {
        let ty = match view.consts[const_] {
            LiteralKindAst::Int(_) => BuiltinType::Int(IntType {
                signed: false,
                width: IntWidth::W32,
            }),
            LiteralKindAst::Str(_) => BuiltinType::Str,
            LiteralKindAst::Bool(..) => BuiltinType::Bool,
        };
        let ty = TypeValue::Leaf(Type::Leaf(LeafType::Builtin(ty)));

        self.push_value(ty);
    }

    fn start_frame(&mut self, func: FuncInstId<TypeValue>) -> Frame {
        let frame = Frame {
            instance: func,
            unknowns: self.res.unknowns.start_frame(),
            instr_base: self.res.instr_types.len(),
            decl_base: self.res.decl_types.len(),
            value_base: self.res.value_types.len(),
            module_item_count: self.instrs.items_of(func.id.module).len(),
            ip: 0,
        };

        self.res.decl_types.extend(
            self.types.values[func.ty.args]
                .iter()
                .map(|&ty| DeclData { index: 0, ty }),
        );

        frame
    }

    fn sym(&mut self, sym: Sym, frame: &Frame, view: &FuncMetaView) {
        let ty = self.sym_type(sym, frame);
        self.push_value(ty);
    }

    fn sym_type(&self, sym: Sym, frame: &Frame) -> TypeValue {
        let Some(local) = sym.index().checked_sub(frame.module_item_count) else {
            return self.module_item_type(frame, sym.index());
        };
        self.res.decl_types[frame.decl_base + local].ty
    }

    fn module_item_type(&self, frame: &Frame, local: usize) -> TypeValue {
        let item = self.instrs.items_of(frame.instance.id.module)[local];

        let leaf = match item.kind {
            InstrItemKind::Func(func) => LeafType::Func(FuncId {
                module: frame.instance.id.module,
                func,
            }),
            InstrItemKind::Import(m) => LeafType::Module(m),
        };

        TypeValue::Leaf(Type::Leaf(leaf))
    }

    fn push_value(&mut self, ty: TypeValue) {
        self.res.instr_types.push(ty);
        self.res.value_types.push(ty);
    }

    fn end_frame(&mut self, frame: Frame) {
        let Frame {
            instance,
            unknowns,
            instr_base,
            decl_base,
            ip,
            ..
        } = frame;

        assert_eq!(ip, self.res.instr_types.len() - instr_base);

        self.res.temp_types.clear();

        let ty_stack_start = 0;
        let ty_stack_end = self.res.instr_types.len() - instr_base;

        let stack_types_iter = self.res.instr_types[instr_base..]
            .iter()
            .copied()
            .map(|ty| ty.upgrade(self.types, &self.res.unknowns))
            .filter_map(|ty| match ty {
                Ok(ty) => Some(ty),
                Err(unknown) => {
                    self.res.temp_deiscovered_unknowns.push(unknown);
                    None
                }
            });

        self.res.temp_types.extend(stack_types_iter);

        let prepared_instance = instance.upgrade(self.types, &self.res.unknowns);

        if let Err(unknown) = prepared_instance {
            self.res.temp_deiscovered_unknowns.push(unknown);
        }

        if let Some(types) = self.res.temp_types.get(ty_stack_start..ty_stack_end)
        && let Ok(instance) = prepared_instance {
            self.generator.generate(
                GeneratorCtx {
                    types: self.types,
                    files: self.modules.files(),
                    interner: self.interner,
                    diags: self.diags,
                },
                GeneratorArgs {
                    instance,
                    func_view: self.instrs.view_of(instance.id),
                    types,
                    unknowns: &self.res.unknowns,
                },
            );
        }

        self.res.unknowns.end_frame(unknowns);
        self.res.decl_types.truncate(decl_base);
        self.res.instr_types.truncate(instr_base);
    }
}

enum Interrupt {
    Call(FuncInstId<TypeValue>),
}

struct TypeCheckerRes {
    unknowns: Unknowns,
    stack: Vec<Frame>,
    instr_types: Vec<TypeValue>,
    decl_types: Vec<DeclData>,
    value_types: Vec<TypeValue>,

    generated: FnvHashSet<FuncInstId<Type>>,
    to_type_check: Vec<FuncInstId<TypeValue>>,

    temp_types: Vec<Type>,
    temp_deiscovered_unknowns: Vec<UnknownType>,
}

struct Frame {
    instance: FuncInstId<TypeValue>,
    unknowns: UnknownFrame,
    instr_base: usize,
    decl_base: usize,
    value_base: usize,
    module_item_count: usize,
    ip: usize,
}

#[derive(Clone, Copy)]
struct DeclData {
    index: usize,
    ty: TypeValue,
}

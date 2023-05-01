use std::{
    borrow::Cow,
    collections::HashMap,
    ops::{Index, IndexMut},
};

use scoped_arena::Scope;

use crate::{
    parser::{Arg, Expr, WordKind},
    Errors, Parser, Source, SourceId,
};

pub struct InterpreterContext<'a> {
    modules: Modules<'a>,
}

struct Modules<'a> {
    inner: Vec<Module<'a>>,
    default: Module<'a>,
}

impl<'a> Modules<'a> {
    fn new() -> Self {
        Self {
            inner: Vec::new(),
            default: Module::default(),
        }
    }
}

impl<'a> Index<SourceId> for Modules<'a> {
    type Output = Module<'a>;

    fn index(&self, index: SourceId) -> &Self::Output {
        &self.inner.get(index.index()).unwrap_or(&self.default)
    }
}

impl<'a> IndexMut<SourceId> for Modules<'a> {
    fn index_mut(&mut self, index: SourceId) -> &mut Self::Output {
        if index.index() >= self.inner.len() {
            self.inner.resize_with(index.index() + 1, Default::default);
        }

        &mut self.inner[index.index()]
    }
}

#[derive(Default)]
struct Module<'a> {
    imports: HashMap<&'a str, SourceId>,
    items: HashMap<&'a str, InterpreterValue<'a>>,
}

pub struct Interpreter<'a> {
    ctx: &'a InterpreterContext<'a>,
    module: Module<'a>,
    source: &'a Source,
    stdout: &'a mut dyn FnMut(Cow<'a, str>),
}

impl<'a> Interpreter<'a> {
    pub fn new(ctx: &'a InterpreterContext<'a>, source: &'a Source, stdout: &'a mut dyn FnMut(Cow<'a, str>)) -> Self {
        Self {
            ctx,
            module: Module::default(),
            source,
            stdout,
        }
    }

    pub fn run(mut self, arena: &'a Scope, errors: &mut Errors) -> Module<'a> {
        self.module.imports = self.source.deps().collect();

        let (Some(program), ..) = Parser::new(
            arena,
            self.source.source(),
            Some(self.source.code_start()),
            errors,
        )
        .program() else {
            return self.module;
        };

        for &expr in program {
            self.interpret(expr, errors);
        }

        self.module
    }

    fn interpret(&mut self, expr: Expr<'a>, errors: &mut Errors) -> InterpreterValue<'a> {
        use InterpreterValue::*;
        match expr {
            Expr::Word(kind, name) => match kind {
                WordKind::Ident if let Some(value) = self.module.items.get(name.id) => value.clone(),
                WordKind::Ident => {
                    errors
                        .error(name.span.into())
                        .message("undefined variable");
                    Int(0)
                }
                WordKind::Str => InterpreterValue::Str(name.id.trim_matches('"').into()),
                WordKind::Int if let Ok(value) = name.id.parse() => InterpreterValue::Int(value),
                WordKind::Int => {
                    errors
                        .error(name.span.into())
                        .message("invalid integer literal");
                    Int(0)
                }
            },
            Expr::BinaryOp { lhs, op, rhs } => {
                let lhs = self.interpret(*lhs, errors);
                let rhs = self.interpret(*rhs, errors);
                match (op.id, lhs, rhs) {
                    ("+", Int(a), Int(b)) => Int(a + b),
                    ("-", Int(a), Int(b)) => Int(a - b),
                    ("/", Int(a), Int(b)) => Int(a / b),
                    ("*", Int(a), Int(b)) => Int(a * b),
                    
                    ("+", Str(a), Str(b)) => Str(a + b),
                    ("*", Str(a), Int(b)) => Str(a.repeat(b as usize).into()),

                    _ => {
                        errors
                            .error(op.span.into())
                            .message("not (yet) supported binary operation");

                        InterpreterValue::Int(0)
                    }
                }
            },
            Expr::UnaryOp { op, rhs } => {
                let rhs = self.interpret(*rhs, errors);
                match (op.id, rhs) {
                    ("-", Int(a)) => Int(-a),
                    _ => {
                        errors
                            .error(op.span.into())
                            .message("not (yet) supported unary operation");

                        Int(0)
                    }
                }
            },
            Expr::Call { callee: Expr::Word(WordKind::Ident, name), args } => {
                match name.id {
                    "print" => {
                        for arg in args {
                            let value = self.interpret(arg, errors);
                            match value {
                                Str(s) => print!("{}", s),
                                Int(i) => print!("{}", i),
                                Func(..) => print!("func"),
                            }
                        }
                        println!();
                        Int(0)
                    },
                    _ => {
                        errors
                            .error(name.span.into())
                            .message("not (yet) supported function call");

                        Int(0)
                    }
                }
            }
            Expr::Call { callee, args } => {
                errors
                    .error(callee.span().into())
                    .message("not (yet) supported function call");

                Int(0)
            },
            Expr::Binding { name, value } => {
                let value = self.interpret(*value, errors);
                self.module.items.insert(name.id, value.clone());
                value
            },
            Expr::Func { arguments, body } => Func(arguments, body),
        }
    }
}

#[derive(Clone)]
enum InterpreterValue<'a> {
    Str(Cow<'a, str>),
    Int(i64),
    Func(&'a [Arg<'a>], &'a [Expr<'a>]),
}

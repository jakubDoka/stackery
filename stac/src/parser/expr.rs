use std::fmt::Display;

use mini_alloc::{Diver, InternedStr};

use crate::Severty::*;
use crate::Token;
use crate::TokenKind::*;
use crate::TransposeOpt;
use crate::{lexer::OpCode, Parser, Span};

use super::StringParseError;

impl<'ctx, 'src, 'arena> Parser<'ctx, 'src, 'arena> {
    pub(super) fn expr(&mut self, mut diver: Diver) -> Option<ExprAst<'arena>> {
        let unit = ExprAst::Unit(self.unit(diver.untyped_dive())?);

        self.binary(diver, unit, u8::MAX)
    }

    fn binary(
        &mut self,
        mut diver: Diver,
        mut lhs: ExprAst<'arena>,
        prev_prec: u8,
    ) -> Option<ExprAst<'arena>> {
        while let Token {
            kind: Op(kind),
            span,
            ..
        } = self.peek() && kind.prec() <= prev_prec
        {
            self.next();

            let mut rhs = ExprAst::Unit(self.unit(diver.untyped_dive())?);
            rhs = self.binary(diver.untyped_dive(), rhs, kind.prec())?;
            lhs = ExprAst::Binary(self.arena.alloc(BinaryAst {
                lhs,
                rhs,
                op: OpAst { span, kind },
            }));
        }

        Some(lhs)
    }

    fn unit(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let token = self.next();

        fn unex_tok(parser: &mut Parser, span: Token, message: &str) -> Option<!> {
            parser
                .diags
                .builder(parser.files, parser.interner)
                .footer(Error, message)
                .annotation(Error, span.span, format_args!("unexpected {}", span.kind))
                .terminate()
        }

        let expr = match token.kind {
            If => self.if_(diver.untyped_dive()),
            Else => unex_tok(self, token, "else can only follow an if")?,
            Loop => self.loop_(diver.untyped_dive()),
            Ret => self.ret(diver.untyped_dive()),
            For => self.for_(diver.untyped_dive()),
            In => unex_tok(self, token, "in can only follow a for")?,
            Unknown => Some(UnitAst::Unknown(token.span)),
            Dot => unex_tok(
                self,
                token,
                "dot can only follow an expression of labeled keyword",
            )?,
            DoubleDot => unex_tok(
                self,
                token,
                "double dot can only follow an expression of labeled keyword",
            )?,
            Comma => unex_tok(self, token, "comma can only follow an expression")?,
            Semi => unex_tok(
                self,
                token,
                "semicolon can only follow an expression inside a block",
            )?,
            Colon => unex_tok(self, token, "colon can only follow pattern")?,
            Enum => self.enum_(diver.untyped_dive()),
            Struct => self.struct_(diver.untyped_dive()),
            LBrace => self.block(diver.untyped_dive()),
            RBrace => unex_tok(self, token, "unmatched right brace")?,
            LBracket => self.array(diver.untyped_dive()),
            RBracket => unex_tok(self, token, "unmatched right bracket")?,
            Tuple => self.tuple(diver.untyped_dive()),
            LParen => self.paren(diver.untyped_dive()),
            RParen => unex_tok(self, token, "unmatched right parenthesis")?,
            Ident => Some(UnitAst::Ident(IdentAst {
                span: token.span,
                name: self.interner.intern(token.source),
            })),
            Import => Some(UnitAst::Import({
                let source =
                    &token.source[Import.name().len()..token.source.len() - RBracket.name().len()];

                let import = IdentAst {
                    span: token.span,
                    name: self.interner.intern(source),
                };

                self.imports.push(import);

                import
            })),
            Str => self.str(token),
            Int => self.int(token),
            True => self.bool(true, token.span),
            False => self.bool(false, token.span),
            Op(op) => self.unary(
                diver.untyped_dive(),
                OpAst {
                    span: token.span,
                    kind: op,
                },
            ),
            Eof => unex_tok(self, token, "got eof when expression is expected")?,
            Err => unex_tok(self, token, "got error token when expression is expected")?,
        }?;

        self.handle_postfix(diver, expr)
    }

    fn bool(&mut self, value: bool, span: Span) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Literal(LiteralAst {
            kind: LiteralKindAst::Bool(value),
            span,
        }))
    }

    fn tuple(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        let (exprs, ..) = self.sequence(diver, Self::expr, Comma, RParen, "tuple element")?;
        Some(UnitAst::Tuple(exprs))
    }

    fn handle_postfix(
        &mut self,
        mut diver: Diver,
        mut expr: UnitAst<'arena>,
    ) -> Option<UnitAst<'arena>> {
        loop {
            let token = self.peek();
            expr = match token.kind {
                Dot => {
                    self.next();
                    let field = self.ident("field")?;
                    UnitAst::FieldAccess(self.arena.alloc(FieldAccessAst { expr, field }))
                }
                LParen => {
                    self.next();
                    let args = &*self
                        .sequence(
                            diver.untyped_dive(),
                            Self::expr,
                            Comma,
                            RParen,
                            "function parameter",
                        )?
                        .0;

                    UnitAst::Call(self.arena.alloc(CallAst { callee: expr, args }))
                }
                LBracket => {
                    self.next();
                    let index = self.expr(diver.untyped_dive())?;
                    self.expect_advance(RBracket, "closing index operator");

                    UnitAst::Index(self.arena.alloc(IndexAst { expr, index }))
                }
                Colon if let UnitAst::Ident(name) = expr => {
                    self.next();

                    let expr = self.expr(diver.untyped_dive())?;
                    break Some(UnitAst::Decl(self.arena.alloc(NamedExpr { name , expr  })))
                }
                _ => break Some(expr),
            }
        }
    }

    fn unary(&mut self, diver: Diver, op: OpAst) -> Option<UnitAst<'arena>> {
        match op.kind {
            OpCode::Or => self.func(diver, false),
            OpCode::BitOr => self.func(diver, true),
            _ => {
                let expr = ExprAst::Unit(self.unit(diver)?);
                Some(UnitAst::Unary(self.arena.alloc(UnaryAst { op, expr })))
            }
        }
    }

    fn func(&mut self, mut diver: Diver, has_args: bool) -> Option<UnitAst<'arena>> {
        let args = match has_args {
            true => {
                &*self
                    .sequence(
                        diver.untyped_dive(),
                        Self::func_arg,
                        Comma,
                        Op(OpCode::BitOr),
                        "function parameter",
                    )?
                    .0
            }
            false => &[],
        };

        let body = self.expr(diver)?;

        Some(UnitAst::Func(self.arena.alloc(FuncAst { args, body })))
    }

    fn func_arg(&mut self, diver: Diver) -> Option<FuncArgAst<'arena>> {
        let name = self.ident("parameter name")?;

        let default = self
            .try_advance(Op(OpCode::Assign))
            .map(|_| self.unit(diver))
            .transpose()?;

        Some(FuncArgAst { name, default })
    }

    fn int(&mut self, token: Token<'src>) -> Option<UnitAst<'arena>> {
        let value = token
            .source
            .parse::<u64>()
            .expect("lexer should have validated int");
        Some(UnitAst::Literal(LiteralAst {
            span: token.span,
            kind: LiteralKindAst::Int(value.to_ne_bytes()),
        }))
    }

    fn str(&mut self, token: Token<'src>) -> Option<UnitAst<'arena>> {
        let delim_len = '"'.len_utf8();
        let source = &token.source[delim_len..token.source.len() - delim_len];

        Some(UnitAst::Literal(LiteralAst {
            span: token.span,
            kind: match self.string_parser.parse(&source, self.interner) {
                Ok(name) => LiteralKindAst::Str(name),
                Result::Err(StringParseError::IncompleteEscape) => {
                    self.diags
                        .builder(self.files, self.interner)
                        .footer(Error, "incomplete escape sequence")
                        .annotation(Error, token.span, "in string literal")
                        .terminate()?;
                }
                Result::Err(StringParseError::InvalidEscape(index)) => {
                    self.diags
                        .builder(self.files, self.interner)
                        .footer(Error, "invalid escape sequence")
                        .annotation(Error, token.span.shift(index), "in string literal")
                        .terminate()?;
                }
            },
        }))
    }

    fn paren(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let expr = self.expr(diver.untyped_dive())?;
        self.expect_advance(RParen, "right parenthesis")?;
        Some(UnitAst::Paren(self.arena.alloc(expr)))
    }

    fn array(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        if self.try_advance(RBracket).is_some() {
            return Some(UnitAst::Array(&[]));
        }

        let first = self.expr(diver.untyped_dive())?;

        if self.try_advance(Semi).is_some() {
            let len = self.expr(diver.untyped_dive())?;
            self.expect_advance(RBracket, "end of filled array")?;
            return Some(UnitAst::FilledArray(
                self.arena.alloc(FilledArrayAst { expr: first, len }),
            ));
        }

        let mut elems = diver.dive::<ExprAst<'arena>>();
        elems.push(first);

        while self.try_advance(Comma).is_some() && self.peek().kind != RBracket {
            let expr = self.expr(elems.untyped_dive())?;
            elems.push(expr);
        }

        self.expect_advance(RBracket, "end of array")?;

        Some(UnitAst::Array(self.arena.alloc_rev_iter(elems)))
    }

    fn block(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        let (exprs, trailing_semi) = self.sequence(diver, Self::expr, Semi, RBrace, "block")?;
        Some(UnitAst::Block(self.arena.alloc(BlockAst {
            exprs,
            trailing_semi,
        })))
    }

    fn struct_(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        let (fields, ..) =
            self.sequence(diver, Self::struct_field, Comma, RBrace, "struct decl")?;
        Some(UnitAst::Struct(fields))
    }

    fn struct_field(&mut self, diver: Diver) -> Option<StructField<'arena>> {
        let peek = self.peek();

        match peek.kind {
            Ident => {
                let name = self.ident("struct field name")?;
                let value = self
                    .try_advance(Colon)
                    .map(|_| self.expr(diver))
                    .transpose()?;

                Some(match value {
                    Some(value) => StructField::Decl(NamedExpr { name, expr: value }),
                    None => StructField::Inline(name),
                })
            }
            DoubleDot => {
                self.next();
                let expr = self.expr(diver)?;
                Some(StructField::Embed(expr))
            }
            _ => self
                .diags
                .builder(self.files, self.interner)
                .footer(
                    Error,
                    "struct field can either start with '..' for embeding or identifier",
                )
                .annotation(Error, peek.span, format_args!("unexpected {}", peek.kind))
                .terminate()?,
        }
    }

    fn enum_(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let name = self.ident("enum name")?;
        let value = self
            .try_advance(Colon)
            .map(|_| self.expr(diver.untyped_dive()))
            .transpose()?;
        self.expect_advance(RBrace, "closing the enum")?;
        Some(UnitAst::Enum(self.arena.alloc(EnumAst { name, value })))
    }

    fn for_(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let label = self.label("for")?;
        let var = self.ident("for loop variable")?;
        self.expect_advance(In, "expected in after for")?;
        let iter = self.expr(diver.untyped_dive())?;
        let body = self.expr(diver)?;

        Some(UnitAst::ForLoop(self.arena.alloc(ForLoopAst {
            label,
            var,
            iter,
            body,
        })))
    }

    fn ret(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Ret(
            (self.peek().kind != Semi)
                .then(|| self.expr(diver))
                .transpose()?
                .map(|expr| &*self.arena.alloc(expr)),
        ))
    }

    fn if_(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let cond = self.expr(diver.untyped_dive())?;
        let then = self.expr(diver.untyped_dive())?;
        let else_ = self
            .try_advance(Else)
            .map(|_| self.expr(diver))
            .transpose()?;

        Some(UnitAst::If(self.arena.alloc(IfAst { cond, then, else_ })))
    }

    fn loop_(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Loop(self.arena.alloc(LoopAst {
            label: self.label("loop")?,
            body: self.expr(diver)?,
        })))
    }

    fn label(&mut self, objective: impl Display) -> Option<Option<IdentAst>> {
        self.try_advance(Dot)
            .map(|_| self.ident(format_args!("{objective} label")))
            .transpose()
    }

    fn ident(&mut self, objective: impl Display) -> Option<IdentAst> {
        let token =
            self.expect_advance(Ident, format_args!("expected identifier for {}", objective))?;

        Some(IdentAst {
            span: token.span,
            name: self.interner.intern(token.source),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'arena> {
    Unit(UnitAst<'arena>),
    Binary(&'arena BinaryAst<'arena>),
}

#[derive(Debug, Clone, Copy)]
pub enum UnitAst<'arena> {
    Literal(LiteralAst),
    Ident(IdentAst),
    Import(IdentAst),
    Str(IdentAst),
    Block(&'arena BlockAst<'arena>),
    Unary(&'arena UnaryAst<'arena>),
    Array(&'arena [ExprAst<'arena>]),
    FilledArray(&'arena FilledArrayAst<'arena>),
    Tuple(&'arena [ExprAst<'arena>]),
    Struct(&'arena [StructField<'arena>]),
    Enum(&'arena EnumAst<'arena>),
    Call(&'arena CallAst<'arena>),
    Func(&'arena FuncAst<'arena>),
    Decl(&'arena NamedExpr<'arena>),
    Loop(&'arena LoopAst<'arena>),
    Index(&'arena IndexAst<'arena>),
    ForLoop(&'arena ForLoopAst<'arena>),
    Break(&'arena BreakAst<'arena>),
    Continue(ContinueAst),
    FieldAccess(&'arena FieldAccessAst<'arena>),
    If(&'arena IfAst<'arena>),
    Ret(Option<&'arena ExprAst<'arena>>),
    Paren(&'arena ExprAst<'arena>),
    Unknown(Span),
}

#[derive(Debug, Clone, Copy)]
pub struct IndexAst<'arena> {
    pub expr: UnitAst<'arena>,
    pub index: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockAst<'arena> {
    pub exprs: &'arena [ExprAst<'arena>],
    pub trailing_semi: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumAst<'arena> {
    pub name: IdentAst,
    pub value: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct IfAst<'arena> {
    pub cond: ExprAst<'arena>,
    pub then: ExprAst<'arena>,
    pub else_: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FieldAccessAst<'arena> {
    pub expr: UnitAst<'arena>,
    pub field: IdentAst,
}

#[derive(Debug, Clone, Copy)]
pub struct ForLoopAst<'arena> {
    pub label: Option<IdentAst>,
    pub var: IdentAst,
    pub iter: ExprAst<'arena>,
    pub body: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAst<'arena> {
    pub body: ExprAst<'arena>,
    pub label: Option<IdentAst>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakAst<'arena> {
    pub label: Option<IdentAst>,
    pub expr: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ContinueAst {
    pub label: Option<IdentAst>,
}

#[derive(Debug, Clone, Copy)]
pub enum StructField<'arena> {
    Decl(NamedExpr<'arena>),
    Inline(IdentAst),
    Embed(ExprAst<'arena>),
}

#[derive(Debug, Clone, Copy)]
pub struct UnaryAst<'arena> {
    pub op: OpAst,
    pub expr: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncAst<'arena> {
    pub args: &'arena [FuncArgAst<'arena>],
    pub body: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncArgAst<'arena> {
    pub name: IdentAst,
    pub default: Option<UnitAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallAst<'arena> {
    pub callee: UnitAst<'arena>,
    pub args: &'arena [ExprAst<'arena>],
}

#[derive(Debug, Clone, Copy)]
pub struct NamedExpr<'arena> {
    pub name: IdentAst,
    pub expr: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct FilledArrayAst<'arena> {
    pub expr: ExprAst<'arena>,
    pub len: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryAst<'arena> {
    pub lhs: ExprAst<'arena>,
    pub rhs: ExprAst<'arena>,
    pub op: OpAst,
}

#[derive(Debug, Clone, Copy)]
pub struct OpAst {
    pub kind: OpCode,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct LiteralAst {
    pub kind: LiteralKindAst,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum LiteralKindAst {
    Int([u8; 8]),
    Str(InternedStr),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub struct IdentAst {
    pub name: InternedStr,
    pub span: Span,
}

#[cfg(test)]
mod test {
    use crate::*;
    use mini_alloc::*;

    fn perform_test(source_code: &str, ctx: &mut String) {
        let mut files = Files::new();
        let interner = StrInterner::default();
        let file = File::new(interner.intern("test"), source_code.into());
        let file_id = files.add(file);
        let mut diags = Diagnostics::default();
        let mut arena = ArenaBase::new(1000);
        let scope = arena.scope();
        let mut string_parser = StringParser::default();
        let mut diver = DiverBase::new(1000);
        let mut imports = vec![];

        let mut parser = Parser::new(
            &files,
            file_id,
            &interner,
            &mut diags,
            &scope,
            &mut string_parser,
            &mut imports,
        );

        let res = parser.parse(diver.dive());

        match res {
            Some(ast) => format_ast(ast, ctx, 0, &interner),
            None => ctx.push_str(diags.diagnostic_view()),
        }
    }

    macro_rules! cases {
        ($($name:ident $code:literal)*) => {
            print_test::cases!($(
                fn $name(ctx) {
                    perform_test(
                        $code,
                        ctx
                    );
                }
            )*);
        }
    }

    cases! {
        precedence "1 + 2 * 3 - 4 / 5"
        function "|a, b = 10| a + b"
        function_call "foo(1, 2, 3)"
        method_call "foo.bar(1, 2, 3)"
        struct_ctor "*{ a: 1, b, ..c }"
        multy_statement "a: 1; b: 2; c: 3;"
        block "{ a: 1; b: 2; a + b }"
        field_access "foo.bar.baz.rar"
        enum_ctor "|{none}; |{ some: 10 }"
        if_expr "if cond then else else_"
        loop_expr "loop.label body"
        for_each "for.label var in iter body"
        some_code "
            enumerate: |iter| *{
                counter: 0,
                inner: iter,
                next: |self| self.inner.next().map(|x| {
                    self.counter += 1;
                    *(self.counter - 1, x)
                }),
            }
        "
    }
}

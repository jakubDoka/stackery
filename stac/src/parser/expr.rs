use std::fmt::Display;

use mini_alloc::{Diver, InternedStr};

use crate::TokenKind::*;
use crate::*;

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
        } = self.peek() && kind.prec() < prev_prec
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
                .footer(Severty::Error, message)
                .annotation(
                    Severty::Error,
                    span.span,
                    format_args!("unexpected {}", span.kind),
                )
                .terminate()
        }

        let expr = match token.kind {
            If => self.if_(diver.untyped_dive(), token.span),
            Else => unex_tok(self, token, "else can only follow an if")?,
            Loop => self.loop_(diver.untyped_dive(), token.span),
            Break => self.break_(diver.untyped_dive(), token.span),
            Continue => self.continue_(token.span),
            Ret => self.ret(diver.untyped_dive(), token.span),
            For => self.for_(diver.untyped_dive(), token.span),
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
            Struct => self.struct_(diver.untyped_dive(), token.span),
            LBrace => self.block(diver.untyped_dive(), token.span),
            RBrace => unex_tok(self, token, "unmatched right brace")?,
            LBracket => self.array(diver.untyped_dive(), token.span),
            RBracket => unex_tok(self, token, "unmatched right bracket")?,
            Tuple => self.tuple(diver.untyped_dive(), token.span),
            LParen => self.paren(diver.untyped_dive()),
            RParen => unex_tok(self, token, "unmatched right parenthesis")?,
            Ident => Some(UnitAst::Ident(IdentAst {
                span: token.span,
                ident: self.interner.intern(token.source),
            })),
            Import => Some(UnitAst::Import({
                let source = &token.source[":{".len()..token.source.len() - "}".len()];

                IdentAst {
                    span: token.span,
                    ident: self.interner.intern(source),
                }
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

    fn break_(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        let label = self.label("break")?;
        let expr = (![Semi, RBrace, Else, Eof, Comma].contains(&self.peek().kind))
            .then(|| self.expr(diver))
            .transpose()?;
        Some(UnitAst::Break(self.arena.alloc(BreakAst {
            keyword,
            label,
            expr,
        })))
    }

    fn continue_(&mut self, keyword: Span) -> Option<UnitAst<'arena>> {
        let label = self.label("continue")?;
        Some(UnitAst::Continue(ContinueAst { keyword, label }))
    }

    fn bool(&mut self, value: bool, span: Span) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Literal(LiteralAst {
            kind: LiteralKindAst::Bool(value),
            span,
        }))
    }

    fn tuple(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        let (values, ..) = self.sequence(diver, Self::expr, Comma, RParen, "tuple element")?;
        Some(UnitAst::Tuple { keyword, values })
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
                    UnitAst::Field(self.arena.alloc(FieldAst { expr, name: field }))
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

                    UnitAst::Call(self.arena.alloc(CallAst { caller: expr, args }))
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
                    break Some(UnitAst::Decl(self.arena.alloc(NamedExprAst { name , expr  })))
                }
                _ => break Some(expr),
            }
        }
    }

    fn unary(&mut self, diver: Diver, op: OpAst) -> Option<UnitAst<'arena>> {
        match op.kind {
            OpCode::Or => self.func(diver, false, op.span),
            OpCode::BitOr => self.func(diver, true, op.span),
            _ => {
                let expr = self.unit(diver)?;
                Some(UnitAst::Unary(self.arena.alloc(UnaryAst { op, expr })))
            }
        }
    }

    fn func(&mut self, mut diver: Diver, has_args: bool, pipe: Span) -> Option<UnitAst<'arena>> {
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

        Some(UnitAst::Func(self.arena.alloc(FuncAst {
            args,
            body,
            pipe,
        })))
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
                        .footer(Severty::Error, "incomplete escape sequence")
                        .annotation(Severty::Error, token.span, "in string literal")
                        .terminate()?;
                }
                Result::Err(StringParseError::InvalidEscape(index)) => {
                    self.diags
                        .builder(self.files, self.interner)
                        .footer(Severty::Error, "invalid escape sequence")
                        .annotation(
                            Severty::Error,
                            token.span.shift('"'.len_utf8() + index),
                            "in string literal",
                        )
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

    fn array(&mut self, mut diver: Diver, bracket: Span) -> Option<UnitAst<'arena>> {
        if self.try_advance(RBracket).is_some() {
            return Some(UnitAst::Array {
                bracket,
                elems: &[],
            });
        }

        let first = self.expr(diver.untyped_dive())?;

        if self.try_advance(Semi).is_some() {
            let len = self.expr(diver.untyped_dive())?;
            self.expect_advance(RBracket, "end of filled array")?;
            return Some(UnitAst::FilledArray(self.arena.alloc(FilledArrayAst {
                expr: first,
                len,
                bracket,
            })));
        }

        let mut elems = diver.dive::<ExprAst<'arena>>();
        elems.push(first);

        while self.try_advance(Comma).is_some() && self.peek().kind != RBracket {
            let expr = self.expr(elems.untyped_dive())?;
            elems.push(expr);
        }

        self.expect_advance(RBracket, "end of array")?;

        Some(UnitAst::Array {
            bracket,
            elems: self.arena.alloc_rev_iter(elems),
        })
    }

    fn block(&mut self, diver: Diver, brace: Span) -> Option<UnitAst<'arena>> {
        let (exprs, trailing_semi) = self.sequence(diver, Self::expr, Semi, RBrace, "block")?;
        Some(UnitAst::Block(self.arena.alloc(BlockAst {
            exprs,
            trailing_semi,
            brace,
        })))
    }

    fn struct_(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        let (fields, ..) =
            self.sequence(diver, Self::struct_field, Comma, RBrace, "struct decl")?;
        Some(UnitAst::Struct { fields, keyword })
    }

    fn struct_field(&mut self, diver: Diver) -> Option<StructFieldAst<'arena>> {
        let peek = self.peek();

        match peek.kind {
            Ident => {
                let name = self.ident("struct field name")?;
                let value = self
                    .try_advance(Colon)
                    .map(|_| self.expr(diver))
                    .transpose()?;

                Some(match value {
                    Some(value) => StructFieldAst::Decl(NamedExprAst { name, expr: value }),
                    None => StructFieldAst::Inline(name),
                })
            }
            DoubleDot => {
                self.next();
                let expr = self.expr(diver)?;
                Some(StructFieldAst::Embed(expr))
            }
            _ => self
                .diags
                .builder(self.files, self.interner)
                .footer(
                    Severty::Error,
                    "struct field can either start with '..' for embeding or identifier",
                )
                .annotation(
                    Severty::Error,
                    peek.span,
                    format_args!("unexpected {}", peek.kind),
                )
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

    fn for_(&mut self, mut diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
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
            keyword,
        })))
    }

    fn ret(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Ret {
            keyword,

            value: (self.peek().kind != Semi)
                .then(|| self.expr(diver))
                .transpose()?
                .map(|expr| &*self.arena.alloc(expr)),
        })
    }

    fn if_(&mut self, mut diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        let cond = self.expr(diver.untyped_dive())?;
        dbg!(self.peek());
        let then = self.expr(diver.untyped_dive())?;
        dbg!(self.peek());
        let else_ = self
            .try_advance(Else)
            .map(|_| self.expr(diver))
            .transpose()?;

        Some(UnitAst::If(self.arena.alloc(IfAst {
            cond,
            then,
            else_,
            keyword,
        })))
    }

    fn loop_(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        Some(UnitAst::Loop(self.arena.alloc(LoopAst {
            label: self.label("loop")?,
            body: self.expr(diver)?,
            keyword,
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
            ident: self.interner.intern(token.source),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'arena> {
    Unit(UnitAst<'arena>),
    Binary(&'arena BinaryAst<'arena>),
}

impl ExprAst<'_> {
    pub fn span(&self) -> Span {
        match self {
            ExprAst::Unit(unit) => unit.span(),
            ExprAst::Binary(binary) => binary.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnitAst<'arena> {
    Literal(LiteralAst),
    Ident(IdentAst),
    Import(IdentAst),
    Block(&'arena BlockAst<'arena>),
    Unary(&'arena UnaryAst<'arena>),
    Array {
        bracket: Span,
        elems: &'arena [ExprAst<'arena>],
    },
    FilledArray(&'arena FilledArrayAst<'arena>),
    Tuple {
        keyword: Span,
        values: &'arena [ExprAst<'arena>],
    },
    Struct {
        keyword: Span,
        fields: &'arena [StructFieldAst<'arena>],
    },
    Enum(&'arena EnumAst<'arena>),
    Call(&'arena CallAst<'arena>),
    Func(&'arena FuncAst<'arena>),
    Decl(&'arena NamedExprAst<'arena>),
    Loop(&'arena LoopAst<'arena>),
    Index(&'arena IndexAst<'arena>),
    ForLoop(&'arena ForLoopAst<'arena>),
    Break(&'arena BreakAst<'arena>),
    Continue(ContinueAst),
    Field(&'arena FieldAst<'arena>),
    If(&'arena IfAst<'arena>),
    Ret {
        keyword: Span,
        value: Option<&'arena ExprAst<'arena>>,
    },
    Paren(&'arena ExprAst<'arena>),
    Unknown(Span),
}

impl UnitAst<'_> {
    fn span(&self) -> Span {
        match self {
            UnitAst::Literal(literal) => literal.span,
            UnitAst::Ident(ident) => ident.span,
            UnitAst::Import(ident) => ident.span,
            UnitAst::Block(block) => block.brace,
            UnitAst::Unary(unary) => unary.op.span,
            UnitAst::Array { bracket, .. } => *bracket,
            UnitAst::FilledArray(array) => array.bracket,
            UnitAst::Tuple { keyword, .. } => *keyword,
            UnitAst::Struct { keyword, .. } => *keyword,
            UnitAst::Enum(enum_) => enum_.name.span,
            UnitAst::Call(call) => call.caller.span(),
            UnitAst::Func(func) => func.pipe,
            UnitAst::Decl(decl) => decl.name.span,
            UnitAst::Loop(loop_) => loop_.keyword,
            UnitAst::Index(index) => index.expr.span(),
            UnitAst::ForLoop(for_) => for_.keyword,
            UnitAst::Break(break_) => break_.keyword,
            UnitAst::Continue(continue_) => continue_.keyword,
            UnitAst::Field(field) => field.expr.span(),
            UnitAst::If(if_) => if_.keyword,
            UnitAst::Ret { keyword, .. } => *keyword,
            UnitAst::Paren(expr) => expr.span(),
            UnitAst::Unknown(span) => *span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexAst<'arena> {
    pub expr: UnitAst<'arena>,
    pub index: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockAst<'arena> {
    pub brace: Span,
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
    pub keyword: Span,
    pub cond: ExprAst<'arena>,
    pub then: ExprAst<'arena>,
    pub else_: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct FieldAst<'arena> {
    pub expr: UnitAst<'arena>,
    pub name: IdentAst,
}

#[derive(Debug, Clone, Copy)]
pub struct ForLoopAst<'arena> {
    pub keyword: Span,
    pub label: Option<IdentAst>,
    pub var: IdentAst,
    pub iter: ExprAst<'arena>,
    pub body: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAst<'arena> {
    pub keyword: Span,
    pub label: Option<IdentAst>,
    pub body: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakAst<'arena> {
    pub keyword: Span,
    pub label: Option<IdentAst>,
    pub expr: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ContinueAst {
    pub keyword: Span,
    pub label: Option<IdentAst>,
}

#[derive(Debug, Clone, Copy)]
pub enum StructFieldAst<'arena> {
    Decl(NamedExprAst<'arena>),
    Inline(IdentAst),
    Embed(ExprAst<'arena>),
}

#[derive(Debug, Clone, Copy)]
pub struct UnaryAst<'arena> {
    pub op: OpAst,
    pub expr: UnitAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncAst<'arena> {
    pub pipe: Span,
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
    pub caller: UnitAst<'arena>,
    pub args: &'arena [ExprAst<'arena>],
}

#[derive(Debug, Clone, Copy)]
pub struct NamedExprAst<'arena> {
    pub name: IdentAst,
    pub expr: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct FilledArrayAst<'arena> {
    pub bracket: Span,
    pub expr: ExprAst<'arena>,
    pub len: ExprAst<'arena>,
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryAst<'arena> {
    pub lhs: ExprAst<'arena>,
    pub rhs: ExprAst<'arena>,
    pub op: OpAst,
}

impl BinaryAst<'_> {
    fn span(&self) -> Span {
        self.lhs.span()
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralKindAst {
    Int([u8; 8]),
    Str(InternedStr),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub struct IdentAst {
    pub ident: InternedStr,
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

        let res = Parser::new(
            &files,
            file_id,
            &interner,
            &mut diags,
            &scope,
            &mut string_parser,
        )
        .parse(diver.dive());

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

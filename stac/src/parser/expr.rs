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

    fn unit(&mut self, diver: Diver) -> Option<UnitAst<'arena>> {
        let token = self.next();

        fn unex_tok(parser: &mut Parser, span: Token, message: &str) -> Option<!> {
            parser
                .diags
                .builder(parser.files)
                .footer(Error, message)
                .annotation(Error, span.span, format_args!("unexpected {}", span.kind))
                .terminate()
        }

        match token.kind {
            If => self.if_(diver),
            Else => unex_tok(self, token, "else can only follow an if")?,
            Loop => self.loop_(diver),
            Ret => self.ret(diver),
            For => self.for_(diver),
            In => unex_tok(self, token, "in can only follow a for")?,
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
            Enum => self.enum_(diver),
            Struct => self.struct_(diver),
            LBrace => self.block(diver),
            RBrace => unex_tok(self, token, "unmatched right brace")?,
            LBracket => self.array(diver),
            RBracket => unex_tok(self, token, "unmatched right bracket")?,
            Tuple => todo!(),
            LParen => self.paren(diver),
            RParen => unex_tok(self, token, "unmatched right parenthesis")?,
            Ident => Some(UnitAst::Ident(IdentAst {
                span: token.span,
                name: self.interner.intern(token.source),
            })),
            Import => Some(UnitAst::Import({
                let source =
                    &token.source[Import.name().len()..token.source.len() - RBracket.name().len()];

                IdentAst {
                    span: token.span,
                    name: self.interner.intern(source),
                }
            })),
            Str => self.str(token),
            Int => self.int(token),
            Op(op) => self.unary(
                diver,
                OpAst {
                    span: token.span,
                    kind: op,
                },
            ),
            Eof => unex_tok(self, token, "got eof when expression is expected")?,
            Err => unex_tok(self, token, "got error token when expression is expected")?,
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
        let value = match token.source.parse::<u64>() {
            Ok(value) => value,
            Result::Err(_) => unreachable!("int token should be a valid integer"),
        };

        Some(UnitAst::Literal(LiteralAst {
            span: token.span,
            kind: LiteralKindAst::Int(value),
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
                        .builder(self.files)
                        .footer(Error, "incomplete escape sequence")
                        .annotation(Error, token.span, "in string literal")
                        .terminate()?;
                }
                Result::Err(StringParseError::InvalidEscape(index)) => {
                    self.diags
                        .builder(self.files)
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
                let expr = self.expr(diver)?;
                Some(StructField::Embed(expr))
            }
            _ => self
                .diags
                .builder(self.files)
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
    MethodCall(&'arena MethodCallAst<'arena>),
    Call(&'arena CallAst<'arena>),
    Func(&'arena FuncAst<'arena>),
    Decl(&'arena NamedExpr<'arena>),
    Loop(&'arena LoopAst<'arena>),
    ForLoop(&'arena ForLoopAst<'arena>),
    Break(&'arena BreakAst<'arena>),
    Continue(ContinueAst),
    FieldAccess(&'arena FieldAccessAst<'arena>),
    If(&'arena IfAst<'arena>),
    Ret(Option<&'arena ExprAst<'arena>>),
    Paren(&'arena ExprAst<'arena>),
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
    pub expr: ExprAst<'arena>,
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
pub struct MethodCallAst<'arena> {
    pub caller: ExprAst<'arena>,
    pub name: IdentAst,
    pub args: &'arena [ExprAst<'arena>],
}

#[derive(Debug, Clone, Copy)]
pub struct CallAst<'arena> {
    pub callee: ExprAst<'arena>,
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
    Int(u64),
    Float(f64),
    Str(InternedStr),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub struct IdentAst {
    pub name: InternedStr,
    pub span: Span,
}

#[cfg(test)]
mod test {
    use mini_alloc::{ArenaBase, DiverBase, StrInterner};

    use crate::{parser::StringParser, Diagnostics, File, Files, TokenKind};

    use super::*;

    fn format_ast(ast: &[ExprAst], ctx: &mut String, indent: usize, interner: &StrInterner) {
        for &ast in ast {
            format_expr(ast, ctx, indent, interner);
        }
    }

    fn format_expr(ast: ExprAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
        match ast {
            ExprAst::Unit(u) => format_unit(u, ctx, indent, interner),
            ExprAst::Binary(&b) => format_binary(b, ctx, indent, interner),
        }
    }

    fn format_binary(binary: BinaryAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
        ctx.push('(');
        format_expr(binary.lhs, ctx, indent, interner);
        ctx.push(' ');
        ctx.push_str(binary.op.kind.name());
        ctx.push(' ');
        format_expr(binary.rhs, ctx, indent, interner);
        ctx.push(')');
    }

    fn indent_f(ctx: &mut String, indent: usize) {
        for _ in 0..indent {
            ctx.push_str("    ");
        }
    }

    fn format_list<T: Copy>(
        list: &[T],
        ctx: &mut String,
        indent: usize,
        interner: &StrInterner,
        [start, sep, end]: [TokenKind; 3],
        indented: bool,
        trailing: bool,
        mut fmt: impl FnMut(T, &mut String, usize, &StrInterner),
    ) {
        ctx.push_str(start.name());
        let Some((last, list)) = list.split_last() else {
            ctx.push_str(end.name());
            return;
        };

        if indented {
            ctx.push('\n');
        }

        for &item in list {
            if indented {
                indent_f(ctx, indent + 1);
            }
            fmt(item, ctx, indent + indented as usize, interner);
            ctx.push_str(sep.name());
            if indented {
                ctx.push('\n');
            } else {
                ctx.push(' ');
            }
        }

        if indented {
            indent_f(ctx, indent + 1);
        }
        fmt(*last, ctx, indent + indented as usize, interner);
        if trailing {
            ctx.push_str(sep.name());
        }
        if indented {
            ctx.push('\n');
        }

        if indented {
            indent_f(ctx, indent);
        }
        ctx.push_str(end.name());
    }

    fn format_unit(unit: UnitAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
        use std::fmt::Write;
        match unit {
            UnitAst::Literal(l) => match l.kind {
                LiteralKindAst::Int(i) => write!(ctx, "{}", i).unwrap(),
                LiteralKindAst::Float(f) => write!(ctx, "{}", f).unwrap(),
                LiteralKindAst::Str(s) => write!(ctx, "{:?}", s).unwrap(),
                LiteralKindAst::Char(c) => write!(ctx, "{:?}", c).unwrap(),
                LiteralKindAst::Bool(b) => write!(ctx, "{}", b).unwrap(),
            },
            UnitAst::Import(ident) | UnitAst::Str(ident) | UnitAst::Ident(ident) => {
                ctx.push_str(&interner[ident.name])
            }
            UnitAst::Block(b) => {
                format_list(
                    b.exprs,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::LBrace, TokenKind::Semi, TokenKind::RBrace],
                    true,
                    b.trailing_semi,
                    |expr, ctx, indent, interner| {
                        format_expr(expr, ctx, indent, interner);
                        ctx.push(';');
                    },
                );
            }
            UnitAst::Unary(u) => {
                ctx.push_str(u.op.kind.name());
                format_expr(u.expr, ctx, indent, interner);
            }
            UnitAst::Array(a) => {
                format_list(
                    a,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::LBracket, TokenKind::Comma, TokenKind::RBracket],
                    false,
                    false,
                    |expr, ctx, indent, interner| {
                        format_expr(expr, ctx, indent, interner);
                    },
                );
            }
            UnitAst::FilledArray(a) => {
                ctx.push('[');
                format_expr(a.expr, ctx, indent, interner);
                ctx.push(';');
                format_expr(a.len, ctx, indent, interner);
                ctx.push(']');
            }
            UnitAst::Tuple(t) => {
                format_list(
                    t,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::Tuple, TokenKind::Comma, TokenKind::RParen],
                    false,
                    false,
                    |expr, ctx, indent, interner| {
                        format_expr(expr, ctx, indent, interner);
                    },
                );
            }
            UnitAst::Struct(s) => {
                format_list(
                    s,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::Struct, TokenKind::Comma, TokenKind::RBrace],
                    true,
                    false,
                    |field, ctx, indent, interner| match field {
                        StructField::Decl(named) => {
                            ctx.push_str(&interner[named.name.name]);
                            ctx.push_str(": ");
                            format_expr(named.expr, ctx, indent, interner);
                        }
                        StructField::Inline(name) => {
                            ctx.push_str(&interner[name.name]);
                        }
                        StructField::Embed(expr) => {
                            ctx.push_str(TokenKind::DoubleDot.name());
                            format_expr(expr, ctx, indent, interner);
                        }
                    },
                );
            }
            UnitAst::Enum(e) => {
                ctx.push_str(TokenKind::Enum.name());
                match e.value {
                    Some(val) => {
                        ctx.push(' ');
                        ctx.push_str(&interner[e.name.name]);
                        ctx.push_str(": ");
                        format_expr(val, ctx, indent, interner);
                        ctx.push_str(" }");
                    }
                    None => {
                        ctx.push_str(&interner[e.name.name]);
                        ctx.push('}');
                    }
                }
            }
            UnitAst::MethodCall(mc) => {
                format_expr(mc.caller, ctx, indent, interner);
                ctx.push('.');
                ctx.push_str(&interner[mc.name.name]);
                format_list(
                    mc.args,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::LParen, TokenKind::Comma, TokenKind::RParen],
                    false,
                    false,
                    |expr, ctx, indent, interner| {
                        format_expr(expr, ctx, indent, interner);
                    },
                );
            }
            UnitAst::Call(c) => {
                format_expr(c.callee, ctx, indent, interner);
                format_list(
                    c.args,
                    ctx,
                    indent,
                    interner,
                    [TokenKind::LParen, TokenKind::Comma, TokenKind::RParen],
                    false,
                    false,
                    |expr, ctx, indent, interner| {
                        format_expr(expr, ctx, indent, interner);
                    },
                );
            }
            UnitAst::Func(f) => {
                format_list(
                    f.args,
                    ctx,
                    indent,
                    interner,
                    [
                        TokenKind::Op(OpCode::BitOr),
                        TokenKind::Comma,
                        TokenKind::Op(OpCode::BitOr),
                    ],
                    false,
                    false,
                    |param, ctx, indent, interner| {
                        ctx.push_str(&interner[param.name.name]);
                        if let Some(default) = param.default {
                            ctx.push_str(": ");
                            format_unit(default, ctx, indent, interner);
                        }
                    },
                );
                ctx.push(' ');
                format_expr(f.body, ctx, indent, interner);
            }
            UnitAst::Decl(d) => {
                ctx.push_str(&interner[d.name.name]);
                ctx.push_str(": ");
                format_expr(d.expr, ctx, indent, interner);
            }
            UnitAst::Loop(l) => {
                ctx.push_str(TokenKind::Loop.name());
                if let Some(label) = l.label {
                    ctx.push('.');
                    ctx.push_str(&interner[label.name]);
                }
                ctx.push(' ');
                format_expr(l.body, ctx, indent, interner);
            }
            UnitAst::ForLoop(fl) => {
                ctx.push_str(TokenKind::For.name());
                if let Some(label) = fl.label {
                    ctx.push('.');
                    ctx.push_str(&interner[label.name]);
                }
                ctx.push(' ');
                ctx.push_str(&interner[fl.var.name]);
                ctx.push(' ');
                ctx.push_str(TokenKind::In.name());
                ctx.push(' ');
                format_expr(fl.iter, ctx, indent, interner);
                ctx.push(' ');
                format_expr(fl.body, ctx, indent, interner);
            }
            UnitAst::Break(_) => todo!(),
            UnitAst::Continue(_) => todo!(),
            UnitAst::FieldAccess(fa) => {
                format_expr(fa.expr, ctx, indent, interner);
                ctx.push('.');
                ctx.push_str(&interner[fa.field.name]);
            }
            UnitAst::If(i) => {
                ctx.push_str(TokenKind::If.name());
                ctx.push(' ');
                format_expr(i.cond, ctx, indent, interner);
                ctx.push(' ');
                format_expr(i.then, ctx, indent, interner);
                if let Some(else_) = i.else_ {
                    ctx.push(' ');
                    ctx.push_str(TokenKind::Else.name());
                    ctx.push(' ');
                    format_expr(else_, ctx, indent, interner);
                }
            }
            UnitAst::Ret(r) => {
                ctx.push_str(TokenKind::Ret.name());
                if let Some(&expr) = r {
                    ctx.push(' ');
                    format_expr(expr, ctx, indent, interner);
                }
            }
            UnitAst::Paren(_) => todo!(),
        }
    }

    fn perform_test(source_code: &str, ctx: &mut String) {
        let mut files = Files::new();
        let file = File::new("test".into(), source_code.into());
        let (file_id, ..) = files.add_file(file);
        let interner = StrInterner::default();
        let mut diags = Diagnostics::default();
        let mut arena = ArenaBase::new(10_000);
        let scope = arena.scope();
        let mut string_parser = StringParser::default();
        let mut diver = DiverBase::new(1000);

        let mut parser = Parser::new(
            &files,
            file_id,
            &interner,
            &mut diags,
            &scope,
            &mut string_parser,
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
    }
}

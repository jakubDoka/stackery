use std::ops::Range;

use scoped_arena::Scope;

use crate::{Errors, Token};

pub struct Parser<'ctx, 'arena> {
    lexer: logos::Lexer<'arena, Token>,
    arena: &'arena Scope<'arena>,
    ctx: ParserContext,
    errors: &'ctx mut Errors,
}

impl<'ctx, 'arena> Parser<'ctx, 'arena> {
    pub fn new(
        arena: &'arena Scope<'arena>,
        source: &'arena str,
        ctx: Option<ParserContext>,
        errors: &'ctx mut Errors,
    ) -> Self {
        let mut lexer = Token::lexer(source);
        let ctx = ParserContext::new(ctx, &mut lexer);
        Self {
            lexer,
            arena,
            ctx,
            errors,
        }
    }

    pub fn program(mut self) -> (Option<Block<'arena>>, ParserContext) {
        (self.block(), self.snapshot())
    }

    fn block(&mut self) -> Option<Block<'arena>> {
        let mut items = Vec::new();
        loop {
            if let Token::Eof = self.peek().token {
                break;
            }

            let Some(item) = self.unit_expr() else {
                continue;
            };

            items.push(item);
        }

        Some(self.arena.to_scope_from_iter(items))
    }

    fn expr(&mut self) -> Option<Expr<'arena>> {
        let unit = self.unit_expr()?;
        self.expr_recur(unit, u8::MAX)
    }

    fn expr_recur(&mut self, mut prev: Expr<'arena>, prec: u8) -> Option<Expr<'arena>> {
        loop {
            let TokenMeta {
                token: Token::Op(next_prec),
                source,
                span,
            } = self.peek() else {break;};

            if prec >= next_prec {
                break;
            }

            self.next();

            let rhs = self.unit_expr()?;
            let rhs = self.expr_recur(rhs, next_prec)?;

            prev = Expr::BinaryOp {
                op: Name {
                    id: source,
                    span: span.into(),
                },
                lhs: self.arena.to_scope(prev),
                rhs: self.arena.to_scope(rhs),
            };
        }

        Some(prev)
    }

    fn unit_expr(&mut self) -> Option<Expr<'arena>> {
        let TokenMeta {
            token,
            span,
            source,
        } = self.next();
        let mut expr = match token {
            Token::Import => self
                .unexpected_token()
                .context("imports can only be declared at the top of the file")
                .terminate()?,
            Token::ListSep => self
                .unexpected_token()
                .context("no list to separate")
                .terminate()?,
            Token::ListEnd => self
                .unexpected_token()
                .context("no list to terminate")
                .terminate()?,
            Token::Bind => self
                .unexpected_token()
                .context("nothing to bind")
                .terminate()?,
            Token::Call => self
                .unexpected_token()
                .context("nothing to call")
                .terminate()?,
            Token::Use => self.use_expr(span, source),
            Token::Op(..) => Some(Expr::UnaryOp {
                op: Name {
                    id: source,
                    span: span.into(),
                },
                rhs: self.arena.to_scope(self.unit_expr()?),
            }),
            Token::Str => Some(Expr::Word(
                WordKind::Str,
                Name {
                    id: source,
                    span: span.into(),
                },
            )),
            Token::Int => Some(Expr::Word(
                WordKind::Int,
                Name {
                    id: source,
                    span: span.into(),
                },
            )),
            Token::Func => self.func(),
            Token::Eof | Token::Comment => return None,
        };

        if let TokenMeta { token: Token::Call, .. } = self.peek() && let Some(ex_expr) = expr {
            self.next();
            let args = self.list(Self::expr);
            expr = Some(Expr::Call {
                callee: self.arena.to_scope(ex_expr),
                args,
            });
        }

        expr
    }

    fn use_expr(&mut self, span: Range<usize>, source: &'arena str) -> Option<Expr<'arena>> {
        let name = Name {
            id: source,
            span: span.into(),
        };

        let TokenMeta { token: Token::Bind, .. } = self.peek() else {
            return Some(Expr::Word(WordKind::Ident, name));
        };

        self.next();

        let expr = self.expr()?;

        Some(Expr::Binding {
            name,
            value: self.arena.to_scope(expr),
        })
    }

    fn func(&mut self) -> Option<Expr<'arena>> {
        let arguments = self.list(Self::func_arg);
        let body = self.list(Self::expr);

        Some(Expr::Func { arguments, body })
    }

    fn func_arg(&mut self) -> Option<Arg<'arena>> {
        let TokenMeta {
            token: Token::Use,
            span,
            source,
        } = self.next() else { return None; };

        Some(Arg {
            id: source,
            span: span.into(),
        })
    }

    pub fn imports(mut self) -> (Imports<'arena>, ParserContext) {
        let Token::Import = self.peek().token else {
            return (&[], self.snapshot());
        };
        self.next();

        let imports = self.list(Self::import);

        (imports, self.snapshot())
    }

    fn import(&mut self) -> Option<Import<'arena>> {
        let TokenMeta { token: Token::Str, span, source } = self.next() else {
            self.unexpected_token().context("expected string");
            return None;
        };
        let id = source.trim_matches('"');

        let alias = if let TokenMeta {
            token: Token::Use,
            span,
            source,
        } = self.peek()
        {
            self.next();
            Name {
                id: source,
                span: span.into(),
            }
        } else {
            Name {
                id,
                span: span.into(),
            }
        };

        Some(Import { id, alias })
    }

    fn list<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>) -> &'arena [T] {
        let mut items = Vec::new();
        loop {
            let Some(item) = f(self ) else {
                break;
            };

            items.push(item);

            match self.next().token {
                Token::ListSep => continue,
                Token::ListEnd => break,
                _ => {
                    self.unexpected_token()
                        .context("expected comma or semicolon");
                    break;
                }
            }
        }
        self.arena.to_scope_from_iter(items)
    }

    fn next(&mut self) -> TokenMeta<'arena> {
        self.ctx.next(&mut self.lexer)
    }

    fn peek(&mut self) -> TokenMeta<'arena> {
        self.ctx.peek(&self.lexer)
    }

    fn unexpected_token(&mut self) -> crate::ErrorBuilder {
        self.errors
            .error(self.lexer.span())
            .message("unexpected token")
    }

    fn snapshot(self) -> ParserContext {
        self.ctx
    }
}

#[derive(Clone, Copy)]
pub struct Import<'arena> {
    pub id: &'arena str,
    pub alias: Name<'arena>,
}

#[derive(Clone, Copy)]
pub struct Name<'arena> {
    pub id: &'arena str,
    pub span: Span,
}

pub type Block<'arena> = &'arena [Expr<'arena>];

#[derive(Clone, Copy)]
pub enum Expr<'arena> {
    Word(WordKind, Name<'arena>),
    BinaryOp {
        lhs: &'arena Expr<'arena>,
        op: Name<'arena>,
        rhs: &'arena Expr<'arena>,
    },
    UnaryOp {
        op: Name<'arena>,
        rhs: &'arena Expr<'arena>,
    },
    Call {
        callee: &'arena Expr<'arena>,
        args: &'arena [Expr<'arena>],
    },
    Binding {
        name: Name<'arena>,
        value: &'arena Expr<'arena>,
    },
    Func {
        arguments: &'arena [Arg<'arena>],
        body: &'arena [Expr<'arena>],
    },
}

impl<'a> Expr<'a> {
    pub(crate) fn span(&self) -> Span {
        match self {
            Expr::Word(_, name) => name.span,
            Expr::BinaryOp { lhs, rhs, .. } => lhs.span().join(rhs.span()),
            Expr::UnaryOp { op, rhs } => op.span.join(rhs.span()),
            Expr::Call { callee, args } => callee.span().join(args.last().unwrap().span()),
            Expr::Binding { name, value } => name.span.join(value.span()),
            Expr::Func { arguments, body } => arguments
                .first()
                .map(|arg| arg.span)
                .unwrap_or_default()
                .join(body.last().unwrap().span()),
        }
    }
}

pub type Arg<'arena> = Name<'arena>;

#[derive(Clone, Copy)]
pub enum WordKind {
    Ident,
    Str,
    Int,
}

#[derive(Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn join(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

pub type Imports<'arena> = &'arena [Import<'arena>];

#[derive(Default, Clone)]
pub struct ParserContext {
    next: Token,
    next_span: Range<usize>,
}

impl ParserContext {
    fn new(ctx: Option<ParserContext>, lexer: &mut logos::Lexer<Token>) -> ParserContext {
        let mut ctx = ctx.unwrap_or_default();
        lexer.bump(ctx.next_span.end);

        // if lexer already finished, nothing really changes
        if ctx.next == Token::Eof {
            ctx.next(lexer);
        }

        ctx
    }

    fn next<'a>(&mut self, lexer: &mut logos::Lexer<'a, Token>) -> TokenMeta<'a> {
        let meta = self.peek(lexer);

        self.next = Self::next_token(lexer);
        self.next_span = lexer.span();

        meta
    }

    fn peek<'a>(&mut self, lexer: &logos::Lexer<'a, Token>) -> TokenMeta<'a> {
        TokenMeta {
            token: self.next,
            span: self.next_span.clone(),
            source: &lexer.source()[self.next_span.clone()],
        }
    }

    fn next_token(lexer: &mut logos::Lexer<Token>) -> Token {
        lexer
            .by_ref()
            .find_map(|tok| match tok {
                Err(()) | Ok(Token::Comment) => None,
                Ok(tok) => Some(tok),
            })
            .unwrap_or_default()
    }
}

struct TokenMeta<'a> {
    token: Token,
    span: Range<usize>,
    source: &'a str,
}

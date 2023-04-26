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

    pub fn imports(mut self) -> (Imports<'arena>, ParserContext) {
        let Some(Token::Import) = self.peek().token else {
            return (&[], self.snapshot());
        };
        self.next();

        let imports = self.list(Self::import);

        (imports, self.snapshot())
    }

    fn import(&mut self) -> Option<Import<'arena>> {
        let TokenMeta { token: Some(Token::Str), span, source } = self.next() else {
            self.unexpected_token().context("expected string");
            return None;
        };
        let id = source.trim_matches('"');

        let alias = if let TokenMeta {
            token: Some(Token::Declare),
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
                Some(Token::ListSep) => continue,
                Some(Token::ListEnd) => break,
                Some(..) => {
                    self.unexpected_token()
                        .context("expected comma or semicolon");
                    break;
                }
                None => break,
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

#[derive(Clone, Copy, Default)]
pub struct Import<'arena> {
    pub id: &'arena str,
    pub alias: Name<'arena>,
}

#[derive(Clone, Copy, Default)]
pub struct Name<'arena> {
    pub id: &'arena str,
    pub span: Span,
}

#[derive(Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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

#[derive(Default)]
pub struct ParserContext {
    next: Option<Token>,
    next_span: Range<usize>,
}

impl ParserContext {
    fn new(ctx: Option<ParserContext>, lexer: &mut logos::Lexer<Token>) -> ParserContext {
        let mut ctx = ctx.unwrap_or_default();
        lexer.bump(ctx.next_span.end);

        // if lexer already finished, nothing really changes
        if ctx.next.is_none() {
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

    fn next_token(lexer: &mut logos::Lexer<Token>) -> Option<Token> {
        lexer.by_ref().find_map(|tok| match tok {
            Err(()) | Ok(Token::Comment) => None,
            Ok(tok) => Some(tok),
        })
    }
}

struct TokenMeta<'a> {
    token: Option<Token>,
    span: Range<usize>,
    source: &'a str,
}

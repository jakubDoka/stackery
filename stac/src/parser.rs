use std::fmt::Display;

use mini_alloc::{ArenaScope, Diver, IdentStr};

use crate::{Diagnostics, FileRef, Files, Lexer, Severty, Token, TokenKind};

pub mod expr;
pub mod fmt;

pub struct Parser<'ctx, 'src, 'arena, 'arena_ctx> {
    files: &'src Files,
    next: Token<'src>,
    lexer: Lexer<'src>,
    diags: &'ctx mut Diagnostics,
    arena: &'arena ArenaScope<'arena_ctx>,
}

impl<'ctx, 'src, 'arena, 'arena_ctx> Parser<'ctx, 'src, 'arena, 'arena_ctx> {
    pub fn new(
        files: &'src Files,
        file: FileRef,
        diags: &'ctx mut Diagnostics,
        arena: &'arena ArenaScope<'arena_ctx>,
    ) -> Self {
        let mut lexer = Lexer::new(files, file);
        Self {
            files,
            next: lexer.next_tok(),
            lexer,
            diags,
            arena,
        }
    }

    fn sequence<T>(
        &mut self,
        mut diver: Diver,
        mut f: impl FnMut(&mut Self, Diver) -> Option<T>,
        sep: impl TokenPattern,
        end: impl TokenPattern,
        objective: impl Display,
    ) -> Option<(&'arena mut [T], bool)> {
        let mut elems = diver.dive::<T>();

        let trailing_sep = loop {
            if self.try_advance(end.clone()).is_some() {
                break elems.len() > 0;
            }

            let elem = f(self, elems.untyped_dive())?;
            elems.push(elem);

            if self.try_advance(end.clone()).is_some() {
                break false;
            }

            self.expect_advance(
                sep.clone(),
                format_args!("missing separateor in {objective} list"),
            )?;
        };

        Some((self.arena.alloc_rev_iter(elems), trailing_sep))
    }

    fn try_advance(&mut self, pat: impl TokenPattern) -> Option<Token<'src>> {
        pat.matches(self.peek()).then(|| self.next())
    }

    fn expect_advance(&mut self, pat: impl TokenPattern, msg: impl Display) -> Option<Token<'src>> {
        self.try_advance(pat.clone()).or_else(|| {
            self.diags
                .builder(self.files)
                .annotation(
                    Severty::Error,
                    self.next.span,
                    format_args!("expected {} but got {}", pat.display(), self.next.kind),
                )
                .footer(Severty::Error, msg)
                .terminate()?
        })
    }

    fn next(&mut self) -> Token<'src> {
        let tok = self.next;
        self.next = self.lexer.next_tok();
        tok
    }

    fn peek(&self) -> Token<'src> {
        self.next
    }
}

trait TokenPattern: Clone {
    fn matches(&self, tok: Token) -> bool;
    fn display(&self) -> impl Display + '_;
}

impl TokenPattern for TokenKind {
    fn matches(&self, tok: Token) -> bool {
        tok.kind == *self
    }

    fn display(&self) -> impl Display + '_ {
        self
    }
}

impl TokenPattern for &str {
    fn matches(&self, tok: Token) -> bool {
        tok.source == *self
    }

    fn display(&self) -> impl Display + '_ {
        self
    }
}

impl<T: TokenPattern, const N: usize> TokenPattern for [T; N] {
    fn matches(&self, tok: Token) -> bool {
        self.iter().any(|pat| pat.matches(tok))
    }

    fn display(&self) -> impl Display + '_ {
        self.iter()
            .map(TokenPattern::display)
            .fold(String::new(), |mut acc, pat| {
                if !acc.is_empty() {
                    acc.push_str(" or ");
                }
                use std::fmt::Write;
                write!(acc, "{}", pat).unwrap();
                acc
            })
    }
}

pub trait TransposeOpt {
    fn transpose(self) -> Self;
}

impl<T> TransposeOpt for Option<Option<T>> {
    fn transpose(self) -> Self {
        match self {
            Some(Some(x)) => Some(Some(x)),
            Some(None) => None,
            None => Some(None),
        }
    }
}

#[derive(Default)]
pub struct StringParser {
    buffer: String,
}

pub enum StringParseError {
    InvalidEscape(usize),
    IncompleteEscape,
}

impl StringParser {
    pub fn parse(&mut self, source: &str) -> Result<IdentStr, StringParseError> {
        self.buffer.clear();
        let mut chars = source.char_indices();
        while let Some((_, c)) = chars.next() {
            match c {
                '\\' => {
                    let (i, c) = chars.next().ok_or(StringParseError::IncompleteEscape)?;
                    match c {
                        'n' => self.buffer.push('\n'),
                        'r' => self.buffer.push('\r'),
                        't' => self.buffer.push('\t'),
                        '\\' => self.buffer.push('\\'),
                        '"' => self.buffer.push('"'),
                        '\'' => self.buffer.push('\''),
                        '0' => self.buffer.push('\0'),
                        _ => return Result::Err(StringParseError::InvalidEscape(i)),
                    }
                }
                _ => self.buffer.push(c),
            }
        }
        Ok(IdentStr::from_str(&self.buffer))
    }
}

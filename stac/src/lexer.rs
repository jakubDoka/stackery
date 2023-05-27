use std::fmt::Display;

use crate::{FileRef, Files, Span};

pub struct Token<'a> {
    pub token: TokenKind,
    pub source: &'a str,
    pub span: Span,
}

#[derive(Default)]
pub struct Extras {
    line: usize,
    last_newline: usize,
}

pub struct Lexer<'a> {
    lexer: logos::Lexer<'a, TokenKind>,
    file: FileRef,
}

impl<'a> Lexer<'a> {
    pub fn new(files: &'a Files, file: FileRef) -> Self {
        Self {
            lexer: TokenKind::lexer(files.get_file(file).source()),
            file,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.lexer.next()?;
        let span = self.lexer.span();
        let source = self.lexer.slice();

        let span = Span::new(
            self.lexer.extras.line,
            span.end - self.lexer.extras.last_newline,
            self.file,
        );

        Some(tok.map(|token| Token {
            token,
            source,
            span,
        }))
    }
}

macro_rules! define_lexer {
    (
        subpatterns {$(
            $subpattern:ident = $subpattern_repr:literal
        )*}
        tokens {$(
            $token:ident = $token_repr:literal
        )*}
        regexes {$(
            $regex:ident = $regex_repr:literal
        )*}
        operators {$(
            ($($op:literal)*) = $prec:literal
        )*}
    ) => {
        #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, logos::Logos)]
        #[logos(skip r"([ \t\r]+|//.*\n)")]
        #[logos(extras = Extras)]
        $(#[logos(subpattern $subpattern = $subpattern_repr)])*
        pub enum TokenKind {
            $(#[token($token_repr)] $token,)*

            $(#[regex($regex_repr)] $regex,)*

            $($(#[token($op, |_| $prec)])*)*
            Op(u8),

            #[default]
            #[token("\n", handle_newlien)]
            Eof,
        }

        impl TokenKind {
            pub const TOKEN_COUNT: usize = [$($token_repr,)* $($regex_repr,)*].len();

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$token => stringify!($token),)*
                    $(Self::$regex => stringify!($regex),)*
                    Self::Op(_) => "operator",
                    Self::Eof => "end of file",
                }
            }
        }
    };

    (@token_repr $token:ident = $token_repr:literal) => {
        $token_repr
    };

    (@token_repr $token:ident) => {
        stringify!($token)
    };
}

fn handle_newlien(lex: &mut logos::Lexer<TokenKind>) -> logos::Skip {
    lex.extras.line += 1;
    lex.extras.last_newline = lex.span().end;
    logos::Skip
}

define_lexer! {
    subpatterns {
        ident_start = r"[a-zA-Z_]"
        ident_content = r"[a-zA-Z0-9_]"
    }

    tokens {
        If = "if"
        Else = "else"
        Loop = "loop"
        Fn = "fn"
        Ret = "ret"
        For = "for"
        In = "in"

        Dot = "."
        Comma = ","
        Semi = ";"
        Colon = ":"
        Enum = "|{"
        Struct = "*{"
        LBrace = "{"
        RBrace = "}"
        LBracket = "["
        RBracket = "]"
        Tuple = "*("
        LParen = "("
        RParen = ")"
    }

    regexes {
        Ident = r"(?&ident_start)(?&ident_content)*"
        Import = r":\{[^ \n\r\t]*\}"
        Str = r#""([^"]|\\")*""#
        Int = r"[0-9]+"

        Comment = r"//.*"
    }

    operators {
        ("*" "/") = 3
        ("+" "-") = 4
    }
}

impl TokenKind {
    pub const ERROR: &str = "Error";

    pub fn lexer(input: &str) -> logos::Lexer<TokenKind> {
        logos::Logos::lexer(input)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

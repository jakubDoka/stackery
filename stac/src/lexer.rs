use std::fmt::Display;

use crate::Span;

pub struct TokenMeta<'a> {
    pub token: Token,
    pub source: &'a str,
    pub span: Span,
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
        #[logos(skip r"[ \t\n\r]+")]
        $(#[logos(subpattern $subpattern = $subpattern_repr)])*
        pub enum Token {
            $(#[token($token_repr)] $token,)*

            $(#[regex($regex_repr)] $regex,)*

            $($(#[token($op, |_| $prec)])*)*
            Op(u8),

            #[default]
            Eof,
        }

        impl Token {
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

impl Token {
    pub const ERROR: &str = "Error";

    pub fn lexer(input: &str) -> logos::Lexer<Token> {
        logos::Logos::lexer(input)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

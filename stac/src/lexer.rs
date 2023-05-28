use std::fmt::Display;

use crate::{FileRef, Files, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    pub kind: TokenKind,
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

    #[cfg(test)]
    pub fn with_fake_file(source: &'a str) -> Self {
        Self {
            lexer: TokenKind::lexer(source),
            file: FileRef::fake(),
        }
    }

    pub fn next_tok(&mut self) -> Token<'a> {
        let kind = self
            .lexer
            .next()
            .unwrap_or(Ok(TokenKind::Eof))
            .unwrap_or(TokenKind::Err);
        let span = self.lexer.span();
        let source = self.lexer.slice();

        let span = Span::new(
            self.lexer.extras.line,
            span.start - self.lexer.extras.last_newline,
            self.file,
        );

        Token { kind, source, span }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_tok())
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
            ($($op_name:ident: $op:literal),*) = $prec:literal,
        )*}
    ) => {
        #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, logos::Logos)]
        #[logos(skip r"([ \t\r]+|//[^\n]*)")]
        #[logos(extras = Extras)]
        $(#[logos(subpattern $subpattern = $subpattern_repr)])*
        pub enum TokenKind {
            $(#[token($token_repr)] $token,)*

            $(#[regex($regex_repr)] $regex,)*

            $($(#[token($op, |_| OpCode::$op_name)])*)*
            Op(OpCode),

            #[default]
            #[token("\n", handle_newlien)]
            Eof,

            Err,
        }

        impl TokenKind {
            pub const TOKEN_COUNT: usize = [$($token_repr,)* $($regex_repr,)*].len();

            pub const ALL: &[Self] = &[
               $(Self::$token,)*
               $(Self::$regex,)*
               $($(Self::Op(OpCode::$op_name),)*)*
            ];

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$token => $token_repr,)*
                    $(Self::$regex => stringify!($regex),)*
                    Self::Op(o) => o.name(),
                    Self::Eof => "end of file",
                    Self::Err => "error",
                }
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum OpCode {
            $($($op_name,)*)*
        }

        impl OpCode {
            pub fn name(self) -> &'static str {
                match self {
                    $($(Self::$op_name => $op,)*)*
                }
            }

            pub fn prec(self) -> u8 {
                match self {
                    $($(Self::$op_name)|* => $prec,)*
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
        Ret = "ret"
        For = "for"
        In = "in"

        Dot = "."
        DoubleDot = ".."
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
        Import = r":\{[^ \n\r\t}]*\}"
        Str = r#""([^"]|\\")*""#
        Int = r"[0-9]+"
    }

    operators {
        (Mul: "*", Div: "/", Mod: "%") = 3,
        (Add: "+", Sub: "-") = 4,
        (Shl: "<<", Shr: ">>") = 5,
        (BitAnd: "&", BitOr: "|", BitXor: "^") = 6,
        (Eq: "==", Ne: "!=", Lt: "<", Le: "<=", Gt: ">", Ge: ">=") = 7,
        (And: "&&", Or: "||") = 8,
        (Assign: "=", AddAssign: "+=", SubAssign: "-=", MulAssign: "*=", DivAssign: "/=", ModAssign: "%=",
         ShlAssign: "<<=", ShrAssign: ">>=", BitAndAssign: "&=", BitOrAssign: "|=", BitXorAssign: "^=") = 9,
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

#[cfg(test)]
mod test {
    use crate::TokenKind;

    #[test]
    fn all_tokens() {
        let code = "
            if else loop ret for in
            . .. , ; : |{ *{ { } [ ] *( ( )
            ident
            :{import}
            \"str\"
            123
            * / % + - << >> & | ^ == != < <= > >= && || = += -= *= /= %= <<= >>= &= |= ^= //
        ";

        let actual = TokenKind::lexer(code)
            .filter_map(Result::ok)
            .collect::<Vec<_>>();

        let expected = TokenKind::ALL;

        assert_eq!(actual, expected);
    }
}

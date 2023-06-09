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
            lexer: TokenKind::lexer(files[file].source()),
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
        other {$(
            $(#[$attr:meta])*
            $other_name:ident = $other_repr:literal
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

            $(
                $(#[$attr])*
                $other_name,
            )*
        }

        impl TokenKind {
            pub const TOKEN_COUNT: usize = [$($token_repr,)* $($regex_repr,)*].len();

            pub const ALL: &[Self] = &[
                $(Self::$token,)*
                $(Self::$regex,)*
                $($(Self::Op(OpCode::$op_name),)*)*
                $(Self::$other_name,)*
            ];

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$token => $token_repr,)*
                    $(Self::$regex => stringify!($regex),)*
                    Self::Op(o) => o.name(),
                    $(Self::$other_name => $other_repr,)*
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

fn handle_newline(lex: &mut logos::Lexer<TokenKind>) -> logos::Skip {
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
        Break = "break"
        Continue = "continue"
        Ret = "ret"
        For = "for"
        In = "in"
        Unknown = "unknown"
        True = "true"
        False = "false"

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
        MetaIdent = r"\$(?&ident_start)(?&ident_content)*"
        Import = r":\{[^}]*\}"
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

    other {
            #[default]
            #[token("\n", handle_newline)]
            Eof = "EOF"
            Err = "Error"
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

#[derive(logos::Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip "([^:]+|:)")]
enum DependencyTokenKind {
    #[regex(r":\{[^}]*\}")]
    Import,
}

pub struct ImportLexer<'a> {
    inner: logos::Lexer<'a, DependencyTokenKind>,
}

impl<'a> ImportLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: logos::Logos::lexer(source),
        }
    }
}

impl<'a> Iterator for ImportLexer<'a> {
    type Item = (&'a str, usize);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.inner.next()?;
        Some((
            unsafe {
                self.inner
                    .slice()
                    .get_unchecked(2..self.inner.slice().len() - 1)
            },
            self.inner.span().start,
        ))
    }
}

#[cfg(test)]
mod test {
    use std::iter;

    use crate::{ImportLexer, TokenKind};

    pub struct ImportLexer2<'a> {
        source: &'a str,
    }

    impl<'a> ImportLexer2<'a> {
        pub fn new(source: &'a str) -> Self {
            Self { source }
        }
    }

    impl<'a> Iterator for ImportLexer2<'a> {
        type Item = &'a str;

        fn next(&mut self) -> Option<Self::Item> {
            let start = self.source.find(":{")? + ":{".len();
            self.source = &self.source[start..];
            let end = self.source.find('}')?;
            let (import, rest) = self.source.split_at(end);
            self.source = rest;
            Some(import)
        }
    }

    #[test]
    fn all_tokens() {
        let code = "
            if else loop break continue ret for in unknown true false
            . .. , ; : |{ *{ { } [ ] *( ( )
            ident
            $meta_ident
            :{import}
            \"str\"
            123
            * / % + - << >> & | ^ == != < <= > >= && || = += -= *= /= %= <<= >>= &= |= ^= //
        ";

        let actual = TokenKind::lexer(code)
            .filter_map(Result::ok)
            .collect::<Vec<_>>();

        let expected = &TokenKind::ALL[..TokenKind::ALL.len() - 2];

        assert_eq!(actual, expected);
    }

    #[test]
    fn dependency_token() {
        let code = ":{import} akshd  : { } lkjas hdlas kjhdlakjs dhlsakj dhlsa kjdhlsak jdhalksjd 
                    hlkjsad hlak jdshlda kshda lkjh alkj hdalks jdhalksj hdaslkj hdalkj hdlas
                    :{import from here}";

        let actual = ImportLexer::new(code).map(|(s, _)| s).collect::<Vec<_>>();

        assert_eq!(actual, vec!["import", "import from here",]);
    }

    fn generate_bench_code() -> String {
        let mut code = String::new();

        let import_variations = "
            import
            a ljf lakjf lhdsakjh falksdjhl kjdsahalf 
            kajhfldaskjf hdslakjhflaksj hlfkjsa hlkj hsalkj
            os:./path/to/file
            git:github.com/username/repo
            laskdjjjjjjj;][;][;-=o=-][l;3ql42';45l23';46l764[p8l0';5l]
        "
        .lines()
        .map(str::trim)
        .filter(|s| str::is_empty(s))
        .map(|i| format!(":{{{i}}}"))
        .collect::<Vec<_>>();
        let tokens = TokenKind::ALL.iter().map(|t| t.name()).collect::<Vec<_>>();
        let numbers = (0..100).map(|i| i.to_string()).collect::<Vec<_>>();

        let dataset = import_variations
            .iter()
            .map(String::as_str)
            .chain(tokens.iter().cycle().take(100).cloned())
            .chain(numbers.iter().map(String::as_str))
            .chain(iter::repeat("\n").take(1000))
            .collect::<Vec<_>>();

        let mut accum = 0x_dead_beef_u64;
        for i in 0..30000 {
            accum = accum.wrapping_mul(i) ^ i.wrapping_add(accum) ^ i.wrapping_sub(accum);
            let idx = accum as usize % dataset.len();

            code.push_str(dataset[idx]);
            code.push(' ');
        }

        code
    }

    #[test]
    #[ignore]
    fn bench_dpendency_token() {
        let code = generate_bench_code();
        println!("Code len: {}", code.len());

        let iters = 10000;

        let mut samples = Vec::with_capacity(iters);
        let mut res = 0;
        for _ in 0..iters {
            let now = std::time::Instant::now();
            res = ImportLexer2::new(&code).count();
            let elapsed = now.elapsed();
            samples.push(elapsed);
        }

        println!(
            "Fake: {:?}",
            samples.drain(..).sum::<std::time::Duration>() / iters as u32
        );

        let mut fake_res = 0;
        for _ in 0..iters {
            let now = std::time::Instant::now();
            fake_res = ImportLexer::new(&code).count();
            let elapsed = now.elapsed();
            samples.push(elapsed);
        }

        println!(
            "Logos: {:?}",
            samples.drain(..).sum::<std::time::Duration>() / iters as u32
        );

        assert_eq!(res, fake_res);
    }
}

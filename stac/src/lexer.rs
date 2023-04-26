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
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, logos::Logos)]
        #[logos(skip r"[ \t\n\r]+")]
        #[repr(C)]
        $(#[logos(subpattern $subpattern = $subpattern_repr)])*
        pub enum Token {
            $(#[token($token_repr)] $token,)*

            $(#[regex($regex_repr)] $regex,)*
        }

        impl Token {
            pub const TOKEN_COUNT: usize = [$($token_repr,)* $($regex_repr,)*].len();

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$token => stringify!($token),)*
                    $(Self::$regex => stringify!($regex),)*
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
        Import = "use"
        Decompose = "="
        ListSep = ","
        ListEnd = ";"
    }

    regexes {
        Use = r"(?&ident_start)(?&ident_content)*"
        Declare = r"@(?&ident_content)+"
        Bind = r"\$(?&ident_content)+"

        Str = r#""([^"]|\\")*""#
        Int = r"[0-9]+"

        Comment = r"//.*"
    }
}

impl Token {
    pub const ERROR: &str = "Error";

    pub fn lexer(input: &str) -> logos::Lexer<Token> {
        logos::Logos::lexer(input)
    }

    pub fn id(self) -> usize {
        self as usize
    }
}

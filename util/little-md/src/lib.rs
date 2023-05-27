use std::mem;

pub fn emit(markdown: &str, config: Config, buffer: &mut String) {
    use logos::Logos;

    fn tag_start(tag: &str, attributes: ElementAttr, buffer: &mut String) {
        buffer.push('<');
        buffer.push_str(tag);
        attributes.emit(buffer);
        buffer.push('>');
    }

    fn tag_end(tag: &str, buffer: &mut String) {
        buffer.push_str("</");
        buffer.push_str(tag);
        buffer.push('>');
    }

    fn tag(tag: &str, attribute: ElementAttr, content: &str, buffer: &mut String) {
        tag_start(tag, attribute, buffer);
        buffer.push_str(content);
        tag_end(tag, buffer);
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    enum TextStyle {
        Italic,
        Bold,
        Strikethrough,
        Code,
    }

    const TEXT_STYLE_TAGS: [&str; 4] = ["i", "b", "s", "code"];

    let mut lexer = Token::lexer(markdown);
    let mut opened_paragraph = false;
    let mut style_stack = [TextStyle::Code; 4];
    let mut style_stack_len = 0;
    let mut push_style = |s: TextStyle, buffer: &mut String| {
        if style_stack_len > 0 && style_stack[style_stack_len - 1] == s {
            tag_end(TEXT_STYLE_TAGS[s as usize], buffer);
            style_stack_len -= 1;
            return;
        }

        if style_stack_len == style_stack.len() || style_stack[..style_stack_len].contains(&s) {
            return;
        }

        let tags = match s {
            TextStyle::Italic => config.italic,
            TextStyle::Bold => config.bold,
            TextStyle::Strikethrough => config.strikethrough,
            TextStyle::Code => config.code,
        };

        tag_start(TEXT_STYLE_TAGS[s as usize], tags, buffer);
        style_stack[style_stack_len] = s;
        style_stack_len += 1;
    };

    while let Some(token) = lexer.next() {
        let Ok(token) = token else {
            tag("span", config.error, lexer.slice(),  buffer);
            continue;
        };

        let is_in_paragraph = matches!(
            token,
            Token::Text
                | Token::Italic
                | Token::Bold
                | Token::Strikethrough
                | Token::Code
                | Token::WhiteSpace(false)
                | Token::Escape
                | Token::Link
        );

        match (is_in_paragraph, opened_paragraph) {
            (true, false) => tag_start("p", config.paragraph, buffer),
            (false, true) => tag_end("p", buffer),
            _ => {}
        }
        opened_paragraph = is_in_paragraph;

        match token {
            Token::Text => buffer.push_str(lexer.slice()),
            Token::WhiteSpace(false) => buffer.push(' '),
            Token::WhiteSpace(true) => {}
            Token::Italic => push_style(TextStyle::Italic, buffer),
            Token::Bold => push_style(TextStyle::Bold, buffer),
            Token::Strikethrough => push_style(TextStyle::Strikethrough, buffer),
            Token::Code => push_style(TextStyle::Code, buffer),
            Token::CodeBlock => {
                let (lang, code) = split_code_block(lexer.slice());

                let syntax_opt = config.syntaxes.iter_mut().find(|s| s.name() == lang);
                let Some(syntax) = syntax_opt else {
                    tag("pre", config.code_block, code,  buffer);
                    continue;
                };

                tag_start("pre", config.code_block, buffer);
                syntax.emit(code, buffer);
                tag_end("pre", buffer);
            }
            Token::Heading => {
                const TAGS: [&str; 6] = ["h1", "h2", "h3", "h4", "h5", "h6"];
                let (hashes, heading) = strip_hashes(lexer.slice());
                let tag_name = TAGS[hashes - 1];
                tag(tag_name, config.heading, heading, buffer);
            }
            Token::Escape => {
                let escaped = &lexer.slice()[1..];
                buffer.push_str(escaped);
            }
            Token::Link => {
                let (text, link) = parse_link(lexer.slice());

                buffer.push_str("<a href=\"");
                buffer.push_str(link);
                buffer.push('"');
                config.link.emit(buffer);
                buffer.push('>');

                buffer.push_str(text);

                tag_end("a", buffer);
            }
        }
    }

    if opened_paragraph {
        tag_end("p", buffer);
    }
}

#[derive(logos::Logos, Clone, Copy, Debug, PartialEq, Eq)]
enum Token {
    #[regex(r"[^\s*~`#\\\[\]\(\)]+")]
    Text,
    #[regex(r"\s+", is_paragraph_sep)]
    WhiteSpace(bool),
    #[token("*")]
    Italic,
    #[token("**")]
    Bold,
    #[token("~~")]
    Strikethrough,
    #[token("`")]
    Code,
    #[regex("```[a-z]*\n", parse_code_block)]
    CodeBlock,
    #[regex(r"#{1,6}.*")]
    Heading,
    #[regex(r"\\[*~`#\\\[\]\(\)]+")]
    Escape,
    #[regex(r"\[[^\]]*\]\([^\)]*\)")]
    Link,
}

fn parse_link(mut link: &str) -> (&str, &str) {
    link = &link[1..link.len() - 1];
    link.split_once("](").unwrap()
}

fn strip_hashes(heading: &str) -> (usize, &str) {
    let mut chars = heading.chars();
    let hashes = chars.by_ref().take_while(|c| *c == '#').count();
    let heading = chars.as_str().trim();
    (hashes, heading)
}

fn split_code_block(mut code_block: &str) -> (&str, &str) {
    code_block = &code_block["```".len()..];
    let (lang, rest) = code_block.split_once('\n').unwrap();
    (lang, &rest[..rest.len() - "\n```".len()])
}

fn is_paragraph_sep(lex: &mut logos::Lexer<Token>) -> bool {
    lex.slice().splitn(3, '\n').count() == 3
}

fn parse_code_block(lex: &mut logos::Lexer<Token>) {
    let pattern = "\n```";
    let pattern_len = pattern.len();
    let end = lex
        .remainder()
        .find(pattern)
        .map(|i| i + pattern_len)
        .unwrap_or(lex.remainder().len());
    lex.bump(end);
}

pub struct Config<'a> {
    pub bold: ElementAttr<'a>,
    pub italic: ElementAttr<'a>,
    pub strikethrough: ElementAttr<'a>,
    pub code: ElementAttr<'a>,
    pub code_block: ElementAttr<'a>,
    pub heading: ElementAttr<'a>,
    pub error: ElementAttr<'a>,
    pub paragraph: ElementAttr<'a>,
    pub link: ElementAttr<'a>,
    pub syntaxes: &'a mut [&'a mut dyn Syntax],
}

impl Config<'static> {
    pub fn empty() -> Self {
        Config {
            bold: ElementAttr::empty(),
            italic: ElementAttr::empty(),
            strikethrough: ElementAttr::empty(),
            code: ElementAttr::empty(),
            code_block: ElementAttr::empty(),
            heading: ElementAttr::empty(),
            error: ElementAttr::empty(),
            paragraph: ElementAttr::empty(),
            link: ElementAttr::empty(),
            syntaxes: &mut [],
        }
    }
}

impl Default for Config<'_> {
    fn default() -> Self {
        Self {
            bold: ElementAttr::class("md-bold"),
            italic: ElementAttr::class("md-italic"),
            strikethrough: ElementAttr::class("md-strikethrough"),
            code: ElementAttr::class("md-code"),
            code_block: ElementAttr::class("md-code-block"),
            heading: ElementAttr::class("md-heading"),
            error: ElementAttr::class("md-error"),
            paragraph: ElementAttr::class("md-paragraph"),
            link: ElementAttr::class("md-link"),
            syntaxes: &mut [],
        }
    }
}

#[derive(Clone, Copy)]
pub struct ElementAttr<'a> {
    content: &'a str,
    as_class: bool,
}

impl<'a> ElementAttr<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            content,
            as_class: true,
        }
    }

    pub fn class(content: &'a str) -> Self {
        Self {
            content,
            as_class: true,
        }
    }

    pub fn empty() -> Self {
        Self {
            content: "",
            as_class: true,
        }
    }

    fn emit(&self, buffer: &mut String) {
        if self.content.is_empty() {
            return;
        }

        buffer.push(' ');

        if self.as_class {
            buffer.push_str("class=\"");
            buffer.push_str(self.content);
            buffer.push('"');
        } else {
            buffer.push_str(self.content);
        }
    }
}

pub trait Syntax {
    fn name(&self) -> &str;
    fn emit(&mut self, code: &str, buffer: &mut String);
}

pub trait LogosSyntaxConfig {
    const NAME: &'static str;
    type Token: for<'a> logos::Logos<'a, Extras = (), Error = (), Source = str>;
    fn style<'a>(&mut self, token: Result<Self::Token, ()>, buffer: &mut String);
    fn clear(&mut self);
}

pub trait ToCodeCategory {
    const NAME: &'static str;
    type Token: for<'a> logos::Logos<'a, Extras = (), Error = (), Source = str> + Eq + Copy;
    fn to_code_category(s: Result<Self::Token, ()>) -> CodeCategory;
}

pub struct CodeCategorySyntaxConfig<T: ToCodeCategory> {
    prev: Option<Result<T::Token, ()>>,
}

impl<T: ToCodeCategory> CodeCategorySyntaxConfig<T> {
    pub fn new() -> Self {
        Self { prev: None }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeCategory {
    Keyword,
    String,
    Number,
    Comment,
    Ident,
    Declaration,
    Punctuation,
    Error,
}

impl CodeCategory {
    pub fn to_class(&self) -> &'static str {
        match self {
            CodeCategory::Keyword => "code-keyword",
            CodeCategory::String => "code-string",
            CodeCategory::Number => "code-number",
            CodeCategory::Comment => "code-comment",
            CodeCategory::Ident => "code-ident",
            CodeCategory::Declaration => "code-declaration",
            CodeCategory::Punctuation => "code-punctation",
            CodeCategory::Error => "code-error",
        }
    }
}

impl<T: ToCodeCategory> LogosSyntaxConfig for CodeCategorySyntaxConfig<T> {
    const NAME: &'static str = T::NAME;

    type Token = T::Token;

    fn style(&mut self, token: Result<Self::Token, ()>, buffer: &mut String) {
        if Some(token) == mem::replace(&mut self.prev, Some(token)) {
            return;
        }

        let class = T::to_code_category(token).to_class();
        buffer.push_str("class=\"");
        buffer.push_str(class);
        buffer.push('"');
    }

    fn clear(&mut self) {
        self.prev = None;
    }
}

pub struct LogosSyntax<T: LogosSyntaxConfig> {
    config: T,
}

impl<T: LogosSyntaxConfig> LogosSyntax<T> {
    pub fn new(config: T) -> Self {
        Self { config }
    }
}

impl<T: LogosSyntaxConfig> Syntax for LogosSyntax<T> {
    fn name(&self) -> &str {
        T::NAME
    }

    fn emit(&mut self, code: &str, buffer: &mut String) {
        use logos::Logos;

        let mut lexer = T::Token::lexer(code);
        let mut is_span_open = false;
        let mut prev_token_end = 0;
        while let Some(token) = lexer.next() {
            buffer.push_str(&code[prev_token_end..lexer.span().start]);
            prev_token_end = lexer.span().end;

            let prev_len = buffer.len();
            self.config.style(token, buffer);

            if prev_len == buffer.len() {
                buffer.push_str(lexer.slice());
                continue;
            }

            let to_insert = match mem::replace(&mut is_span_open, true) {
                true => "</span><span ",
                false => "<span ",
            };
            buffer.insert_str(prev_len, to_insert);
            buffer.push('>');

            buffer.push_str(lexer.slice());
        }

        if is_span_open {
            buffer.push_str("</span>");
        }
        buffer.push_str(&code[prev_token_end..]);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn test_case_config(config: Config<'_>, input: &str, expected: &str) {
        let mut buffer = String::new();
        emit(input, config, &mut buffer);
        assert_eq!(buffer, expected);
    }

    #[track_caller]
    fn test_case(input: &str, expected: &str) {
        test_case_config(Config::empty(), input, expected);
    }

    #[test]
    fn just_text() {
        // paragraph
        test_case("", "");
        test_case("hello world", "<p>hello world</p>");
        test_case("hello\nworld", "<p>hello world</p>");
        test_case("hello\n\nworld", "<p>hello</p><p>world</p>");
        test_case("hello\n\n\nworld", "<p>hello</p><p>world</p>");

        // italic
        test_case("*hello world*", "<p><i>hello world</i></p>");
        test_case("hello *world*", "<p>hello <i>world</i></p>");
        test_case("*hello* world*", "<p><i>hello</i> world<i></p>");
        test_case(
            r"\*hello\* *world* *again*",
            "<p>*hello* <i>world</i> <i>again</i></p>",
        );

        // bold
        test_case("**hello world**", "<p><b>hello world</b></p>");
        test_case("hello **world**", "<p>hello <b>world</b></p>");
        test_case("**hello** world**", "<p><b>hello</b> world<b></p>");
        test_case(
            r"\**hello\** **world** **again**",
            "<p>**hello** <b>world</b> <b>again</b></p>",
        );

        // strikethrough
        test_case("~~hello world~~", "<p><s>hello world</s></p>");
        test_case("hello ~~world~~", "<p>hello <s>world</s></p>");
        test_case("~~hello~~ world~~", "<p><s>hello</s> world<s></p>");
        test_case(
            r"\~~hello\~~ ~~world~~ ~~again~~",
            "<p>~~hello~~ <s>world</s> <s>again</s></p>",
        );

        // code
        test_case("`hello world`", "<p><code>hello world</code></p>");
        test_case("hello `world`", "<p>hello <code>world</code></p>");
        test_case("`hello` world`", "<p><code>hello</code> world<code></p>");
        test_case(
            r"\`hello\` `world` `again`",
            "<p>`hello` <code>world</code> <code>again</code></p>",
        );

        // links
        test_case(
            "[hello world](https://example.com)",
            "<p><a href=\"https://example.com\">hello world</a></p>",
        );
    }

    #[test]
    fn headings() {
        // no edge case
        test_case("# hello world", "<h1>hello world</h1>");
        test_case("## hello world", "<h2>hello world</h2>");
        test_case("### hello world", "<h3>hello world</h3>");
        test_case("#### hello world", "<h4>hello world</h4>");
        test_case("##### hello world", "<h5>hello world</h5>");
        test_case("###### hello world", "<h6>hello world</h6>");

        // hashes in text
        test_case("# hello # world", "<h1>hello # world</h1>");
        test_case("hello \\## world", "<p>hello ## world</p>");
    }

    #[test]
    fn code_blocks() {
        // no highlight
        test_case("```\nhello world\n```", "<pre>hello world</pre>");

        #[derive(logos::Logos, PartialEq, Eq, Clone, Copy)]
        #[logos(skip r"\s+")]
        enum Token {
            #[regex(r"[a-zA-Z]+")]
            Word,
            #[regex(r"[^a-zA-Z\s]+")]
            Other,
        }

        impl ToCodeCategory for Token {
            const NAME: &'static str = "te";
            type Token = Self;
            fn to_code_category(s: Result<Self, ()>) -> CodeCategory {
                match s {
                    Ok(Token::Word) => CodeCategory::Ident,
                    Ok(Token::Other) => CodeCategory::Punctuation,
                    Err(()) => CodeCategory::Error,
                }
            }
        }

        let mut ts = LogosSyntax::new(CodeCategorySyntaxConfig::<Token>::new());

        let config = Config {
            syntaxes: &mut [&mut ts],
            ..Config::empty()
        };

        test_case_config(
            config,
            "```te\nhello\n+\n```",
            "<pre><span class=\"code-ident\">hello\n</span><span class=\"code-punctation\">+</span></pre>",
        );
    }
}

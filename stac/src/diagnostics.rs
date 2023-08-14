use std::{
    fmt::{self, Display, Write},
    iter,
    ops::Range,
};

use crate::{FileRef, Files, Span};

#[derive(Default)]
pub struct Diagnostics {
    counters: [u32; 4],
    output: String,
    temp_buffer: String,
    annotation_temp: Vec<Annotation>,
    footer_temp: Vec<FooterAnnotation>,
    config: DiagnosticConfig,
}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.output)?;
        let [err, warn, ..] = self.counters;
        writeln!(f, "err: {err}, warn: {warn}")
    }
}

impl Diagnostics {
    pub fn with_config(config: DiagnosticConfig) -> Self {
        Self {
            config,
            ..Default::default()
        }
    }

    pub fn view(&self) -> &str {
        &self.output
    }

    fn cache_string(&mut self, message: impl Display) -> Range<usize> {
        let start = self.temp_buffer.len();
        write!(self.temp_buffer, "{}", message).unwrap();
        let end = self.temp_buffer.len();

        start..end
    }

    fn commit_diagnostic(&mut self, files: &Files) {
        for footer in self.footer_temp.drain(..) {
            footer.display_standalone(&mut self.output, &self.temp_buffer, self.config.skip_colors)
        }

        let highest_severty = self.annotation_temp.iter().map(|a| a.severty).max();
        let Some(highest_severty) = highest_severty else {
            return;
        };

        self.counters[highest_severty as usize] += 1;

        self.annotation_temp
            .sort_unstable_by_key(Annotation::get_file);

        struct Padder {
            progress: usize,
            tab_width: usize,
        }

        impl Padder {
            fn new(tab_width: usize) -> Self {
                Self {
                    progress: 0,
                    tab_width,
                }
            }

            fn pad(&mut self, out: Option<&mut String>, c: char) {
                match c {
                    '\t' => {
                        let n = self.tab_width - self.progress % self.tab_width;
                        if let Some(out) = out {
                            out.extend(iter::repeat(' ').take(n));
                        }
                        self.progress += n;
                    }
                    _ => {
                        if let Some(out) = out {
                            out.push(c);
                        }
                        self.progress += 1;
                    }
                }
            }
        }

        fn num_chars(mut n: u16) -> impl Iterator<Item = char> {
            let mut buf = [0; 5];
            let mut i = 0;
            while n > 0 {
                buf[i] = (n % 10) as u8 + b'0';
                n /= 10;
                i += 1;
            }
            buf.into_iter().take(i).rev().map(char::from)
        }

        for group in self
            .annotation_temp
            .group_by_mut(|a, b| a.get_file() == b.get_file())
        {
            group.sort_unstable_by_key(|a| a.span);
            let first = &group[0];

            let file = first.span.file();
            let file = &files[file];
            let lines = file.source().lines().enumerate();
            writeln!(
                self.output,
                "-> {:?}:{}:{}",
                file.name(),
                first.span.row(),
                first.span.col()
            )
            .unwrap();

            let line_number_pad = group
                .last()
                .unwrap()
                .span
                .row()
                .saturating_add(self.config.view_range)
                .ilog10()
                + 1;
            let mut diags = group.iter().peekable();

            let mut last_diag_line = diags.peek().unwrap().span.row() as usize;

            #[derive(Clone, Copy)]
            enum Region {
                Inside,
                Outside,
            }
            let mut region = Region::Outside;
            let view_range = self.config.view_range as usize;

            'b: for (i, line) in lines {
                fn check_in_range(i: usize, last_diag_line: usize, view_range: usize) -> bool {
                    i >= last_diag_line.saturating_sub(view_range)
                        && i <= last_diag_line.saturating_add(view_range)
                }
                let in_range = check_in_range(i, last_diag_line, view_range);

                match (region, in_range) {
                    (Region::Outside, true) => {
                        region = Region::Inside;
                    }
                    (Region::Inside, false) => 'a: {
                        if let Some(annotation) = diags.peek() {
                            last_diag_line = annotation.span.row() as usize;
                            if check_in_range(i, last_diag_line, view_range) {
                                break 'a;
                            }
                        }

                        region = Region::Outside;
                        self.output.push_str("...\n");
                        continue 'b;
                    }
                    (Region::Outside, false) => continue,
                    _ => {}
                }

                num_chars(i as u16 + 1)
                    .chain(iter::repeat(' '))
                    .take(line_number_pad as usize)
                    .chain(['|', ' '])
                    .collect_into(&mut self.output);

                let mut padder = Padder::new(self.config.tab_width);
                for c in line.chars() {
                    padder.pad(Some(&mut self.output), c)
                }
                self.output.push('\n');

                while let Some(annotation) = diags.peek() && annotation.span.row() as usize == i {
                    last_diag_line = annotation.span.row() as usize;
                    let mut padder = Padder::new(self.config.tab_width);
                    line[..annotation.span.col()].chars().for_each(|c| padder.pad(None, c));

                    iter::repeat(' ')
                        .take(padder.progress + line_number_pad as usize + "| ".len())
                        .chain(['^', ' '])
                        .collect_into(&mut self.output);

                    annotation.display_standalone(&mut self.output, &self.temp_buffer, self.config.skip_colors);

                    diags.next();
                }
            }
        }

        self.annotation_temp.clear();
        self.temp_buffer.clear();
        self.output.push('\n');
    }

    pub fn clear(&mut self) {
        self.output.clear();
        self.counters = [0; 4];
    }

    pub fn error_count(&self) -> u32 {
        self.counters[Severty::Error as usize]
    }

    pub fn todo_error(&mut self, files: &Files, span: Span, message: impl Display) -> Option<!> {
        self.builder(files)
            .footer(Severty::Error, message)
            .annotation(Severty::Error, span, "due to this")
            .terminate()
    }
}

impl Diagnostics {
    pub fn builder<'ctx>(&'ctx mut self, files: &'ctx Files) -> Diagnostic<'ctx> {
        Diagnostic { inner: self, files }
    }
}

#[derive(Clone, Copy)]
pub struct DiagnosticConfig {
    pub tab_width: usize,
    pub skip_colors: bool,
    pub view_range: usize,
}

impl Default for DiagnosticConfig {
    fn default() -> Self {
        Self {
            tab_width: 4,
            skip_colors: true,
            view_range: 1,
        }
    }
}

pub struct Diagnostic<'ctx> {
    inner: &'ctx mut Diagnostics,
    files: &'ctx Files,
}

impl<'ctx> Diagnostic<'ctx> {
    pub fn footer(self, severty: Severty, message: impl Display) -> Self {
        let message = self.inner.cache_string(message);
        self.inner
            .footer_temp
            .push(FooterAnnotation { severty, message });
        self
    }

    pub fn annotation(self, severty: Severty, span: Span, message: impl Display) -> Self {
        let message = self.inner.cache_string(message);
        self.inner.annotation_temp.push(Annotation {
            severty,
            span,
            message,
        });
        self
    }

    pub fn terminate(self) -> Option<!> {
        None
    }
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        self.inner.commit_diagnostic(self.files);
    }
}

struct FooterAnnotation {
    severty: Severty,
    message: Range<usize>,
}

impl FooterAnnotation {
    fn display_standalone(&self, out: &mut String, cache: &str, skip_color: bool) {
        out.push_str(self.severty.as_ansi_color(skip_color));
        out.push_str(self.severty.as_str());
        out.push_str(self.severty.reset_ansi_color(skip_color));
        writeln!(out, ": {}", &cache[self.message.clone()]).unwrap();
    }
}

struct Annotation {
    severty: Severty,
    span: Span,
    message: Range<usize>,
}

impl Annotation {
    fn get_file(&self) -> FileRef {
        self.span.file()
    }

    fn display_standalone(&self, out: &mut String, cahce: &str, skip_color: bool) {
        out.push_str(self.severty.as_ansi_color(skip_color));
        writeln!(out, "{}", &cahce[self.message.clone()]).unwrap();
        out.push_str(self.severty.reset_ansi_color(skip_color));
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Severty {
    Error,
    Warning,
    Note,
    Help,
}

impl Severty {
    fn as_str(self) -> &'static str {
        match self {
            Severty::Error => "error",
            Severty::Warning => "warning",
            Severty::Note => "note",
            Severty::Help => "help",
        }
    }

    fn as_ansi_color(self, skip: bool) -> &'static str {
        if skip {
            return "";
        }

        match self {
            Severty::Error => "\x1b[31m",
            Severty::Warning => "\x1b[33m",
            Severty::Note => "\x1b[34m",
            Severty::Help => "\x1b[32m",
        }
    }

    fn reset_ansi_color(self, skip: bool) -> &'static str {
        if skip {
            return "";
        }

        "\x1b[0m"
    }
}

#[cfg(test)]
mod test {
    use crate::{
        print_cases, DiagnosticConfig, Diagnostics, File, Files, Lexer, Severty, TokenKind,
    };

    fn perform_test(_: &str, code: &str, ctx: &mut String) {
        let mut files = Files::new();
        let file = files.push(File::new("main".into(), code.into()));
        let lexer = Lexer::new(&files, file);
        let mut diags = Diagnostics::with_config(DiagnosticConfig {
            skip_colors: true,
            ..Default::default()
        });

        let mut builder = diags.builder(&files).footer(Severty::Error, "test");
        for tok in lexer.take_while(|tok| tok.kind != TokenKind::Eof) {
            builder = builder.annotation(Severty::Warning, tok.span, tok.kind);
        }

        drop(builder);

        ctx.push_str(diags.view());
    }

    print_cases! { perform_test:
        just_do_it "
main: || {
\tx: 1;
}
";

        lot_or_code "
main: || {
\tx: 1;



f: x



};
";
    }
}

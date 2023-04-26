use std::{iter, mem, ops::Range};

use stac::Token;

use super::state::{EditingContext, EditingState, EditorMode};

pub struct DisplayState<'a> {
    pub code: &'a str,
    pub cursor: Option<usize>,
    pub mode: Option<&'a EditorMode>,
}

impl<'a> DisplayState<'a> {
    pub fn from_state(state: &'a EditingState) -> Self {
        Self {
            code: state.code(),
            cursor: Some(state.cursor()),
            mode: state.focused().then(|| state.mode()),
        }
    }

    pub fn from_str(code: &'a str) -> Self {
        Self {
            code,
            cursor: None,
            mode: None,
        }
    }
}

#[derive(Default)]
pub struct CodeDisplayer {
    html: String,
    line_numbers: String,
    resolver: NodeResolver,
}

impl CodeDisplayer {
    const ERROR_CLASS: &'static str = "AnalyzerError";

    pub fn display(&mut self, state: DisplayState, ctx: Option<EditingContext>) {
        self.html.clear();
        self.line_numbers.clear();

        let code = state.code;
        let errors = ctx.map_or(&[][..], |ctx| &ctx.errors[..]);

        let mut lexer = stac::Token::lexer(code)
            .spanned()
            .map(|(tok, span)| (tok.map_or(Token::ERROR, |t| t.name()), span))
            .peekable();

        let highlights = iter::from_fn(|| {
            let (col, mut span) = lexer.next()?;
            while let Some((other_col, next_span)) = lexer.peek() && &col == other_col {
                span.end = next_span.end;
                lexer.next();
            }

            Some(StyleNode {
                start: span.start,
                end: span.end,
                style: Style {
                    foreground: col,
                    ..Default::default()
                },
            })
        });

        self.resolver.add_nodes(highlights);

        let error_iter = errors
            .iter()
            .filter(|e| e.source.is_none())
            .map(|e| StyleNode {
                start: e.span.start,
                end: e.span.end,
                style: Style {
                    background: Self::ERROR_CLASS,
                    ..Default::default()
                },
            });

        self.resolver.add_nodes(error_iter);

        if let Some(cursor) = state.cursor {
            self.resolver.add_nodes(iter::once(StyleNode {
                start: cursor,
                end: cursor + 1,
                style: Style {
                    cursor: true,
                    ..Default::default()
                },
            }));
        }

        self.resolver.resolve();

        self.resolver.create_error_index(code, errors);

        self.display_line_numbers();
        self.resolver
            .display(code, &mut self.html, state.mode, errors);
    }

    fn display_line_numbers(&mut self) {
        self.resolver.display_lines(&mut self.line_numbers);
    }

    pub fn as_str(&self) -> &str {
        self.html.as_str()
    }

    pub fn line_number_str(&self) -> &str {
        self.line_numbers.as_str()
    }

    pub(crate) fn into_string(self) -> String {
        self.html
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
struct Style {
    foreground: &'static str,
    background: &'static str,
    cursor: bool,
}

impl Style {
    fn combine(self, other: Self) -> Self {
        fn choose<'a>(a: &'a str, b: &'a str) -> &'a str {
            if a.is_empty() {
                b
            } else {
                a
            }
        }

        Self {
            foreground: choose(other.foreground, self.foreground),
            background: choose(other.background, self.background),
            cursor: other.cursor || self.cursor,
        }
    }

    fn display(
        &self,
        text: &str,
        range: Range<usize>,
        html: &mut String,
        mode: Option<&EditorMode>,
    ) {
        if self.cursor {
            let Some(mode) = mode else {
                html.push_str(text.get(range).unwrap_or(""));
                return;
            };

            html.push_str(mode.cursor());
            match text.get(range) {
                Some("\n") | None => {
                    html.push(' ');
                    html.push_str("</span>");
                    html.push('\n');
                }
                Some(c) => {
                    html.push_str(c);
                    html.push_str("</span>");
                }
            }
            return;
        }

        let Some(text) = text.get(range) else {
            return;
        };

        html.push_str("<span class=\"");
        html.push_str(self.foreground);
        html.push_str(" ");
        html.push_str(self.background);
        html.push_str("\">");
        html.push_str(text);
        html.push_str("</span>");
    }
}

struct StyleNode {
    start: usize,
    end: usize,
    style: Style,
}

#[derive(Debug, PartialEq, Eq)]
struct StyleCommand {
    pos: usize,
    style: Style,
}

#[derive(Clone, Copy, Debug)]
struct ErrorIndexPoint {
    pos: usize,
    error_index: usize,
}

#[derive(Default)]
struct NodeResolver {
    nodes: Vec<StyleNode>,
    resulting_nodes: Vec<StyleCommand>,

    error_index: Vec<ErrorIndexPoint>,
    line_ranges: Vec<usize>,
}

impl NodeResolver {
    fn add_nodes(&mut self, nodes: impl IntoIterator<Item = StyleNode>) {
        self.nodes.extend(nodes);
    }

    fn resolve(&mut self) {
        self.resulting_nodes.clear();

        self.nodes.sort_by_key(|n| n.start);

        let mut previous_node_insert_index = 0;
        for node in self.nodes.drain(..) {
            let search_from = previous_node_insert_index;

            let start_result =
                self.resulting_nodes[search_from..].binary_search_by_key(&node.start, |n| n.pos);
            let start = start_result.unwrap_or_else(|e| e) + search_from;

            let end_result =
                self.resulting_nodes[start..].binary_search_by_key(&node.end, |n| n.pos);
            let end = end_result.unwrap_or_else(|e| e) + start;

            let revrite_index = end.checked_sub(1).filter(|i| i >= &start);

            for i in self.resulting_nodes[start..revrite_index.unwrap_or(start)].iter_mut() {
                i.style = i.style.combine(node.style);
            }

            let last_style = if let Some(i) = revrite_index {
                mem::replace(&mut self.resulting_nodes[i].style, node.style)
            } else if let Some(i) = end.checked_sub(1) {
                // fully withing a range
                self.resulting_nodes[i].style
            } else {
                Default::default()
            };

            if end_result.is_err() {
                self.resulting_nodes.insert(
                    end,
                    StyleCommand {
                        pos: node.end,
                        style: last_style,
                    },
                );
            }

            if start_result.is_err() {
                self.resulting_nodes.insert(
                    start,
                    StyleCommand {
                        pos: node.start,
                        style: node.style,
                    },
                );
            }

            previous_node_insert_index = start;
        }

        self.resulting_nodes.dedup_by_key(|n| n.style);
        self.nodes.clear();
    }

    fn display(
        &self,
        code: &str,
        html: &mut String,
        mode: Option<&EditorMode>,
        errors: &[stac::Error],
    ) {
        let Some(first) = self.resulting_nodes.first() else {
            html.push_str(code);
            return;
        };
        html.push_str(&code[..first.pos]);

        let mut line = code[..first.pos].matches('\n').count();
        for [start, end] in self.resulting_nodes.array_windows() {
            if let Some(nline_index) = code.get(start.pos..end.pos).and_then(|s| s.find('\n'))
                && let error_indices @ [_, ..] = Self::line_errors(&mut line, &self.error_index, &self.line_ranges) {
                start.style.display(&code, start.pos..start.pos + nline_index + 1, html, mode);

                Self::display_error_box(html, errors,  error_indices);

                start.style.display(&code, start.pos + nline_index + 1..end.pos, html, mode);

                continue;
            }

            start.style.display(&code, start.pos..end.pos, html, mode);
        }

        let last = self.resulting_nodes.last().unwrap();
        last.style.display(&code, last.pos..code.len(), html, mode);
        for _ in code
            .get(last.pos..)
            .unwrap_or("")
            .matches('\n')
            .chain(iter::once(""))
        {
            if let error_indices @ [_, ..] =
                Self::line_errors(&mut line, &self.error_index, &self.line_ranges)
            {
                Self::display_error_box(html, errors, error_indices);
            }
        }
    }

    fn display_error_box(
        html: &mut String,
        errors: &[stac::Error],
        error_indices: &[ErrorIndexPoint],
    ) {
        html.push_str("<pre class=\"editor-error-box\">");

        for error in error_indices.iter().map(|e| &errors[e.error_index]) {
            html.push('\t');
            html.push_str(&error.message);
            html.push('\n');
        }

        html.push_str("</pre>");
    }

    fn line_errors<'a>(
        line: &mut usize,
        index: &'a [ErrorIndexPoint],
        line_ranges: &[usize],
    ) -> &'a [ErrorIndexPoint] {
        let Some(start) = line_ranges.get(*line) else {
            return &[];
        };
        *line += 1;
        let Some(end) = line_ranges.get(*line) else {
            return &[];
        };

        let start = index
            .binary_search_by_key(&(start + 1), |p| p.pos)
            .unwrap_or_else(|e| e);
        let end = index
            .binary_search_by_key(&(end + 1), |p| p.pos)
            .unwrap_or_else(|e| e);

        &index[start..end]
    }

    fn create_error_index(&mut self, code: &str, errors: &[stac::Error]) {
        self.error_index.clear();
        for (error_index, error) in errors.iter().enumerate() {
            let pos = error.span.end;
            self.error_index.push(ErrorIndexPoint { pos, error_index });
        }
        self.error_index.sort_by_key(|p| p.pos);

        self.line_ranges.clear();
        self.line_ranges.push(0);
        code.match_indices('\n')
            .map(|(i, ..)| i)
            .collect_into(&mut self.line_ranges);
        self.line_ranges.push(code.len());
    }

    fn display_lines(&self, html: &mut String) {
        use std::fmt::Write;
        for (i, [start, end]) in self.line_ranges.array_windows().enumerate() {
            write!(html, "{}", i + 1).unwrap();

            let start = self
                .error_index
                .binary_search_by_key(&(start + 1), |p| p.pos)
                .unwrap_or_else(|e| e);
            let end = self
                .error_index
                .binary_search_by_key(&(end + 1), |p| p.pos)
                .unwrap_or_else(|e| e);

            for _ in start..=end {
                html.push('\n');
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn style_node(start: usize, end: usize, foreground: &'static str) -> StyleNode {
        StyleNode {
            start,
            end,
            style: Style {
                foreground,
                ..Default::default()
            },
        }
    }

    fn syle_command(pos: usize, foreground: &'static str) -> StyleCommand {
        StyleCommand {
            pos,
            style: Style {
                foreground,
                ..Default::default()
            },
        }
    }

    fn perform_test(
        ranges: impl IntoIterator<Item = StyleNode>,
        expected: impl IntoIterator<Item = StyleCommand>,
    ) {
        let mut resolver = NodeResolver::default();
        resolver.add_nodes(ranges);
        resolver.resolve();

        assert_eq!(
            resolver.resulting_nodes,
            expected.into_iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_disjoint() {
        perform_test(
            [style_node(0, 1, "a")],
            [syle_command(0, "a"), syle_command(1, "")],
        );

        perform_test(
            [style_node(0, 1, "a"), style_node(2, 3, "b")],
            [
                syle_command(0, "a"),
                syle_command(1, ""),
                syle_command(2, "b"),
                syle_command(3, ""),
            ],
        );

        perform_test(
            [style_node(0, 2, "a"), style_node(2, 3, "b")],
            [
                syle_command(0, "a"),
                syle_command(2, "b"),
                syle_command(3, ""),
            ],
        );
    }

    #[test]
    fn test_overlaps() {
        perform_test(
            [style_node(0, 2, "a"), style_node(1, 3, "b")],
            [
                syle_command(0, "a"),
                syle_command(1, "b"),
                syle_command(3, ""),
            ],
        );

        perform_test(
            [style_node(0, 2, "a"), style_node(1, 2, "b")],
            [
                syle_command(0, "a"),
                syle_command(1, "b"),
                syle_command(2, ""),
            ],
        );

        perform_test(
            [
                style_node(0, 2, "a"),
                style_node(2, 4, "c"),
                style_node(1, 3, "b"),
            ],
            [
                syle_command(0, "a"),
                syle_command(1, "b"),
                syle_command(2, "c"),
                syle_command(4, ""),
            ],
        );

        perform_test(
            [style_node(0, 4, "a"), style_node(1, 3, "b")],
            [
                syle_command(0, "a"),
                syle_command(1, "b"),
                syle_command(3, "a"),
                syle_command(4, ""),
            ],
        );
    }
}

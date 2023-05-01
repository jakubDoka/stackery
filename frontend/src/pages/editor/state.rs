use std::{cell::RefCell, mem, rc::Rc};

use dioxus::{
    html::input_data::keyboard_types::{self, Code, Key, Modifiers},
    prelude::*,
};

use super::analyzer::Analyzer;

#[derive(Default, Clone, Copy, PartialEq, Eq)]
struct Cursor {
    index: usize,
}

impl Cursor {
    fn chnage(&mut self, code: &str, dx: isize, dy: isize) -> bool {
        let norm_dx = dx.abs() as usize;
        let norm_dy = dy.abs() as usize;

        if dx < 0 {
            let left_index = code[..self.index]
                .char_indices()
                .rev()
                .nth(norm_dx - 1)
                .map_or(self.index, |(i, _)| i);
            self.index = left_index;

            return true;
        }

        if dx > 0 {
            let right_index = code[self.index..]
                .char_indices()
                .nth(norm_dx)
                .map_or(code.len(), |(i, _)| self.index + i);
            self.index = right_index;

            return true;
        }

        let column = || {
            code[..self.index]
                .chars()
                .rev()
                .take_while(|c| *c != '\n')
                .count()
        };

        fn project_column(code: &str, line_start: usize, column: usize) -> usize {
            code[line_start..]
                .char_indices()
                .take_while(|(_, c)| *c != '\n')
                .take(column)
                .last()
                .map_or(0, |(i, c)| i + c.len_utf8())
        }

        if dy < 0 {
            let up_line_start = code[..self.index]
                .rmatch_indices('\n')
                .nth(norm_dy)
                .map_or(0, |(i, _)| i + 1);
            let up_index = project_column(code, up_line_start, column());
            self.index = up_line_start + up_index;

            return true;
        }

        if dy > 0 {
            let down_line_start = code[self.index..]
                .match_indices('\n')
                .nth(norm_dy - 1)
                .map_or(code.len(), |(i, _)| self.index + i + 1);
            let down_index = project_column(code, down_line_start, column());
            self.index = down_line_start + down_index;

            return true;
        }

        false
    }

    fn insert(&mut self, code: &mut String, str: &str, before: bool) {
        code.insert_str(self.index, str);
        self.index += str.len() * before as usize;
    }

    fn delete(&mut self, code: &mut String, amount: isize) {
        let normalized_amount = amount.abs() as usize;
        self.index = if amount < 0 {
            let start = self.index.saturating_sub(normalized_amount);
            code.drain(start..self.index);
            start
        } else {
            let end = (self.index + normalized_amount).min(code.len());
            code.drain(self.index..end);
            self.index
        };
    }

    fn find(&mut self, code: &str, pattern: &str, forward: bool) {
        self.index = if forward {
            // we skip first character as that kind of search is useles
            let mut chars = code[self.index..].chars();
            let char_len = chars.next().map_or(0, |c| c.len_utf8());
            chars.as_str().find(pattern).unwrap_or(0) + self.index + char_len
        } else {
            code[..self.index].rfind(pattern).unwrap_or(self.index)
        };
    }
}

#[derive(Clone, Copy)]
pub struct EditingContext<'a> {
    pub status: &'a UseState<String>,
    pub errors: &'a UseState<Vec<stac::Error>>,
    pub cx: &'a ScopeState,
}

#[derive(Default)]
pub struct EditingState {
    cursor: Cursor,
    mode: EditorMode,
    focused: bool,
    code: String,
    analyser: Rc<RefCell<Analyzer>>,
}

impl EditingState {
    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn mode(&self) -> &EditorMode {
        &self.mode
    }

    pub fn focused(&self) -> bool {
        self.focused && !matches!(self.mode, EditorMode::Command { .. })
    }

    pub fn update(&mut self, event: Event<KeyboardData>, ctx: EditingContext<'_>) -> bool {
        let mode = mem::take(&mut self.mode);
        let (changed, mode) = match mode {
            EditorMode::Normal => self.update_normal(event),
            EditorMode::Insert => self.update_insert(event),
            EditorMode::Find { forward } => self.update_find(event, forward),
            EditorMode::Command { text } => self.update_command(event, text, ctx),
        };

        self.mode = mode;
        changed
    }

    fn update_command(
        &mut self,
        event: Event<KeyboardData>,
        mut text: String,
        ctx: EditingContext<'_>,
    ) -> (bool, EditorMode) {
        match event.data.code() {
            Code::Enter => {
                self.handle_command(text, ctx);
                (true, EditorMode::Normal)
            }
            Code::Escape => (true, EditorMode::Normal),
            Code::Backspace => {
                text.pop();
                (true, EditorMode::Command { text })
            }
            _ => match event.data.key() {
                keyboard_types::Key::Character(c) => {
                    text.push_str(&c);
                    (true, EditorMode::Command { text })
                }
                _ => (true, EditorMode::Normal),
            },
        }
    }

    fn handle_command(
        &mut self,
        text: String,
        EditingContext { status, errors, cx }: EditingContext<'_>,
    ) {
        let mut parts = text.split_whitespace();
        match parts.next() {
            Some("w") => status.set("TODO: saving editor state to database".into()),
            Some("r") => status.set("TODO: run the code".into()),
            Some("c") => {
                let analyzer = self.analyser.clone();
                let code = self.code.clone();
                let status = status.clone();
                let errors = errors.clone();
                cx.push_future(async move {
                    let Ok(mut a) = analyzer.try_borrow_mut() else {
                        status.set("analyzer is already in use".into());
                        return;
                    };

                    a.check(code, status).await;
                    errors.set(a.errors().collect());
                });
            }
            _ => status.set("unknown command".into()),
        }
    }

    fn update_normal(&mut self, event: Event<KeyboardData>) -> (bool, EditorMode) {
        let times = 1;
        match event.data.code() {
            Code::KeyI => (true, EditorMode::Insert),
            Code::KeyA => self.change_cursor(1, 0, EditorMode::Insert),
            Code::Semicolon if event.data.modifiers().contains(Modifiers::SHIFT) => (
                true,
                EditorMode::Command {
                    text: String::new(),
                },
            ),
            Code::KeyF => (
                true,
                EditorMode::Find {
                    forward: !event.data.modifiers().contains(Modifiers::SHIFT),
                },
            ),
            Code::KeyH => self.change_cursor(-times, 0, EditorMode::Normal),
            Code::KeyL => self.change_cursor(times, 0, EditorMode::Normal),
            Code::KeyJ => self.change_cursor(0, times, EditorMode::Normal),
            Code::KeyK => self.change_cursor(0, -times, EditorMode::Normal),
            _ => (false, EditorMode::Normal),
        }
    }

    fn change_cursor(&mut self, dx: isize, dy: isize, mode: EditorMode) -> (bool, EditorMode) {
        (self.cursor.chnage(&self.code, dx, dy), mode)
    }

    fn update_insert(&mut self, event: Event<KeyboardData>) -> (bool, EditorMode) {
        match event.data.key() {
            Key::Character(ref char) => self.cursor.insert(&mut self.code, char, true),
            Key::Escape => return (true, EditorMode::Normal),
            Key::Backspace => self.cursor.delete(&mut self.code, -1),
            Key::Enter => self.cursor.insert(&mut self.code, "\n", true),
            Key::Tab => self.cursor.insert(&mut self.code, "\t", true),
            _ => return (false, EditorMode::Insert),
        }

        (true, EditorMode::Insert)
    }

    fn update_find(&mut self, event: Event<KeyboardData>, forward: bool) -> (bool, EditorMode) {
        match event.data.key() {
            Key::Character(ref char) => self.cursor.find(&self.code, char, forward),
            _ => {}
        }
        (true, EditorMode::Normal)
    }

    pub(super) fn cursor(&self) -> usize {
        self.cursor.index
    }

    pub(super) fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
    }
}

#[derive(Clone, PartialEq, Eq, Default)]
pub enum EditorMode {
    #[default]
    Normal,
    Insert,
    Find {
        forward: bool,
    },
    Command {
        text: String,
    },
}

impl std::fmt::Display for EditorMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "nor"),
            Self::Insert => write!(f, "ins"),
            Self::Find { forward: true } => write!(f, "fnd"),
            Self::Find { forward: false } => write!(f, "rfd"),
            Self::Command { text } => {
                write!(
                    f,
                    ":{}{} <span/>",
                    html_escape::encode_text_minimal(text),
                    self.cursor()
                )
            }
        }
    }
}

impl EditorMode {
    pub fn cursor(&self) -> &'static str {
        match self {
            EditorMode::Normal => "<span class=\"cursor-nor\">",
            EditorMode::Insert | Self::Command { .. } => "<span class=\"cursor-ins\">",
            EditorMode::Find { .. } => "<span class=\"cursor-fnd\">",
        }
    }
}

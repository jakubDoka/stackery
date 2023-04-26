use crate::use_theme;

use self::state::{EditingContext, EditingState};
use super::InputData;
use dioxus::prelude::*;
use std::{cell::RefCell, ops::Deref};

mod analyzer;
mod display;
mod state;

pub use display::{CodeDisplayer, DisplayState};

#[derive(Default, serde::Serialize, Clone)]
#[serde(transparent)]
pub struct Code(pub String);

impl InputData for Code {
    type Error = bf_shared::db::post::CodeError;

    const TYPE: &'static str = "text";

    fn parse(value: &str) -> Result<Self, Self::Error> {
        bf_shared::db::post::validate_code(value).map(|_| Self(value.to_string()))
    }

    fn render<'a>(cx: &'a ScopeState, name: &'a str) -> Element<'a> {
        let ctx = EditingContext {
            status: use_state(cx, || String::new()),
            errors: use_state(cx, || Vec::new()),
            cx,
        };
        let editor = &*cx.use_hook(|| RefCell::new(Editor::default()));
        use_memo(cx, ctx.errors, |_| editor.borrow_mut().redraw(ctx));
        let theme = use_theme(cx);

        cx.render(rsx! { div {
            class: "editor",
            "theme": "{theme}",
            onfocus: move |_| editor.borrow_mut().set_focused(true, ctx),
            onfocusout: move |_| editor.borrow_mut().set_focused(false, ctx),
            onkeydown: move |e| editor.borrow_mut().oninput(e, ctx),
            prevent_default: "onkeydown",
            tabindex: 0,

            input {
                r#type: Self::TYPE,
                name: name,
                hidden: true,
                value: "{editor.borrow().state.code()}"
            }
            div {
                class: "editor-content",
                pre {
                    class: "editor-content-numbers",
                    dangerous_inner_html: "{editor.borrow().html.line_number_str()}",
                }
                pre {
                    class: "editor-content-code",
                    dangerous_inner_html: "{editor.borrow().html.as_str()}",
                }
            }
            pre { class: "editor-mode", dangerous_inner_html: "{editor.borrow().state.mode()}" }
            pre { class: "editor-status", "{ctx.status.get()}" }
        } })
    }
}

impl Deref for Code {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

#[derive(Default)]
struct Editor {
    html: CodeDisplayer,
    state: EditingState,
}

impl Editor {
    fn set_focused(&mut self, focused: bool, ctx: EditingContext) {
        self.state.set_focused(focused);
        self.redraw(ctx);
    }

    fn oninput(&mut self, event: Event<KeyboardData>, ctx: EditingContext) {
        event.stop_propagation();

        let changed = self.state.update(event, ctx);

        if changed {
            self.redraw(ctx);
        }
    }

    fn redraw(&mut self, ctx: EditingContext) {
        self.html
            .display(DisplayState::from_state(&self.state), Some(ctx));
        ctx.cx.needs_update();
    }
}

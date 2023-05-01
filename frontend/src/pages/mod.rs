use std::{collections::HashMap, fmt::Display, future::Future, marker::PhantomData, ops::Deref};

use dioxus::{html::input_data::keyboard_types, prelude::Scope};

mod about;
mod editor;
mod home;
mod login;
mod post;
mod register;
mod search;

pub use {
    about::About, editor::Code, home::Home, login::Login, post::Post, register::Register,
    search::Search,
};

use dioxus::prelude::*;

use crate::navbar::use_app_state;

use self::editor::{CodeDisplayer, DisplayState};

pub trait SearchResult {
    fn key(&self) -> &str;
    fn render<'a>(&'a self, cx: &'a ScopeState) -> Element<'a> {
        cx.render(rsx! { self.key() })
    }
}

impl SearchResult for String {
    fn key(&self) -> &str {
        self
    }
}

impl SearchResult for bf_shared::search::post::Model {
    fn key(&self) -> &str {
        &self.name
    }

    fn render<'a>(&'a self, cx: &'a ScopeState) -> Element<'a> {
        let displayer = {
            let mut d = CodeDisplayer::default();
            let state = DisplayState::from_str(&self.code);
            d.display(state, None);
            d.into_string()
        };
        let app_state = use_app_state(cx);

        cx.render(rsx! { div {
            class: "post-search-result",
            div {
                class: "post-name",
                "{self.name}"
            }
            div {
                class: "post-author",
                "by: {self.author}"
            }
            pre {
                class: "post-code editor",
                "theme": "{app_state.theme()}",
                dangerous_inner_html: "{displayer}"
            }
        } })
    }
}

#[inline_props]
fn SearchBar<'a, F, R, E, D>(cx: Scope, desc: &'a str, fetcher: F) -> Element
where
    F: Fn(String) -> R + 'static + Clone,
    R: Future<Output = Result<Vec<D>, E>> + 'static,
    E: ToString + 'static,
    D: 'static + SearchResult,
{
    let results = use_state::<Vec<D>>(cx, || vec![]);
    let selection = use_state::<usize>(cx, || 0);
    let query = use_state(cx, || String::new());
    let error = use_state(cx, || None::<E>);

    #[inline_props]
    fn SearchResult<'a, T: SearchResult + 'static>(cx: Scope, data: &'a T) -> Element {
        let element = data.render(cx);
        cx.render(rsx! { element })
    }

    cx.render(rsx! {
        div {
            class: "form",
            input {
                class: "form-field",
                r#type: "text",
                placeholder: *desc,
                value: query.get().as_str(),
                oninput: move |e| {
                    let value = e.value.clone();
                    let fetcher = fetcher.clone()(value.clone());
                    let results = results.clone();
                    let error = error.clone();
                    query.set(value);
                    async move {
                        match fetcher.await {
                            Ok(res) => results.set(res),
                            Err(e) => error.set(Some(e)),
                        };
                    }
                },
                onkeydown: move |e| {
                    let len = results.get().len();

                    if len == 0 {
                        return;
                    }

                    let &current = selection.get();

                    let new_selection = match e.code() {
                        keyboard_types::Code::ArrowUp => current.wrapping_sub(1),
                        keyboard_types::Code::ArrowDown => current.wrapping_add(1),
                        keyboard_types::Code::Enter => {
                            if let Some(res) = results.get().get(current) {
                                query.set(res.key().to_string());
                            }
                            current
                        }
                        _ => return,
                    };

                    selection.set(new_selection % len);
                    e.stop_propagation();
                }
            }
            div {
                class: "form-error",
                hidden: error.get().is_none(),
                error.get().as_ref().map(|e| e.to_string()).unwrap_or_default()
            }
            div {
                class: "search-results",
                for (i, res) in results.iter().enumerate() {
                    div {
                        class: if i == *selection.get() {
                            "search-result search-result-selected"
                        } else {
                            "search-result"
                        },
                        SearchResult { data: res }
                    }
                }
            }
        }
    })
}

pub trait FormModel: Default {
    fn render(cx: &ScopeState) -> Element;
    fn parse(values: &HashMap<String, String>, errors: &mut Vec<String>) -> Option<Self>;
}

pub trait InputData: Sized {
    type Error: Display;
    const TYPE: &'static str;
    fn parse(value: &str) -> Result<Self, Self::Error>;

    fn render<'a>(cx: &'a ScopeState, name: &'a str) -> Element<'a> {
        cx.render(rsx! {
            input {
                class: "form-field",
                r#type: Self::TYPE,
                name: name,
                placeholder: name,
            }
        })
    }
}

#[derive(Default, serde::Serialize, Clone)]
#[serde(transparent)]
pub struct Password(pub String);

impl InputData for Password {
    type Error = std::convert::Infallible;

    const TYPE: &'static str = "password";

    fn parse(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.to_string()))
    }
}

impl Deref for Password {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

#[derive(Default, serde::Serialize, Clone)]
#[serde(transparent)]
pub struct Name(pub String);

impl InputData for Name {
    type Error = bf_shared::db::NameError;

    const TYPE: &'static str = "text";

    fn parse(value: &str) -> Result<Self, Self::Error> {
        bf_shared::db::validate_name(value).map(|_| Self(value.to_string()))
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

#[macro_export]
macro_rules! form_model {
    ($name:ident {$(
        $field:ident: $type:ty
    ),* $(,)?}) => {
        #[derive(Default, serde::Serialize)]
        pub struct $name {
            $(
                pub $field: $type,
            )*
        }

        impl $crate::pages::FormModel for $name {
            fn render(cx: &dioxus::prelude::ScopeState) -> dioxus::prelude::Element {
                use dioxus::prelude::*;
                cx.render(rsx! {
                    $(
                        <$type as $crate::pages::InputData>::render(cx, stringify!($field)),
                    )*
                })
            }

            fn parse(values: &std::collections::HashMap<String, String>, errors: &mut Vec<String>) -> Option<Self> {
                errors.clear();
                $(
                    let $field = values.get(stringify!($field)).filter(|i| !i.is_empty()).ok_or_else(|| {
                        format!("missing field: {}", stringify!($field))
                    }).and_then(|value| {
                        <$type as $crate::pages::InputData>::parse(value).map_err(|e| {
                            format!("invalid field: {}: {}", stringify!($field), e)
                        })
                    }).map_err(|e| {
                        errors.push(e);
                    });
                )*

                Some(Self {
                    $(
                        $field: $field.ok()?,
                    )*
                })
            }
        }
    };
}

#[inline_props]
pub fn Form<'a, S, T, E, M, F>(
    cx: Scope,
    fields: PhantomData<M>,
    submit: S,
    children: Element<'a>,
) -> Element
where
    M: FormModel + 'static,
    S: FnOnce(M) -> F + Clone,
    F: Future<Output = Result<T, E>> + 'static,
    T: Display + 'static,
    E: Display + 'static,
{
    enum State<T, E> {
        Empty,
        Loading,
        Error(E),
        Success(T),
    }

    impl<T, E> Display for State<T, E>
    where
        T: Display,
        E: Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                State::Empty => write!(f, ""),
                State::Loading => write!(f, "loading..."),
                State::Error(err) => write!(f, "error: {}", err),
                State::Success(suc) => write!(f, "success: {}", suc),
            }
        }
    }

    let _ = fields;

    let state = use_state(cx, || State::Empty);
    let field_issues = use_state::<Vec<String>>(cx, || vec![]);

    cx.render(rsx! {
        form {
            class: "form",

            onsubmit: move |ev| {
                let mut new_issues = vec![];
                let data = M::parse(&ev.values, &mut new_issues);
                field_issues.set(new_issues);
                let state = state.clone();
                let fut = data.map(|data| submit.clone()(data));
                async move {
                    let Some(future) = fut else {return};
                    state.set(State::Loading);
                    state.set(match future.await {
                        Ok(uuid) => State::Success(uuid),
                        Err(err) => State::Error(err),
                    });
                }
            },
            prevent_default: "onsubmit",

            div { class: "form-fields", M::render(cx) }
            input { r#type: "submit", value: "POST", class: "form-submit" }
            div { class: "form-state", "{state.get()}" }
            div {
                class: "form-issues",
                hidden: field_issues.get().is_empty(),
                for issue in field_issues.get().iter() {
                    div { class: "form-error", issue.as_str() }
                }
            }
            div { class: "form-children", children }
        }
    })
}

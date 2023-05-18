use std::{collections::HashMap, fmt::Display, future::Future, marker::PhantomData, ops::Deref};

use dioxus::{html::input_data::keyboard_types, prelude::Scope};

mod about;
mod editor;
mod home;
mod login;
mod post;
mod profile;
mod register;
mod search;

pub use {editor::CodeEditor, login::Login, post::Post, register::Register, search::Search};

use dioxus::prelude::*;
use little_md::{CodeCategory, CodeCategorySyntaxConfig, LogosSyntax, ToCodeCategory};

use crate::navbar::use_app_state;

use self::editor::{CodeDisplayer, DisplayState};

#[inline_props]
fn Code<'a>(cx: Scope<'a>, content: &'a str, classes: Option<&'a str>) -> Element<'a> {
    let displayer = {
        let mut d = CodeDisplayer::default();
        let state = DisplayState::from_str(content);
        d.display(state, None);
        d.into_string()
    };
    let app_state = use_app_state(cx);

    cx.render(rsx! { pre {
       class: format_args!("{} code-theme md-code-block", classes.unwrap_or("")),
       "theme": "{app_state.theme()}",
       dangerous_inner_html: "{displayer}"
    } })
}

pub trait SearchResult {
    fn key(&self) -> &str;
    fn render<'a>(&'a self, cx: &'a ScopeState, _: &'a UseState<String>) -> Element<'a> {
        cx.render(rsx! { self.key() })
    }
}

impl SearchResult for String {
    fn key(&self) -> &str {
        self
    }
}

pub trait SearchResultView<D: SearchResult> {
    const ACTIVE: bool = true;
    fn render<'a>(&'a self, cx: &'a ScopeState, result: Option<&D>) -> Element<'a>;
}

impl<D: SearchResult> SearchResultView<D> for () {
    const ACTIVE: bool = false;
    fn render<'a>(&'a self, cx: &'a ScopeState, _: Option<&D>) -> Element<'a> {
        cx.render(rsx!(()))
    }
}

#[inline_props]
fn SearchBar<'a, F, R, E, D, V>(cx: Scope, desc: &'a str, fetcher: F, view: V) -> Element
where
    F: Fn(String) -> R + 'static + Clone,
    R: Future<Output = Result<Vec<D>, E>> + 'static,
    E: ToString + 'static,
    D: 'static + SearchResult,
    V: SearchResultView<D> + 'a,
{
    let selection = use_state::<usize>(cx, || 0);
    let query = use_state(cx, || String::new());
    let error = use_state(cx, || None::<E>);
    let searching = use_state(cx, || true);
    let results = use_state(cx, || Vec::new());

    let fut_error = error.clone();
    let fut_results = results.clone();
    use_future!(cx, |query, searching| {
        let fetcher = fetcher.clone();
        async move {
            if query.is_empty() {
                return;
            }

            if !*searching.get() {
                return;
            }

            match fetcher(query.get().clone()).await {
                Ok(res) => {
                    fut_error.set(None);
                    fut_results.set(res);
                }
                Err(e) => {
                    fut_error.set(Some(e));
                }
            };
        }
    });

    #[inline_props]
    fn SearchResult<'a, T: SearchResult + 'static>(
        cx: Scope<'a>,
        data: &'a T,
        query: &'a UseState<String>,
    ) -> Element<'a> {
        let element = data.render(cx, query);
        cx.render(rsx! { element })
    }

    cx.render(rsx! {
        div {
            class: "form",
            input {
                class: "form-field",
                r#type: "text",
                placeholder: *desc,
                value: query.as_str(),
                oninput: move |e| {
                    let value = e.value.clone();
                    query.set(value);
                    searching.set(true);
                },
                onkeydown: move |e| {
                    if !*searching.get() {
                        return;
                    }

                    let len = results.len();
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
                            searching.set(false);
                            current
                        }
                        _ => return,
                    };

                    selection.set(new_selection % len);
                    e.stop_propagation();
                },
                onfocus: move |_| {
                    searching.set(true);
                },
            }
            div {
                class: "form-error",
                hidden: error.get().is_none(),
                error.get().as_ref().map(|e| e.to_string()).unwrap_or_default()
            }
            div {
                class: "search-results",
                hidden: !searching.get() && V::ACTIVE,
                for (i, res) in results.iter().enumerate() {
                    div {
                        class: if i == *selection.get() {
                            "search-result search-result-selected"
                        } else {
                            "search-result"
                        },
                        onmouseenter: move |_| selection.set(i),
                        onclick: move |_| {
                            query.set(res.key().to_string());
                            searching.set(false);
                        },
                        SearchResult { data: res, query: query }
                    }
                }
            }
            div {
                class: "search-result-view",
                hidden: **searching || !V::ACTIVE,
                view.render(cx, results.get().get(*selection.get()).filter(|_| !searching))
            }
        }
    })
}

pub trait FormModel: Default {
    fn render(cx: &ScopeState) -> Element;
    fn parse(values: &HashMap<String, Vec<String>>, errors: &mut Vec<String>) -> Option<Self>;
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

            fn parse(values: &std::collections::HashMap<String, Vec<String>>, errors: &mut Vec<String>) -> Option<Self> {
                errors.clear();
                $(
                    let $field = values.get(stringify!($field)).filter(|i| !i.is_empty()).ok_or_else(|| {
                        format!("missing field: {}", stringify!($field))
                    }).and_then(|value| {
                        <$type as $crate::pages::InputData>::parse(value.first().map_or("", String::as_str))
                            .map_err(|e| {
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

struct StacSyntaxConfig;

impl ToCodeCategory for StacSyntaxConfig {
    const NAME: &'static str = "stac";

    type Token = stac::Token;

    fn to_code_category(s: Result<Self::Token, ()>) -> CodeCategory {
        let Ok(s) = s else {return CodeCategory::Error;};
        match s {
            stac::Token::ListSep
            | stac::Token::ListEnd
            | stac::Token::Bind
            | stac::Token::Op(_) => CodeCategory::Punctuation,
            stac::Token::Import | stac::Token::Func | stac::Token::Call => CodeCategory::Keyword,
            stac::Token::Str => CodeCategory::String,
            stac::Token::Int => CodeCategory::Number,
            stac::Token::Comment => CodeCategory::Comment,
            stac::Token::Eof => CodeCategory::Error,
            stac::Token::Use => CodeCategory::Ident,
        }
    }
}

fn grammar_syntax() -> CodeCategorySyntaxConfig<impl ToCodeCategory> {
    #[derive(logos::Logos, Clone, Copy, Debug, PartialEq, Eq)]
    #[logos(skip r"\s+")]
    enum Token {
        #[regex(r"//.*")]
        Comment,

        #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
        Ident,

        #[regex(r"'[^']*'")]
        Regex,

        #[regex(r"[|,()\[\]{}=]")]
        Punctuation,
    }

    struct Config;

    impl ToCodeCategory for Config {
        const NAME: &'static str = "grammar";

        type Token = Token;

        fn to_code_category(s: Result<Self::Token, ()>) -> CodeCategory {
            let Ok(s) = s else {return CodeCategory::Error;};
            match s {
                Token::Comment => CodeCategory::Comment,
                Token::Ident => CodeCategory::Ident,
                Token::Regex => CodeCategory::String,
                Token::Punctuation => CodeCategory::Punctuation,
            }
        }
    }

    CodeCategorySyntaxConfig::<Config>::new()
}

pub fn translate_md(md: &str) -> String {
    let mut stac_syntax = LogosSyntax::new(CodeCategorySyntaxConfig::<StacSyntaxConfig>::new());
    let mut grammar_syntax = LogosSyntax::new(grammar_syntax());
    let config = little_md::Config {
        syntaxes: &mut [&mut stac_syntax, &mut grammar_syntax],
        ..Default::default()
    };

    let mut buf = String::new();
    little_md::emmit(md, config, &mut buf);

    buf
}

#[inline_props]
pub fn MdPage<'a>(cx: Scope, md: &'a str) -> Element {
    let state = use_app_state(cx);
    let translated = use_memo(cx, (), |()| translate_md(md));
    cx.render(rsx! { div {
        class: "code-theme",
        "theme": "{state.theme()}",
        dangerous_inner_html: translated.as_ref()
    } })
}

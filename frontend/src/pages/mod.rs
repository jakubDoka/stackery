use std::future::Future;

use dioxus::{html::input_data::keyboard_types::Code, prelude::Scope};

mod about;
mod home;
mod login;
mod register;
mod search;

pub use {about::About, home::Home, login::Login, register::Register, search::Search};

use dioxus::prelude::*;

#[inline_props]
fn SearchBar<'a, F, R, E>(cx: Scope, desc: &'a str, fetcher: F) -> Element
where
    F: Fn(String) -> R + 'static + Clone,
    R: Future<Output = Result<Vec<String>, E>> + 'static,
    E: ToString + 'static,
{
    let results = use_state::<Vec<String>>(cx, || vec![]);
    let selection = use_state::<usize>(cx, || 0);
    let query = use_state(cx, || String::new());

    cx.render(rsx! {
        div {
            input {
                r#type: "text",
                placeholder: *desc,
                value: query.get().as_str(),
                oninput: move |e| {
                    let fetcher = fetcher.clone();
                    let value = e.value.clone();
                    query.set(value.clone());
                    use_future!(cx, |results| async move {
                        let new_results = fetcher(value).await;
                        match new_results {
                            Ok(res) => results.set(res),
                            Err(e) => results.set(vec![e.to_string()]),
                        };
                    });
                },
                onkeydown: move |e| {
                    let len = results.get().len();
                    let &current = selection.get();

                    let new_selection = match e.code() {
                        Code::ArrowUp => current.wrapping_sub(1),
                        Code::ArrowDown => current.wrapping_add(1),
                        Code::Enter => {
                            if let Some(res) = results.get().get(current) {
                                query.set(res.clone());
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
                for (i, res) in results.iter().enumerate() {
                    div {
                        style: if i == *selection.get() { "background: blue" } else { "" },
                        res.as_str()
                    }
                }
            }
        }
    })
}

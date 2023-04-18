use std::fmt::Display;

use dioxus::prelude::*;

use crate::{
    api::{self, users::LoginError},
    navbar::Account,
};

pub fn Login(cx: Scope) -> Element {
    enum State {
        Empty,
        Loading,
        Error(LoginError),
        Success,
    }

    impl Display for State {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                State::Empty => write!(f, ""),
                State::Loading => write!(f, "Loading..."),
                State::Error(err) => write!(f, "Error: {}", err),
                State::Success => write!(f, "Session is on!"),
            }
        }
    }

    impl State {
        fn from_response(resp: Result<(), LoginError>) -> Self {
            match resp {
                Ok(_) => State::Success,
                Err(err) => State::Error(err),
            }
        }
    }

    let state = use_state(cx, || State::Empty);

    cx.render(rsx! {
        div {
            h1 { "Login Page" }
            form {
                onsubmit: move |ev| {
                    state.set(State::Loading);
                    let values = ev.values.clone();
                    let account = Account::use_state(cx);

                    use_future!(cx, |state, account| async move {
                        let reps = api::users::login(&values).await;
                        let name = values.get("name").unwrap().clone();
                        *account.write() = reps.is_ok().then_some(Account { name });
                        state.set(State::from_response(reps));
                    });
                },
                prevent_default: "onsubmit",
                input { r#type: "text", placeholder: "Username", name: "name" }
                input { r#type: "password", placeholder: "UUID", name: "uuid" }
                button { r#type: "submit", "Login" }
            }
            div { "{state.get()}" }
        }
    })
}

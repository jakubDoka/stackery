use std::fmt::Display;

use dioxus::prelude::*;

use crate::api::{self, users::RegisterError};

pub fn Register(cx: Scope) -> Element {
    enum State {
        Empty,
        Loading,
        Error(RegisterError),
        Success(String),
    }

    impl Display for State {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                State::Empty => write!(f, ""),
                State::Loading => write!(f, "Loading..."),
                State::Error(err) => write!(f, "Error: {}", err),
                State::Success(uuid) => write!(
                    f,
                    "Success! Your UUID is '{}'. Keep it safe in your password manager!",
                    uuid
                ),
            }
        }
    }

    impl State {
        fn from_response(resp: Result<String, RegisterError>) -> Self {
            match resp {
                Ok(uuid) => State::Success(uuid),
                Err(err) => State::Error(err),
            }
        }
    }

    let state = use_state(cx, || State::Empty);

    const NAME: &str = "name";

    cx.render(rsx! {
        div {
            h1 { "Register Page" }
            form {
                onsubmit: move |ev| {
                    state.set(State::Loading);
                    let values = ev.values.clone();
                    use_future!(
                        cx, | state | async move { let reps = api::users::register(& values).await; state
                        .set(State::from_response(reps)); }
                    );
                },
                prevent_default: "onsubmit",
                input { r#type: "text", placeholder: "Username", name: NAME }
                button { r#type: "submit", disabled: matches!(state.get(), State::Loading), "Register" }

                div { "{state}" }
            }
        }
    })
}

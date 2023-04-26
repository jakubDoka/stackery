use std::marker::PhantomData;

use dioxus::prelude::*;

use crate::api::{self, users::RegisterModel};

pub fn Register(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            super::Form {
                fields: PhantomData::<RegisterModel>,
                submit: move |ev| api::users::register(ev),
            }
        }
    })
}

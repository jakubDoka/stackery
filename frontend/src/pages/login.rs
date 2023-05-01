use std::marker::PhantomData;

use dioxus::prelude::*;

use crate::{
    api::{self, users::LoginModel},
    navbar::use_app_state,
};

pub fn Login(cx: Scope) -> Element {
    let app_state = use_app_state(cx);
    cx.render(rsx! {
        super::Form {
            fields: PhantomData::<LoginModel>,
            submit: move |ev| async move {
                let resp = api::users::login(ev).await;
                if resp.is_ok() {
                    app_state.refresh().await;
                }
                resp
            },
        }
    })
}

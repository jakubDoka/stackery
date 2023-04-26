use std::marker::PhantomData;

use dioxus::prelude::*;

use crate::{
    api::{self, users::LoginModel},
    navbar::{use_account, Account},
    pages::Name,
};

pub fn Login(cx: Scope) -> Element {
    let account = use_account(cx);
    cx.render(rsx! {
        super::Form {
            fields: PhantomData::<LoginModel>,
            submit: move |ev| async move {
                let Name(name) = ev.name.clone();
                let resp = api::users::login(ev).await;
                if resp.is_ok() {
                    account.login(Account { name });
                }
                resp
            },
        }
    })
}

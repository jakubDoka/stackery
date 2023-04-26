use std::marker::PhantomData;

use dioxus::prelude::*;

pub fn Post(cx: Scope) -> Element {
    cx.render(rsx! {
        super::Form {
            fields: PhantomData::<crate::api::posts::CreateModel>,
            submit: |model| crate::api::posts::create(model),
        }
    })
}

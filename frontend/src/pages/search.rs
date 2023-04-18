use dioxus::prelude::*;

use crate::api;

pub fn Search(cx: Scope) -> Element {
    cx.render(rsx! {
        super::SearchBar { desc: "search users", fetcher: api::users::search }
    })
}

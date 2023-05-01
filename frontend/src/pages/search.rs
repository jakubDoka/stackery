use dioxus::prelude::*;

use crate::{
    api,
    navbar::NavTab,
    router::{self, Route},
};

pub fn Search(cx: Scope) -> Element {
    router::use_router(cx, "/user");
    cx.render(rsx! {
        div {
            SearchNav {}
            Route {
                to: "/user",
                super::SearchBar {
                    desc: "user name",
                    fetcher: |query| api::users::search(query),
                }
            }
            Route {
                to: "/post",
                super::SearchBar {
                    desc: "post name",
                    fetcher: |query| api::posts::search(query),
                }
            }
        }
    })
}

#[inline_props]
fn SearchNav(cx: Scope) -> Element {
    cx.render(rsx! {
        nav {
            class: "navbar",
            div {
                class: "nav-tabs",
                NavTab { name: "/user" }
                NavTab { name: "/post" }
            }
        }
    })
}

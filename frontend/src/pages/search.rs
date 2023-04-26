use dioxus::prelude::*;

use crate::{
    api,
    navbar::NavTab,
    router::{self, Route, UseRouter},
};

pub fn Search(cx: Scope) -> Element {
    let router = router::use_router(cx, "/user");
    cx.render(rsx! {
        div {
            SearchNav { router: router }
            Route {
                to: router.route("/user"),
                super::SearchBar {
                    desc: "user name",
                    fetcher: |query| api::users::search(query),
                }
            }
            Route {
                to: router.route("/post"),
                super::SearchBar {
                    desc: "post name",
                    fetcher: |query| api::posts::search(query),
                }
            }
        }
    })
}

#[inline_props]
fn SearchNav<'a>(cx: Scope, router: &'a UseRouter) -> Element {
    cx.render(rsx! {
        nav {
            class: "navbar",
            div {
                class: "nav-tabs",
                NavTab { name: "/user", router: router }
                NavTab { name: "/post", router: router }
            }
        }
    })
}

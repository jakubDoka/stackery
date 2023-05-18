use dioxus::prelude::*;

use crate::{
    api,
    navbar::NavTab,
    router::{self, Route},
};

use super::{SearchResult, SearchResultView};

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
                    view: UserView,
                }
            }
            Route {
                to: "/post",
                super::SearchBar {
                    desc: "post name",
                    fetcher: |query| api::posts::search(query),
                    view: (),
                }
            }
            Route { to: "/help", super::MdPage { md: include_str!("search-help.md") } }
        }
    })
}

struct UserView;

impl SearchResultView<String> for UserView {
    fn render<'a>(&'a self, cx: &'a ScopeState, result: Option<&String>) -> Element<'a> {
        let error = use_state(cx, || None);
        let fut_error = error.clone();
        let user_name = &result.cloned();
        let codes = use_future!(cx, |user_name| async move {
            let Some(name) = user_name else {
                return Vec::new();
            };
            match api::posts::for_user(name.clone()).await {
                Ok(codes) => codes,
                Err(e) => {
                    fut_error.set(Some(e.to_string()));
                    Vec::new()
                }
            }
        });

        cx.render(rsx! { div {
            hidden: user_name.is_none(),
            div {
                hidden: user_name.is_none(),
                class: "user-profile",
                div {
                    class: "user-profile-about",
                    h1 { "about" }
                    "no about for users yet"
                }
                div {
                    class: "user-profile-posts",
                    h1 { "posts" }
                    div {
                        class: "user-profile-posts-list",
                        hidden: codes.value().map_or(true, |v| v.is_empty()),
                        for code in codes.value().map(|v| v.iter()).into_iter().flatten() { div {
                            class: "post-search-result",
                            div {
                                class: "post-name",
                                "{code.name}"
                            }
                            super::Code { content: &code.code }
                        } }
                    }
                    div {
                        class: "user-profile-empty-posts",
                        hidden: codes.value().map_or(true, |v| !v.is_empty()),
                        "no posts yet"
                    }
                }
            }
            div {
                class: "form-error",
                hidden: error.get().is_none(),
                error.get().as_deref().unwrap_or("")
            }
        } })
    }
}

impl SearchResult for bf_shared::search::post::Model {
    fn key(&self) -> &str {
        &self.name
    }

    fn render<'a>(&'a self, cx: &'a ScopeState, query: &'a UseState<String>) -> Element<'a> {
        cx.render(rsx! { div {
            class: "post-search-result",
            div {
                class: "post-name",
                "{self.name}"
            }
            div {
                class: "post-author",
                "author: ",
                a {
                    href: "{self.author}",
                    onclick: move |_| {
                        let new_value = bf_shared::search::Query::new(query.get().as_str()).set_field("author", &self.author);
                        query.set(new_value);
                    },
                    prevent_default: "onclick",
                    "{self.author}"
                }
            }
            super::Code { content: &self.code, classes: "post-code" }
        } })
    }
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
                NavTab { name: "/help" }
            }
        }
    })
}

#![allow(incomplete_features)]
#![feature(
    lazy_cell,
    let_chains,
    array_chunks,
    async_fn_in_trait,
    impl_trait_projections,
    iter_collect_into,
    array_windows
)]
#![allow(non_snake_case)]

// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use navbar::use_app_state_provider;
use router::*;

mod api;
mod navbar;
mod pages;
mod router;

// create a component that renders a div with the text "Hello, world!"
pub fn App(cx: Scope) -> Element {
    let app_state = use_app_state_provider(cx);

    use_router(cx, "/");

    cx.render(rsx! {
        body {
            class: "root",
            "theme": "{app_state.theme()}",

            navbar::Navbar {}
            Route { to: "/", pages::Home {} }
            Route { to: "/about", pages::About {} }
            Route { to: "/register", pages::Register {} }
            Route { to: "/login", pages::Login {} }
            Route { to: "/search", pages::Search {} }
            Route { to: "/post", pages::Post {} }
        }
    })
}

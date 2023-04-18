#![feature(once_cell)]
#![allow(non_snake_case)]
// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use dioxus_router::*;
use navbar::Account;

mod api;
mod navbar;
mod pages;

// create a component that renders a div with the text "Hello, world!"
pub fn App(cx: Scope) -> Element {
    Account::init(cx);
    cx.render(rsx! {
        div {
            Router {
                navbar::Navbar {}
                Route { to: "/", pages::Home {} }
                Route { to: "/about", pages::About {} }
                Route { to: "/register", pages::Register {} }
                Route { to: "/login", pages::Login {} }
                Route { to: "/search", pages::Search {} }
            }
        }
    })
}

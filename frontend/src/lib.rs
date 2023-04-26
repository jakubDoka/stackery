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
use std::fmt;

// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use navbar::use_account_provider;
use router::*;

mod api;
mod navbar;
mod pages;
mod router;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Theme {
    Light,
    #[default]
    Dark,
}

impl Theme {
    pub fn cycle(&mut self) {
        *self = match self {
            Theme::Light => Theme::Dark,
            Theme::Dark => Theme::Light,
        }
    }
}

impl fmt::Display for Theme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Theme::Light => write!(f, "light"),
            Theme::Dark => write!(f, "dark"),
        }
    }
}

#[derive(Clone, Copy)]
pub struct UseTheme<'a> {
    state: &'a UseSharedState<Theme>,
}

impl<'a> UseTheme<'a> {
    pub fn cycle(&self) {
        self.state.write().cycle();
    }
}

impl<'a> std::fmt::Display for UseTheme<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.state.read())
    }
}

pub fn use_theme<'a>(cx: &'a ScopeState) -> UseTheme<'a> {
    UseTheme {
        state: use_shared_state::<Theme>(cx).expect("We made sure it was initialized"),
    }
}

pub fn use_theme_provider<'a>(cx: &'a ScopeState) {
    use_shared_state_provider(cx, Theme::default);
}

// create a component that renders a div with the text "Hello, world!"
pub fn App(cx: Scope) -> Element {
    use_account_provider(cx);
    use_theme_provider(cx);

    let theme = use_theme(cx);
    let router = use_router(cx, "/");

    cx.render(rsx! {
        body {
            class: "root",
            "theme": "{theme}",

            navbar::Navbar { router: router }
            Route { to: router.route("/"), pages::Home {} }
            Route { to: router.route("/about"), pages::About {} }
            Route { to: router.route("/register"), pages::Register {} }
            Route { to: router.route("/login"), pages::Login {} }
            Route { to: router.route("/search"), pages::Search {} }
            Route { to: router.route("/post"), pages::Post {} }
        }
    })
}

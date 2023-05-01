use bf_shared::db::session::{AppState, Theme};
use core::fmt;
use dioxus::prelude::*;
use futures_util::StreamExt;
use std::{cell, future::Future};

use crate::{
    api,
    router::{use_link, Link},
};

#[derive(Default, PartialEq, Eq)]
enum AppStateFase {
    LoggedIn(AppState),
    LoggedOut,
    #[default]
    NotLoaded,
}

struct Trigger;

#[derive(Clone, PartialEq)]
pub struct UseAppState {
    state: UseSharedState<AppStateFase>,
    trigger: Option<Coroutine<Trigger>>,
}

pub fn use_app_state_provider(cx: &ScopeState) -> UseAppState {
    use_shared_state_provider(cx, AppStateFase::default);

    let app_state = use_app_state(cx);
    let future_app_state = app_state.clone();
    use_future!(cx, || future_app_state.refresh());

    let future_app_state = app_state.clone();
    use_coroutine(cx, move |mut chan: UnboundedReceiver<Trigger>| async move {
        loop {
            let future = futures_util::future::select(
                chan.next(),
                gloo_timers::future::TimeoutFuture::new(1000 * 5),
            );

            if let futures_util::future::Either::Right(..) = future.await {
                future_app_state.clone().save().await;
                chan.next().await;
            }
        }
    });

    app_state
}

pub fn use_app_state(cx: &ScopeState) -> UseAppState {
    UseAppState {
        state: use_shared_state(cx)
            .expect("AppState is registered")
            .clone(),
        trigger: use_coroutine_handle::<Trigger>(cx).cloned(),
    }
}

impl UseAppState {
    pub fn name(&self) -> Option<cell::Ref<str>> {
        cell::Ref::filter_map(self.state.read(), |state| match state {
            AppStateFase::LoggedIn(state) => Some(state.user_name.as_str()),
            _ => None,
        })
        .ok()
    }

    pub fn theme(&self) -> Theme {
        match *self.state.read() {
            AppStateFase::LoggedIn(ref state) => state.theme,
            _ => Theme::Dark,
        }
    }

    pub fn cycle_theme(&self) {
        match *self.state.write() {
            AppStateFase::LoggedIn(ref mut state) => {
                state.theme = match state.theme {
                    Theme::Light => Theme::Dark,
                    Theme::Dark => Theme::Light,
                };
                self.trigger_action();
            }
            _ => {}
        }
    }

    fn trigger_action(&self) {
        self.trigger.as_ref().unwrap().send(Trigger);
    }

    pub fn is_logged_in(&self) -> bool {
        matches!(*self.state.read(), AppStateFase::LoggedIn(_))
    }

    pub fn logout(&self) -> impl Future<Output = ()> {
        *self.state.write() = AppStateFase::LoggedOut;

        async move {
            let _ = api::users::logout().await;
        }
    }

    pub async fn refresh(self) {
        let state = api::users::get_state().await;

        *self.state.write() = match state {
            Ok(state) => AppStateFase::LoggedIn(state),
            Err(_) => AppStateFase::LoggedOut,
        };
    }

    pub async fn save(self) {
        if let AppStateFase::LoggedIn(ref state) = *self.state.read() {
            let _ = api::users::set_state(&state).await;
        }
    }
}

#[inline_props]
pub fn Navbar(cx: Scope) -> Element {
    struct NameDisplay<'a> {
        name: Option<cell::Ref<'a, str>>,
    }

    impl<'a> fmt::Display for NameDisplay<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.name.as_deref() {
                Some(name) => write!(f, "{}", name),
                None => write!(f, "anonimous"),
            }
        }
    }

    let app_state = use_app_state(&cx);
    let dup = app_state.clone();
    let another_dup = app_state.clone();
    let account = NameDisplay { name: dup.name() };
    let not_logged_in = !app_state.is_logged_in();
    let home_link = use_link(cx, "/");

    cx.render(rsx! {
        nav {
            class: "navbar",
            div {
                class: "nav-tabs",
                NavTab { name: "/" }
                NavTab { name: "/about" }
                NavTab { name: "/register", hidden: !not_logged_in }
                NavTab { name: "/login", hidden: !not_logged_in }
                NavTab { name: "/search" }
                NavTab { name: "/post", hidden: not_logged_in }
            }
            div {
                class: "nav-account",
                div { class: "nav-account-name", "{account}" }
                button {
                    class: "nav-button",
                    hidden: not_logged_in,
                    onclick: move |_| {
                        home_link.activate(cx);
                        app_state.logout()
                    },
                    "logout"
                }
                button {
                    class: "nav-button",
                    hidden: not_logged_in,
                    onclick: move |_| another_dup.cycle_theme(),
                    "theme"
                }
            }
        }
    })
}

#[inline_props]
pub fn NavTab(cx: Scope, name: &'static str, hidden: Option<bool>) -> Element {
    cx.render(rsx! {
        Link { class: "nav-tab", active_class: "nav-active-tab", to: name, hidden: hidden.unwrap_or(false), *name }
    })
}

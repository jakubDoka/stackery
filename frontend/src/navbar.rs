use std::future::Future;

use dioxus::prelude::*;

use crate::{
    api,
    router::{Link, UseRouter},
    use_theme,
};

#[derive(PartialEq, Eq)]
pub struct Account {
    pub name: String,
}

#[derive(Clone)]
pub struct UseAccount {
    state: UseSharedState<Option<Account>>,
}

impl UseAccount {
    pub fn logout(&self) -> impl Future<Output = ()> {
        let account = self.state.write().take();

        web_sys::window()
            .expect("We are in a browser")
            .location()
            .reload()
            .expect("We are in a browser");

        let is_some = account.is_some();
        async move {
            if is_some {
                let _ = api::users::logout().await;
            }
        }
    }

    pub fn is_logged_in(&self) -> bool {
        self.state.read().is_some()
    }

    pub fn login(&self, account: Account) {
        self.state.write().replace(account);
    }
}

impl<'a> std::fmt::Display for UseAccount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.state.read().as_ref() {
            Some(account) => write!(f, "{}", account.name),
            None => write!(f, "anonimous"),
        }
    }
}

pub fn use_account<'a>(cx: &'a ScopeState) -> UseAccount {
    UseAccount {
        state: use_shared_state::<Option<Account>>(cx)
            .expect("We mad sure it was initialized")
            .clone(),
    }
}

pub fn use_account_provider<'a>(cx: &'a ScopeState) {
    use_shared_state_provider(cx, || None::<Account>);
}

#[inline_props]
pub fn Navbar<'a>(cx: Scope, router: &'a UseRouter) -> Element {
    let account = use_account(cx);
    let theme = use_theme(cx);

    cx.render(rsx! {
        nav {
            class: "navbar",
            div {
                class: "nav-tabs",
                NavTab { name: "/", router: router }
                NavTab { name: "/about", router: router }
                NavTab { name: "/register", router: router }
                NavTab { name: "/login", router: router }
                NavTab { name: "/search", router: router }
                NavTab { name: "/post", router: router, hidden: account.is_logged_in() && false }
            }
            div {
                class: "nav-account",
                div { class: "nav-account-name", "{account}" }
                button {
                    class: "nav-button",
                    hidden: account.is_logged_in(),
                    onclick: move |_| account.logout(),
                    "logout"
                }
                button {
                    class: "nav-button",
                    onclick: move |_| theme.cycle(),
                    "theme"
                }
            }
        }
    })
}

#[inline_props]
pub fn NavTab<'a>(
    cx: Scope,
    name: &'static str,
    router: &'a UseRouter,
    hidden: Option<bool>,
) -> Element {
    cx.render(rsx! {
        Link { class: "nav-tab", active_class: "nav-active-tab", to: router.link(name), hidden: hidden.unwrap_or(false), *name }
    })
}

use dioxus::prelude::*;
use dioxus_router::*;

use crate::api;

#[derive(PartialEq, Eq)]
pub struct Account {
    pub name: String,
}

impl Account {
    pub fn logout<T>(cx: Scope<T>) -> Option<Account> {
        let state = Self::use_state(cx);
        let account = state.write().take();

        if account.is_some() {
            use_future!(cx, || api::users::logout());
        }

        account
    }

    pub fn init<T>(cx: Scope<T>) {
        use_shared_state_provider(cx, || None::<Account>);
    }

    pub fn use_state<'a, T>(cx: Scope<'a, T>) -> &'a UseSharedState<Option<Account>> {
        use_shared_state::<Option<Account>>(cx).expect("We mad sure it was initialized")
    }
}

pub fn Navbar(cx: Scope) -> Element {
    let account = Account::use_state(cx);

    struct AccountDisplay<'a>(&'a UseSharedState<Option<Account>>);

    impl<'a> std::fmt::Display for AccountDisplay<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0.read().as_ref() {
                Some(account) => write!(f, "Stacker: {}", account.name),
                None => write!(f, "Stacking anonymously"),
            }
        }
    }

    cx.render(rsx! {
        nav {
            Link { to: "/", "Home" }
            Link { to: "/about", "About" }
            Link { to: "/register", "Register" }
            Link { to: "/login", "Login" }
            Link { to: "/search", "Search" }
            span { "{AccountDisplay(account)}" }
            button {
                onclick: |_| {
                    Account::logout(cx);
                },
                hidden: account.read().is_none(),
                "Logout"
            }
        }
    })
}

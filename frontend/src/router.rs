use std::cell::Cell;

use dioxus::prelude::*;

#[inline_props]
pub fn Route<'a>(cx: Scope, to: RouteArg<'a>, children: Element<'a>) -> Element<'a> {
    cx.render(rsx! {
        div {
            class: if to.is_active(cx) {
                "route-active"
            } else {
                "route-hidden"
            },
            children
        }
    })
}

#[inline_props]
pub fn Link<'a>(
    cx: Scope,
    to: LinkArg<'a>,
    class: Option<&'a str>,
    active_class: Option<&'a str>,
    hidden: Option<bool>,
    children: Element<'a>,
) -> Element<'a> {
    let class = class.unwrap_or("");
    let active_class = active_class.filter(|_| to.is_active(cx)).unwrap_or("");
    cx.render(rsx! {
        a {
            hidden: hidden.unwrap_or(false),
            class: format_args!("{} {}", class, active_class),
            href: to.route,
            onclick: move |_| {
                to.activate(cx);
            },
            prevent_default: "onclick",
            children
        }
    })
}

pub fn use_router<'a>(cx: &'a ScopeState, default_route: &'static str) -> &'a UseRouter {
    let hook = cx.use_hook(|| UseRouter {
        current_route: default_route.into(),
        register_index: 0.into(),
        subscribers: Vec::new().into(),
    });

    hook.register_index.set(0);

    hook
}

/// A handle to the current location of the router.
pub struct UseRouter {
    current_route: Cell<&'static str>,
    register_index: Cell<usize>,
    subscribers: Cell<Vec<ScopeId>>,
}

impl UseRouter {
    pub fn route(&self, route: &'static str) -> RouteArg {
        RouteArg { route, inner: self }
    }

    pub fn link(&self, route: &'static str) -> LinkArg {
        LinkArg { route, inner: self }
    }

    fn is_active(&self, route: &'static str, cx: &ScopeState) -> bool {
        let mut callbacks = self.subscribers.take();

        if self.register_index.get() < callbacks.len() {
            self.subscribers.set(callbacks);
            return self.current_route.get() == route;
        }

        self.register_index.set(self.register_index.get() + 1);
        callbacks.push(cx.scope_id());
        self.subscribers.set(callbacks);

        self.current_route.get() == route
    }
}

pub struct LinkArg<'a> {
    route: &'static str,
    inner: &'a UseRouter,
}

impl<'a> LinkArg<'a> {
    pub fn activate(&self, cx: &ScopeState) {
        let subscribers = self.inner.subscribers.take();
        for &id in subscribers.iter() {
            cx.needs_update_any(id);
        }
        self.inner.current_route.set(self.route);
        self.inner.subscribers.set(subscribers);
    }

    pub fn is_active(&self, cx: &'a ScopeState) -> bool {
        self.inner.is_active(self.route, cx)
    }
}

pub struct RouteArg<'a> {
    route: &'static str,
    inner: &'a UseRouter,
}

impl<'a> RouteArg<'a> {
    pub fn is_active(&self, cx: &'a ScopeState) -> bool {
        self.inner.is_active(self.route, cx)
    }
}

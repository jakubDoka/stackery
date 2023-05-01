use std::cell::Cell;
use std::rc::Rc;

use dioxus::prelude::*;

#[inline_props]
pub fn Route<'a>(cx: Scope, to: &'static str, children: Element<'a>) -> Element<'a> {
    let to = use_route(cx, to);
    let active = to.is_active();
    cx.render(rsx! { section {
        class: if active {
            "route-active"
        } else {
            "route-hidden"
        },
        children
    } })
}

#[inline_props]
pub fn Link<'a>(
    cx: Scope,
    to: &'static str,
    class: Option<&'a str>,
    active_class: Option<&'a str>,
    hidden: Option<bool>,
    children: Element<'a>,
) -> Element<'a> {
    let to = use_link(cx, to);
    let class = class.unwrap_or("");
    let active_class = active_class.filter(|_| to.is_active()).unwrap_or("");
    cx.render(rsx! { a {
        hidden: hidden.unwrap_or(false),
        class: format_args!("{} {}", class, active_class),
        href: to.route,
        onclick: move |_| to.activate(cx),
        prevent_default: "onclick",
        children
    } })
}

pub fn use_router<'a>(cx: &'a ScopeState, default_route: &'static str) {
    use_context_provider(cx, || {
        Rc::new(Router {
            current_route: default_route.into(),
            subscribers: Vec::new().into(),
        })
    });
}

/// A handle to the current location of the router.
struct Router {
    current_route: Cell<&'static str>,
    subscribers: Cell<Vec<ScopeId>>,
}

impl Router {
    fn is_active(&self, route: &'static str) -> bool {
        self.current_route.get() == route
    }

    fn register(&self, cx: &ScopeState) {
        let mut subscribers = self.subscribers.take();
        subscribers.push(cx.scope_id());
        self.subscribers.set(subscribers);
    }
}

pub fn use_link<'a>(cx: &'a ScopeState, route: &'static str) -> &'a UseLink {
    let inner = use_context::<Rc<Router>>(cx).expect("Router not initialized");
    cx.use_hook(|| {
        inner.register(cx);
        UseLink {
            route,
            inner: inner.clone(),
        }
    })
}

pub struct UseLink {
    route: &'static str,
    inner: Rc<Router>,
}

impl UseLink {
    pub fn activate(&self, cx: &ScopeState) {
        let subscribers = self.inner.subscribers.take();
        for &id in subscribers.iter() {
            cx.needs_update_any(id);
        }
        self.inner.current_route.set(self.route);
        self.inner.subscribers.set(subscribers);
    }

    pub fn is_active(&self) -> bool {
        self.inner.is_active(self.route)
    }
}

pub fn use_route<'a>(cx: &'a ScopeState, route: &'static str) -> &'a UseRoute {
    let inner = use_context::<Rc<Router>>(cx).expect("Router not initialized");
    cx.use_hook(|| {
        inner.register(cx);
        UseRoute {
            route,
            inner: inner.clone(),
        }
    })
}

pub struct UseRoute {
    route: &'static str,
    inner: Rc<Router>,
}

impl UseRoute {
    pub fn is_active(&self) -> bool {
        self.inner.is_active(self.route)
    }
}

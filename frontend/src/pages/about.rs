use dioxus::prelude::*;

pub fn About(cx: Scope) -> Element {
    cx.render(rsx! { div { "About Page" } })
}

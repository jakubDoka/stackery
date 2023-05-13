use dioxus::prelude::*;

pub fn About(cx: Scope) -> Element {
    let translated = use_memo(cx, (), |()| {
        markdown_to_html::markdown(include_str!("about.md"))
    });
    cx.render(rsx! { div { dangerous_inner_html: translated.as_ref() } })
}

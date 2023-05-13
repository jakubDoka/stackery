use dioxus::prelude::*;

pub fn Home(cx: Scope) -> Element {
    let translated = use_memo(cx, (), |()| {
        markdown_to_html::markdown(include_str!("home.md"))
    });
    cx.render(rsx! { div { dangerous_inner_html: translated.as_ref() } })
}

pub mod posts;
pub mod users;

static CLIENT: std::sync::LazyLock<reqwest::Client> =
    std::sync::LazyLock::new(|| reqwest::Client::new());

const OTHER_MESSAGE: &str =
    concat!("something went wrong (for dev: check browser console for logs)",);

#[macro_export]
macro_rules! reqwest_error_handler {
    ($error:ident) => {
        impl From<reqwest::Error> for $error {
            fn from(error: reqwest::Error) -> Self {
                log::error!("reqwest error: {}", error);
                Self::Other
            }
        }
    };
}

#[macro_export]
macro_rules! reqwest_unexpected_status {
    ($response:expr, $error:ident) => {{
        log::error!("unexpected status code: {}", $response.status());
        log::error!("response body: {}", $response.text().await.unwrap());
        Err($error::Other)
    }};
}

#[macro_export]
macro_rules! url {
    ($($args:tt)*) => {{
        let origin = $crate::api::origin();
        format!("{}/{}", origin, format!($($args)*))
    }};
}

pub fn origin() -> String {
    web_sys::window().map(|w| w.origin()).unwrap_or_default()
}

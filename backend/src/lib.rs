#![feature(lazy_cell)]

#[macro_export]
macro_rules! http_try {
    ($result:expr, $error:expr, $level:ident($err:ident) $($arg:tt)*) => {
        match $result {
            Ok(ok) => ok,
            Err($err) => {
                $crate::log!($level, $($arg)*);
                return $error;
            }
        }
    };

    ($result:expr, $error:expr, $level:ident $($arg:tt)*) => {
        match $result {
            Ok(ok) => ok,
            Err(err) => {
                $crate::log!($level, $($arg)*; "ERROR" => err.to_string());
                return $error;
            }
        }
    };
}

mod config;
mod db;
mod endpoints;
mod log;
mod search_client;

#[cfg_attr(test, mockall_double::double)]
pub use search_client::SearcherClient;

pub async fn run() {
    config_var! {
        HOST: String = "127.0.0.1:8080".into();
    }

    let db = db::connect().await;

    let search_client = search_client::connect();
    let users = SearcherClient::new(&search_client).await;
    let posts = SearcherClient::new(&search_client).await;

    let router = endpoints::router(db, users, posts).await;

    let listener = poem::listener::TcpListener::bind(HOST.as_str());

    log!(info, "listening on {}", HOST.as_str());

    poem::Server::new(listener).run(router).await.unwrap();
}

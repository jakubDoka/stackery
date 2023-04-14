#![feature(once_cell)]

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
}

mod config;
mod db;
mod log;
mod posts;
mod users;

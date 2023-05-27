use std::fs::OpenOptions;

use slog::{Drain, Logger};

use crate::config_var;

struct PrintlnWriter;

impl std::io::Write for PrintlnWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        print!("{}", s);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

static LOGGER: std::sync::LazyLock<Logger> = std::sync::LazyLock::new(|| {
    config_var! {
        LOG_FILE: Option<String>;
    }

    let Some(file) = LOG_FILE.as_ref() else {
        let format = slog_json::Json::default(PrintlnWriter).fuse();
        let drain = slog_async::Async::new(format).build().fuse();
        return slog::Logger::root(drain, slog::o!());
    };

    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(file)
        .expect("failed to open log file");
    let format = slog_json::Json::default(file).fuse();
    let drain = slog_async::Async::new(format).build().fuse();

    slog::Logger::root(drain, slog::o!())
});

pub fn get() -> &'static Logger {
    &LOGGER
}

#[macro_export]
macro_rules! log {
    ($level:ident, $($arg:tt)*) => {
        slog::$level!($crate::log::get(), $($arg)*)
    };
}

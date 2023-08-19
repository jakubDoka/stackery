#![feature(array_windows, decl_macro)]

use std::{
    env,
    path::{Path, PathBuf},
};

mod diff;

pub fn function_name<T>(_: T) -> String {
    std::any::type_name::<T>().replace("::", "-")
}

struct Config {
    cahce_dir: PathBuf,
    write_changes: bool,
    accept_filter: Option<String>,
}

impl Config {
    fn new() -> Self {
        Self {
            cahce_dir: env::var("PRINT_TEST_CACHE_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|_| PathBuf::from("print-test-cache")),
            write_changes: env::var("PRINT_TEST_WRITE_CHANGES")
                .map(|s| s == "1")
                .unwrap_or(false),
            accept_filter: env::var("PRINT_TEST_ACCEPT_FILTER").ok(),
        }
    }
}

fn get_config() -> &'static Config {
    static CONFIG: std::sync::OnceLock<Config> = std::sync::OnceLock::new();
    CONFIG.get_or_init(Config::new)
}

pub fn case(name: &str, body: impl FnOnce(&str, &mut String)) {
    struct Case<'a> {
        name: &'a str,
        buffer: String,
    }

    fn save_result(name: &str, source: &str) {
        use std::fmt::Write;

        struct Dump {
            buffer: String,
        }

        impl Drop for Dump {
            fn drop(&mut self) {
                if self.buffer.is_empty() {
                    return;
                }
                eprintln!("{}", self.buffer);
            }
        }

        let mut dump = Dump {
            buffer: String::new(),
        };

        macro log($dump:ident, $($arg:tt)*) {
            writeln!($dump.buffer, "print-test: {}", format_args!($($arg)*)).unwrap();
        }

        fn usage_hint(dump: &mut Dump) {
            log!(dump, "to save, run with 'PRINT_TEST_WRITE_CHANGES=1'");
        }

        let config = get_config();

        let path = config.cahce_dir.join(name);

        fn write_file(dump: &mut Dump, path: &Path, source: &str, should_run: bool) -> bool {
            if !should_run {
                usage_hint(dump);
                return true;
            }

            if let Err(e) = std::fs::create_dir_all(path.parent().unwrap_or(Path::new("."))) {
                log!(dump, "failed to create directory: {}", e);
                return false;
            }
            if let Err(e) = std::fs::write(path, source) {
                log!(dump, "failed to write to file: {}", e);
                return false;
            }

            true
        }

        if !path.exists() {
            if !write_file(&mut dump, &path, source, config.write_changes) {
                return;
            }

            log!(dump, "no previous result found, current form:");

            for line in source.lines() {
                log!(dump, "  {}", line);
            }

            if !std::thread::panicking() && !config.write_changes {
                panic!("new test case detected");
            }

            return;
        }

        let prev = match std::fs::read_to_string(&path) {
            Ok(prev) => prev,
            Err(e) => {
                log!(dump, "failed to read from file: {}", e);
                return;
            }
        };

        let diff = diff::lines(&prev, source);

        if diff.iter().all(|d| matches!(d, diff::Result::Both(..))) {
            return;
        }

        log!(dump, "changes detected for test '{}':", name);
        for line in diff.iter() {
            let ansi_term = "\u{001b}[0m";
            match line {
                diff::Result::Both(line, ..) => {
                    log!(dump, "  {}", line);
                }
                diff::Result::Left(line) => {
                    let ansi_red = "\u{001b}[31m";
                    log!(dump, "{ansi_red}- {line}{ansi_term}");
                }
                diff::Result::Right(line) => {
                    let ansi_green = "\u{001b}[32m";
                    log!(dump, "{ansi_green}+ {line}{ansi_term}");
                }
            }
        }

        let write_changes = config.write_changes
            && config
                .accept_filter
                .as_ref()
                .map_or(true, |filter| dump.buffer.contains(filter));

        write_file(&mut dump, &path, source, write_changes);

        if !std::thread::panicking() && !write_changes {
            panic!("test '{}' failed", name);
        }
    }

    let mut case = Case {
        name,
        buffer: String::new(),
    };

    body(name, &mut case.buffer);

    impl Drop for Case<'_> {
        fn drop(&mut self) {
            if std::thread::panicking() {
                self.buffer.push_str("\n\n");
                self.buffer
                    .push_str("Panic occurred during test execution.");
            }

            save_result(self.name, &self.buffer);
        }
    }
}

#[macro_export]
macro_rules! cases {
    ($(fn $name:ident($test_name:tt, $ctx:ident) $body:block)*) => {$(
        #[test]
        fn $name() {
            let fn_name = $crate::function_name($name);
            let test_fn = |$test_name: &str, $ctx: &mut String| $body;
            $crate::case(&fn_name, test_fn);
        }
    )*};
}

#[cfg(test)]
mod tests {
    use super::*;

    cases! {
        fn test1(_, ctx) {
            ctx.push_str("test1");
        }

        fn test2(_, ctx) {
            ctx.push_str("test2");
        }
    }
}

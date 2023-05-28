#![feature(array_windows, decl_macro)]

use std::{
    env,
    path::{Path, PathBuf},
};

mod diff;

pub fn function_name<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
}

struct Config {
    cahce_dir: PathBuf,
    auto_git_add: bool,
    write_changes: bool,
}

impl Config {
    fn new() -> Self {
        Self {
            cahce_dir: env::var("PRINT_TEXT_CACHE_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|_| PathBuf::from("print-test-cache")),
            auto_git_add: env::var("PRINT_TEST_AUTO_GIT_ADD")
                .map(|s| s == "1")
                .unwrap_or(false),
            write_changes: env::var("PRINT_TEST_WRITE_CHANGES")
                .map(|s| s == "1")
                .unwrap_or(false),
        }
    }
}

fn get_config() -> &'static Config {
    static CONFIG: std::sync::OnceLock<Config> = std::sync::OnceLock::new();
    CONFIG.get_or_init(|| Config::new())
}

pub fn case(name: &str, body: impl FnOnce(&mut String)) {
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

        let config = get_config();

        let path = config.cahce_dir.join(name);

        fn write_file(dump: &mut Dump, path: &Path, source: &str, should_run: bool) -> bool {
            if !should_run {
                return true;
            }

            if let Err(e) = std::fs::create_dir_all(path.parent().unwrap_or(Path::new("."))) {
                log!(dump, "failed to create directory: {}", e);
                return false;
            }
            if let Err(e) = std::fs::write(&path, source) {
                log!(dump, "failed to write to file: {}", e);
                return false;
            }

            true
        }

        fn run_git_add(dump: &mut Dump, path: &Path, should_run: bool) {
            if !should_run {
                return;
            }

            log!(dump, "adding file to git:\n  git add {}", path.display());
            let output = std::process::Command::new("git")
                .arg("add")
                .arg(path)
                .output()
                .expect("failed to execute git add");

            if !output.status.success() {
                log!(dump, "failed to execute git add");
                log!(dump, "  status: {}", output.status);
                log!(
                    dump,
                    "  stdout: {}",
                    String::from_utf8_lossy(&output.stdout)
                );
                log!(
                    dump,
                    "  stderr: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        }

        if !path.exists() {
            if !write_file(&mut dump, &path, source, config.write_changes) {
                return;
            }

            log!(
                dump,
                "no previous result found, saving current result:\n{}",
                source
            );

            run_git_add(&mut dump, &path, config.auto_git_add);

            return;
        }

        let prev = match std::fs::read_to_string(&path) {
            Ok(prev) => prev,
            Err(e) => {
                log!(dump, "failed to read from file: {}", e);
                return;
            }
        };

        let diff = diff::lines(&prev, &source);

        if diff.iter().all(|d| matches!(d, diff::Result::Both(..))) {
            return;
        }

        log!(dump, "changes detected for test '{}':", name);
        let mut displaying = false;
        for (i, line) in diff.iter().enumerate() {
            let ansi_term = "\u{001b}[0m";
            match line {
                diff::Result::Both(line, ..) => {
                    let window_radius = 1;
                    let window = i.saturating_sub(window_radius)
                        ..=i.saturating_add(window_radius).min(diff.len());
                    let contains_changes = diff[window]
                        .iter()
                        .any(|d| !matches!(d, diff::Result::Both(..)));

                    match (displaying, contains_changes) {
                        (false, true) => {
                            displaying = true;
                        }
                        (true, false) => {
                            log!(dump, "...");
                            displaying = false;
                        }
                        _ => {}
                    }

                    if displaying {
                        log!(dump, "  {}", line);
                    }
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

        write_file(&mut dump, &path, source, config.write_changes);

        run_git_add(&mut dump, &path, config.auto_git_add);

        if !std::thread::panicking() {
            panic!("test '{}' failed", name);
        }
    }

    let mut case = Case {
        name,
        buffer: String::new(),
    };

    body(&mut case.buffer);

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
    ($(fn $name:ident($ctx:ident) $body:block)*) => {$(
        #[test]
        fn $name() {
            let fn_name = $crate::function_name($name);
            let test_fn = |$ctx: &mut String| $body;
            $crate::case(fn_name, test_fn);
        }
    )*};
}

#[cfg(test)]
mod tests {
    use super::*;

    cases! {
        fn test1(ctx) {
            ctx.push_str("test1");
        }

        fn test2(ctx) {
            ctx.push_str("test2");
        }
    }
}

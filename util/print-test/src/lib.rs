#![feature(array_windows)]

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
}

impl Config {
    fn new() -> Self {
        Self {
            cahce_dir: env::var("PRINT_TEXT_CACHE_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|_| PathBuf::from("print-test-cache")),
            auto_git_add: env::var("PRINT_TEXT_AUTO_GIT_ADD")
                .map(|s| s == "1")
                .unwrap_or(true),
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
        let config = get_config();

        let path = config.cahce_dir.join(name);

        fn write_file(path: &Path, source: &str) -> bool {
            if let Err(e) = std::fs::create_dir_all(path.parent().unwrap_or(Path::new("."))) {
                eprintln!("print-test: failed to create directory: {}", e);
                return false;
            }
            if let Err(e) = std::fs::write(&path, source) {
                eprintln!("print-test: failed to write to file: {}", e);
                return false;
            }

            true
        }

        fn run_git_add(path: &Path, should_run: bool) {
            if !should_run {
                return;
            }

            println!(
                "print-test: adding file to git:\n  git add {}",
                path.display()
            );
            let output = std::process::Command::new("git")
                .arg("add")
                .arg(path)
                .output()
                .expect("failed to execute git add");

            if !output.status.success() {
                eprintln!("print-test: failed to execute git add");
                eprintln!("  status: {}", output.status);
                eprintln!("  stdout: {}", String::from_utf8_lossy(&output.stdout));
                eprintln!("  stderr: {}", String::from_utf8_lossy(&output.stderr));
            }
        }

        if !path.exists() {
            if !write_file(&path, source) {
                return;
            }

            println!(
                "print-test: no previous result found, saving current result:\n{}",
                source
            );

            run_git_add(&path, config.auto_git_add);

            return;
        }

        let prev = match std::fs::read_to_string(&path) {
            Ok(prev) => prev,
            Err(e) => {
                eprintln!("print-test: failed to read from file: {}", e);
                return;
            }
        };

        let diff = diff::lines(&source, &prev);

        if diff.iter().all(|d| matches!(d, diff::Result::Both(..))) {
            println!("print-test: no changes detected for test '{}'", name);
            return;
        }

        println!("print-test: changes detected for test '{}':", name);
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
                            println!("...");
                            displaying = false;
                        }
                        _ => {}
                    }

                    if displaying {
                        println!("  {}", line);
                    }
                }
                diff::Result::Left(line) => {
                    let ansi_red = "\u{001b}[31m";
                    println!("{ansi_red}- {line}{ansi_term}");
                }
                diff::Result::Right(line) => {
                    let ansi_green = "\u{001b}[32m";
                    println!("{ansi_green}+ {line}{ansi_term}");
                }
            }
        }

        write_file(&path, source);

        run_git_add(&path, config.auto_git_add);
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

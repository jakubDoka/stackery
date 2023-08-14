use std::process::{self, Output};

use stac::LoaderMock;

use super::*;

struct LoaderProviderMock<'a>(&'a str);

impl<'a> LoaderProvider for LoaderProviderMock<'a> {
    type Loader = LoaderMock<'a>;

    fn provide(self, _: PathBuf) -> Self::Loader {
        LoaderMock::new(self.0)
    }
}

fn perform_test(name: &str, sources: &str, ctx: &mut String) {
    let lp = LoaderProviderMock(sources);
    let command = Command {
        root: PathBuf::from("root"),
        target: None,
        output: name.to_owned(),
        object_only: false,
        dump_ir: true,
    };
    let mut stdout = Vec::<u8>::new();

    command.run(lp, &mut stdout);
    *ctx = String::from_utf8(stdout).unwrap();

    let Output { status, .. } = process::Command::new(format!("./{name}")).output().unwrap();
    ctx.push_str(&format!("status: {}", status));

    std::fs::remove_file(name).unwrap();
}

stac::print_cases! { perform_test:
    minimal "let main = fn(): :{bi}.i32 42";
}

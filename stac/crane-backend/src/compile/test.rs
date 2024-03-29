use stac::LoaderMock;

use super::*;

struct LoaderProviderMock<'a>(&'a str);

impl<'a> LoaderProvider for LoaderProviderMock<'a> {
    type Loader = LoaderMock<'a>;

    fn provide(self, _: PathBuf) -> io::Result<Self::Loader> {
        Ok(LoaderMock::new(self.0))
    }
}

fn perform_test(name: &str, sources: &str, ctx: &mut String) {
    let lp = LoaderProviderMock(sources);
    let command = Command {
        input: PathBuf::from("root"),
        target: None,
        output: name.to_owned(),
        object_only: false,
        dump_ir: true,
        run: true,
        isa_params: "".into(),
    };
    let mut stdout = Vec::<u8>::new();

    command.run(lp, &mut stdout);
    *ctx = String::from_utf8(stdout).unwrap();

    _ = std::fs::remove_file(name);
}

stac::print_cases! { perform_test:
    minimal "
        let main = fn(): :{bi}.i32 42
    ";
    fib "
        let i32 = :{bi}.i32
        let main = fn(): i32 fib(10)
        let fib = fn(n: i32): i32
            if n < 2
                n
            else
                fib(n - 1) + fib(n - 2)
    ";
    sparter_fib "
        let i32 = :{bi}.i32
        let main = fn(): i32 fib(rt 10)
        let fib = fn(n: i32): i32 fib_rcur(n, 0, 1)
        let fib_rcur = fn(n: i32, a: i32, b: i32): i32
            if n == 0
                a
            else
                fib_rcur(n - 1, b, a + b)
    ";
    multifile "
        `root
            let i32 = :{other}.i32
            let main = fn(): i32 :{other}.main()
        `

        `other
            let i32 = :{bi}.i32
            let main = fn(): i32 42
        `
    ";
    globals "
        let i32 = :{bi}.i32
        let foo = goo() - 9
        let goo = fn(): i32 43 + 8
        let main = fn(): i32 foo
    ";

    bit_struct "
        let i32 = :{bi}.i32
        let Point = struct { x: i32 y: i32 }
        let main = fn(): i32 {
            let x = 1
            let p = Point.{ x, y: 2 }
            p.y + p.x
        }
    ";
    stack_struct "
        let uint = :{bi}.uint
        let Point = struct { x: uint y: uint }
        let main = fn(): uint {
            let x = 1
            let p = Point.{ x, y: 2 }
            p.x + p.y
        }
    ";
    assign_stack "
        let uint = :{bi}.uint
        let Point = struct { x: uint y: uint }
        let main = fn(): uint {
            let x = 1
            let mut p = Point.{ x, y: 2 }
            p.x = Point.{ x: 3, y: 4 }.x
            p.x + p.y
        }
    ";
    copy_stack "
        let uint = :{bi}.uint
        let Point = struct { x: uint y: uint }
        let main = fn(): uint {
            let mut p = Point.{ x: 1, y: 2 }
            p = Point.{ x: 3, y: 4 }
            p.x + p.y
        }
    ";
}

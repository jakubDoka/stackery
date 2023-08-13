use std::{
    collections::HashMap,
    fmt::Display,
    future::Future,
    path::{Path, PathBuf},
    str::FromStr,
};

use crane_backend::{Emmiter, Generator};
use pollster::FutureExt;

use stac::{
    mini_alloc::{ArenaBase, DiverBase, IdentStr},
    BuiltInType, Diagnostics, FileRef, Files, FuncId, Instrs, Interpreter, LoaderFut, LoaderRes,
    ModuleLoader, ModuleMeta, Modules, Type,
};
use target_lexicon::PointerWidth;

#[derive(clap::Args)]
pub struct Command {
    #[clap(long, short, default_value = "main.stac")]
    root: PathBuf,
    #[clap(long, short)]
    target: Option<String>,
    #[clap(long, short, default_value = "a")]
    output: String,
    #[clap(long, short = 'O')]
    object_only: bool,
}

impl Command {
    pub fn run(self) {
        let target = resolve_target(self.target);
        let arch = resolve_arch(&target);

        let mut loader = Loader::new(self.root.clone());
        let mut diags = Diagnostics::default();
        let mut modules = Modules::new();

        let root = match self.root.canonicalize() {
            Ok(root) => root,
            Err(err) => terminate(format_args!(
                "failed to canonicalize root file path ({}): {}",
                self.root.display(),
                err
            )),
        };
        let root = IdentStr::from_str(root.to_str().unwrap());

        let Some(meta) = ModuleLoader::new(&mut modules, &mut diags)
            .update(root, &mut loader)
            .block_on()
        else {
            terminate(diags);
        };

        let mut instrs = Instrs::new();

        Self::build_instrs(&meta, &modules, arch, &mut diags, &mut instrs);

        let Some(entry) = instrs.entry_of(meta.root()) else {
            terminate(format_args!(
                "no entry point found, expencted function called \"main\" in \"{}\"",
                modules.files()[meta.root()].name().display()
            ));
        };

        let entry = FuncId {
            module: meta.root(),
            func: entry,
        };
        let mut call_frontier = vec![stac::Entry {
            id: entry,
            inputs: Vec::new(),
            ret: Type::BuiltIn(BuiltInType::I32),
        }];

        let mut gen = Generator::new(target.clone());

        while let Some(entry) = call_frontier.pop() {
            let mut interp = Interpreter::new(arch, &mut diags, &modules, &instrs);
            let ir = interp.eval(&entry, &mut call_frontier);
            if diags.error_count() > 0 {
                terminate(diags);
            }

            let emmiter = Emmiter::new(&mut diags, &mut gen, &modules, &instrs, arch);
            emmiter.emit(&entry, ir);

            if diags.error_count() > 0 {
                terminate(diags);
            }
        }

        let object = gen.finish();
        let object_name = format!("{}.o", self.output);
        let object_path = Path::new(&object_name);
        std::fs::write(object_path, object).unwrap();

        if self.object_only {
            diags
                .builder(modules.files())
                .footer(stac::Severty::Note, "finished compiling")
                .footer(
                    stac::Severty::Note,
                    format_args!("output: {}", object_path.display()),
                );
        } else {
            std::process::Command::new("cc")
                .arg(object_path)
                .arg("-o")
                .arg(&self.output)
                .status()
                .unwrap();
            std::fs::remove_file(object_path).unwrap();

            diags
                .builder(modules.files())
                .footer(stac::Severty::Note, "finished compiling")
                .footer(stac::Severty::Note, format_args!("output: {}", self.output));
        }

        println!("{}", diags);
    }

    fn build_instrs(
        meta: &ModuleMeta,
        modules: &Modules,
        arch: stac::Layout,
        diags: &mut Diagnostics,
        instrs: &mut Instrs,
    ) {
        let mut arena = ArenaBase::new(1 << 12);
        let mut diver = DiverBase::new(1 << 8);
        let mut resolver = stac::resolver();

        for &module in meta.order() {
            let scope = arena.scope();
            let diver = diver.untyped_dive();
            let parser = stac::Parser::new(modules.files(), module, diags, &scope);
            let Some(ast) = parser.parse(diver) else {
                terminate(diags);
            };

            let builder = stac::InstrBuilder::new(arch, instrs, modules, diags, &mut resolver);
            if builder.build(module, ast).is_none() || diags.error_count() > 0 {
                terminate(diags);
            }
        }
    }
}

fn resolve_target(target: Option<String>) -> target_lexicon::Triple {
    let Some(target) = target else {
        return target_lexicon::Triple::host();
    };

    match target_lexicon::Triple::from_str(&target) {
        Ok(target) => target,
        Err(err) => terminate(format_args!("invalid target: {}", err)),
    }
}

fn resolve_arch(target: &target_lexicon::Triple) -> stac::Layout {
    match target.architecture.pointer_width() {
        Ok(PointerWidth::U16) => stac::Layout::ARCH_16,
        Ok(PointerWidth::U32) => stac::Layout::ARCH_32,
        Ok(PointerWidth::U64) => stac::Layout::ARCH_64,
        Err(()) => terminate("target has unsupported pointer width"),
    }
}

fn terminate(diags: impl Display) -> ! {
    eprintln!("{diags}");
    std::process::exit(1);
}

struct Loader {
    root: PathBuf,
    cache: HashMap<PathBuf, FileRef>,
}

impl Loader {
    fn new(root: PathBuf) -> Self {
        Self {
            root,
            cache: HashMap::new(),
        }
    }

    fn resolve_path(&self, files: &Files, from: Option<FileRef>, id: &IdentStr) -> PathBuf {
        let path = Path::new(id.as_str());

        if path.is_absolute() {
            return path.to_owned();
        }

        if let Some(from) = from {
            let from = files[from].name();
            return PathBuf::from_iter([from.as_ref(), Path::new(id.as_str())]);
        }

        PathBuf::from_iter([self.root.as_path(), Path::new(id.as_str())])
    }
}

impl stac::Loader for Loader {
    fn create_loader(
        &mut self,
        mut ctx: stac::LoaderCtx<'_>,
        from: Option<FileRef>,
        id: IdentStr,
    ) -> Result<FileRef, impl LoaderFut> {
        let path = self.resolve_path(ctx.files, from, &id);

        if let Some(&file) = self.cache.get(&path) {
            return Ok(file);
        }

        let slot = ctx.reserve_slot();
        self.cache.insert(path.clone(), slot);

        Err(async move {
            let content = load_file_async(&path).await?;
            Ok(LoaderRes {
                file: stac::File::new(path, content),
                slot,
            })
        })
    }
}

fn load_file_async(
    path: &Path,
) -> impl std::future::Future<Output = Result<String, std::io::Error>> + 'static {
    struct FileLoader {
        path: Option<PathBuf>,
        tread_handle: Option<std::thread::JoinHandle<Result<String, std::io::Error>>>,
    }

    impl Future for FileLoader {
        type Output = Result<String, std::io::Error>;

        fn poll(
            mut self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            if let Some(handle) = self.tread_handle.take() {
                if !handle.is_finished() {
                    self.tread_handle = Some(handle);
                    return std::task::Poll::Pending;
                }

                let output = handle.join().unwrap();
                return std::task::Poll::Ready(output);
            }

            if let Some(path) = self.path.take() {
                let waker = cx.waker().clone();
                let handle = std::thread::spawn(move || {
                    let f = std::fs::read_to_string(path);
                    waker.wake();
                    f
                });
                self.tread_handle = Some(handle);
                return std::task::Poll::Pending;
            }

            std::task::Poll::Pending
        }
    }

    FileLoader {
        path: Some(path.to_owned()),
        tread_handle: None,
    }
}

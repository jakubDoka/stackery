use std::{
    collections::HashMap,
    fmt::Display,
    future::Future,
    io,
    path::{Path, PathBuf},
    str::FromStr,
};

use crane_backend::{Emmiter, Generator};
use pollster::FutureExt;

use stac::{
    mini_alloc::{ArenaBase, DiverBase, IdentStr},
    BuiltInType, Diagnostics, FileRef, Files, FuncId, FuncInstances, Instrs, Interpreter,
    LoaderFut, LoaderRes, ModuleLoader, ModuleMeta, Modules, Type,
};
use target_lexicon::PointerWidth;

#[cfg(test)]
mod test;

#[derive(clap::Args)]
pub struct Command {
    #[clap(long, short, default_value = "main.stac")]
    input: PathBuf,
    #[clap(long, short)]
    target: Option<String>,
    #[clap(long, short, default_value = "a")]
    output: String,
    #[clap(long, short = 'O')]
    object_only: bool,
    #[clap(long, short, default_value = "false")]
    dump_ir: bool,
    #[clap(long, short, default_value = "false")]
    run: bool,
}

impl Command {
    pub fn run(self, lp: impl LoaderProvider, stdout: &mut impl std::io::Write) -> Option<()> {
        let target = resolve_target(stdout, self.target)?;
        let arch = resolve_arch(stdout, &target)?;
        let root_name = IdentStr::from_str(self.input.to_str().unwrap());

        let mut diags = Diagnostics::default();
        let mut loader = match lp.provide(self.input) {
            Ok(loader) => loader,
            Err(err) => terminate(stdout, err)?,
        };
        let mut modules = Modules::new();

        let Some(meta) = ModuleLoader::new(&mut modules, &mut diags)
            .update(root_name, &mut loader)
            .block_on()
        else {
            terminate(stdout, diags)?;
        };

        let mut instrs = Instrs::new();

        Self::build_instrs(stdout, &meta, &modules, arch, &mut diags, &mut instrs)?;

        let Some(entry) = instrs.entry_of(meta.root()) else {
            terminate(
                stdout,
                format_args!(
                    "no entry point found, expencted function called \"main\" in \"{}\"",
                    modules.files()[meta.root()].name().display()
                ),
            )?;
        };

        let entry = FuncId {
            module: meta.root(),
            func: entry,
        };
        let mut instances = FuncInstances::default();
        instances.project(stac::Entry {
            id: entry,
            inputs: Vec::new(),
            ip: instrs.func(entry).signature_len,
            ret: Type::BuiltIn(BuiltInType::I32),
        });

        let mut gen = Generator::new(target.clone());

        while let Some((id, entry)) = instances.next() {
            let mut interp = Interpreter::new(arch, &mut instances, &mut diags, &modules, &instrs);
            let ir = interp.eval(&entry);
            if diags.error_count() > 0 {
                terminate(stdout, diags)?;
            }

            for (id, recent) in instances.recent() {
                let emmiter = Emmiter::new(&mut diags, &mut gen, &modules, &instrs, arch, false);
                let sig = emmiter.load_signature(recent);
                gen.declare_func(id, sig);
            }
            instances.clear_recent();

            let emmiter = Emmiter::new(&mut diags, &mut gen, &modules, &instrs, arch, self.dump_ir);
            emmiter.emit(&entry, id, ir);

            if diags.error_count() > 0 {
                terminate(stdout, diags)?;
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

        if self.run {
            let status = std::process::Command::new(format!("./{}", self.output))
                .status()
                .unwrap();

            let severty = if status.success() {
                stac::Severty::Note
            } else {
                stac::Severty::Error
            };

            diags.builder(modules.files()).footer(
                severty,
                format_args!("program exited with status code {}", status.code().unwrap()),
            );
        }

        writeln!(stdout, "{diags}").unwrap();

        Some(())
    }

    fn build_instrs(
        stdout: &mut impl std::io::Write,
        meta: &ModuleMeta,
        modules: &Modules,
        arch: stac::Layout,
        diags: &mut Diagnostics,
        instrs: &mut Instrs,
    ) -> Option<()> {
        let mut arena = ArenaBase::new(1 << 12);
        let mut diver = DiverBase::new(1 << 8);
        let mut resolver = stac::resolver();

        for &module in meta.order() {
            let scope = arena.scope();
            let diver = diver.untyped_dive();
            let parser = stac::Parser::new(modules.files(), module, diags, &scope);
            let Some(ast) = parser.parse(diver) else {
                terminate(stdout, diags)?;
            };

            let builder = stac::InstrBuilder::new(arch, instrs, modules, diags, &mut resolver);
            if builder.build(module, ast).is_none() || diags.error_count() > 0 {
                terminate(stdout, diags)?;
            }
        }

        Some(())
    }
}

fn resolve_target(
    stdout: &mut impl std::io::Write,
    target: Option<String>,
) -> Option<target_lexicon::Triple> {
    let Some(target) = target else {
        return Some(target_lexicon::Triple::host());
    };

    match target_lexicon::Triple::from_str(&target) {
        Ok(target) => Some(target),
        Err(err) => terminate(stdout, format_args!("invalid target: {}", err))?,
    }
}

fn resolve_arch(
    stdout: &mut impl std::io::Write,
    target: &target_lexicon::Triple,
) -> Option<stac::Layout> {
    Some(match target.architecture.pointer_width() {
        Ok(PointerWidth::U16) => stac::Layout::ARCH_16,
        Ok(PointerWidth::U32) => stac::Layout::ARCH_32,
        Ok(PointerWidth::U64) => stac::Layout::ARCH_64,
        Err(()) => terminate(stdout, "target has unsupported pointer width")?,
    })
}

fn terminate(stdout: &mut impl std::io::Write, diags: impl Display) -> Option<!> {
    writeln!(stdout, "{}", diags).unwrap();
    None
}

pub trait LoaderProvider {
    type Loader: stac::Loader;
    fn provide(self, root: PathBuf) -> io::Result<Self::Loader>;
}

pub struct Loader {
    root: PathBuf,
    cache: HashMap<PathBuf, FileRef>,
}

impl Loader {
    fn new(root: PathBuf) -> io::Result<Self> {
        Ok(Self {
            root: root
                .canonicalize()?
                .parent()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "root file has no parent"))?
                .to_owned(),
            cache: HashMap::new(),
        })
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

pub struct DefaultLoaderProvider;

impl LoaderProvider for DefaultLoaderProvider {
    type Loader = Loader;

    fn provide(self, root: PathBuf) -> io::Result<Self::Loader> {
        Loader::new(root)
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

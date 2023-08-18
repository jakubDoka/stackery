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
    BuiltInType, Diagnostics, FileRef, Files, Instances, Instrs, Interpreter, LoaderFut, LoaderRes,
    ModuleLoader, ModuleMeta, Modules, Resolved, Type, Types,
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

        let builtin_names = BuiltInType::ALL.map(|ty| ty.name());
        let mut instrs = Instrs::new(builtin_names);
        let mut types = Types::default();

        Self::build_instrs(stdout, &meta, &modules, &mut diags, &mut instrs)?;

        let Some(entry) = instrs.entry_of(meta.root()) else {
            terminate(
                stdout,
                format_args!(
                    "no entry point found, expencted function called \"main\" in \"{}\"",
                    modules.files()[meta.root()].name().display()
                ),
            )?;
        };

        let mut instances = Instances::new();
        let Resolved::Def(id) = Interpreter::new(
            arch,
            &mut instances,
            &mut diags,
            &modules,
            &instrs,
            &mut types,
        )
        .access_module_decl(meta.root(), entry) else {
            todo!("error handling")
        };

        instances.project_entry(stac::Entry {
            id,
            inputs: Vec::new(),
            ip: instrs.initial_ip_for(id),
            ret: Type::BuiltIn(BuiltInType::I32),
        });

        let mut gen = Generator::new(target.clone());

        while let Some((eid, entry)) = instances.next_entry() {
            let mut interp = Interpreter::new(
                arch,
                &mut instances,
                &mut diags,
                &modules,
                &instrs,
                &mut types,
            );

            let ir = match interp.eval(&entry) {
                stac::EvalResult::Rt(ir) => ir,
                stac::EvalResult::Const(c) if entry.id == id => {
                    diags.builder(modules.files()).footer(
                        stac::Severty::Error,
                        format_args!("entry point must be a function, found constant `{}`", c),
                    );
                    terminate(stdout, diags)?;
                }
                stac::EvalResult::Const(_) => continue,
            };

            if diags.error_count() > 0 {
                terminate(stdout, diags)?;
            }

            for (id, recent) in instances.recent_entries() {
                let emmiter = Emmiter::new(
                    &mut diags, &mut gen, &modules, &instrs, &mut types, arch, false,
                );
                let sig = emmiter.load_signature(recent);
                gen.declare_func(id, sig);
            }

            let emmiter = Emmiter::new(
                &mut diags,
                &mut gen,
                &modules,
                &instrs,
                &mut types,
                arch,
                self.dump_ir,
            );
            emmiter.emit(&entry, eid, ir);

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
                format_args!(
                    "program exited with status code {}",
                    status.code().unwrap_or(1)
                ),
            );
        }

        writeln!(stdout, "{diags}").unwrap();

        Some(())
    }

    fn build_instrs(
        stdout: &mut impl std::io::Write,
        meta: &ModuleMeta,
        modules: &Modules,
        diags: &mut Diagnostics,
        instrs: &mut Instrs,
    ) -> Option<()> {
        let mut arena = ArenaBase::new(1 << 12);
        let mut diver = DiverBase::new(1 << 8);

        for &module in meta.order() {
            let scope = arena.scope();
            let diver = diver.untyped_dive();
            let parser = stac::Parser::new(modules.files(), module, diags, &scope);
            let Some(ast) = parser.parse(diver) else {
                terminate(stdout, diags)?;
            };

            let builder = stac::InstrBuilder::new(instrs, modules, diags);
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

    fn resolve_path(&self, _files: &Files, _rom: Option<FileRef>, id: &IdentStr) -> PathBuf {
        let path = Path::new(id.as_str());

        if path.is_absolute() {
            return path.to_owned();
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
        let path = self
            .resolve_path(ctx.files, from, &id)
            .with_extension("stac");

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
                    let message_path = path.clone();
                    let f = std::fs::read_to_string(path).map_err(|err| {
                        std::io::Error::new(
                            err.kind(),
                            format!("{}: {}", message_path.display(), err),
                        )
                    });
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

use core::fmt;
use std::{
    future::Future,
    io,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{BitSet, Diagnostics, File, FileRef, Files, ShadowStore, Slice, VecStore};
use mini_alloc::{hashbrown::HashMap, IdentStr};

mod loader_impl;

pub type ModuleRefRepr = u16;
pub type ModuleRef = FileRef;
pub type ModuleDeps = Slice<ModuleRef, ModuleRefRepr>;
pub type ModuleShadowStore<T> = ShadowStore<File, ModuleRefRepr, T>;

#[derive(Clone, Default, PartialEq, Eq)]
pub struct Module {
    deps: ModuleDeps,
    name: IdentStr,
}

#[allow(dead_code)]
pub struct ModuleMeta {
    order: Vec<ModuleRef>,
    root: ModuleRef,
    updated_modules: BitSet,
}

#[allow(dead_code)]
impl ModuleMeta {
    pub fn order(&self) -> &[ModuleRef] {
        &self.order
    }

    pub fn root(&self) -> ModuleRef {
        self.root
    }

    pub fn is_updated(&self, module: ModuleRef) -> bool {
        self.updated_modules.contains(module.index())
    }

    pub fn no_changes(&self) -> bool {
        !self.updated_modules.contains(self.root.index())
    }

    pub fn changed(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.order().iter().copied().filter(|&m| self.is_updated(m))
    }
}

#[derive(Default)]
pub struct Modules {
    entities: ModuleShadowStore<Module>,
    deps: VecStore<ModuleRef, ModuleRefRepr>,
    files: Files,
}

impl Modules {
    pub const BUILTIN: ModuleRef = ModuleRef::from_repr(0);
    pub const BUILTIN_NAME: &'static str = "bi";

    pub fn new() -> Self {
        Self::default().init()
    }

    pub fn init(mut self) -> Self {
        let builtin = File::new(PathBuf::from("bi"), String::new());
        assert_eq!(self.files.push(builtin), Self::BUILTIN);
        self
    }

    pub fn name_of(&self, module: ModuleRef) -> &IdentStr {
        &self.entities[module].name
    }

    pub fn files(&self) -> &Files {
        &self.files
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.files().len()
    }

    pub fn deps_of(&self, module_ref: ModuleRef) -> impl Iterator<Item = ModuleRef> + '_ {
        self.deps[self.entities[module_ref].deps].iter().copied()
    }

    pub fn find_dep(&self, on: ModuleRef, ident: &IdentStr) -> Option<crate::Ref<File, u16>> {
        self.deps_of(on).find(|&m| self.name_of(m) == ident)
    }
}

pub struct LoaderCtx<'a> {
    pub files: &'a mut Files,
}

impl<'a> LoaderCtx<'a> {
    pub fn reserve_slot(&mut self) -> FileRef {
        self.files.reserve()
    }
}

pub struct LoaderRes {
    pub file: File,
    pub slot: FileRef,
}

pub trait LoaderFut = Future<Output = io::Result<LoaderRes>> + 'static;

pub trait Loader {
    /// its okay to return cached file even before if gets finalized
    fn create_loader(
        &mut self,
        ctx: LoaderCtx<'_>,
        from: Option<FileRef>,
        id: IdentStr,
    ) -> Result<FileRef, impl LoaderFut>;
}

pub struct ModuleLoader<'a> {
    modules: &'a mut Modules,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(modules: &'a mut Modules, diagnostics: &'a mut Diagnostics) -> Self {
        Self {
            modules,
            diagnostics,
        }
    }
}

pub fn is_visible(
    files: &Files,
    vis: Option<Vis>,
    from: FileRef,
    origin: FileRef,
) -> Result<(), VisError> {
    match vis {
        Some(Vis::Exported) => Ok(()),
        Some(Vis::ModulePrivate) if from == origin => Ok(()),
        None if files[from].package() == files[origin].package() => Ok(()),

        Some(Vis::ModulePrivate) => Err(VisError {
            reason: VisErrorReason::FilePrivate,
            from: files[from].name().clone(),
            origin: files[origin].name().clone(),
        }),
        None => Err(VisError {
            reason: VisErrorReason::NotExported,
            from: files[from].name().clone(),
            origin: files[origin].name().clone(),
        }),
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Vis {
    Exported,
    ModulePrivate,
}

pub struct VisError {
    reason: VisErrorReason,
    from: Arc<Path>,
    origin: Arc<Path>,
}

impl fmt::Display for VisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            reason,
            from,
            origin,
        } = self;

        match reason {
            VisErrorReason::NotExported => write!(
                f,
                "cannot access item in {origin:?} from {from:?}, because it is not exported",
            ),
            VisErrorReason::FilePrivate => write!(
                f,
                "cannot access item in {origin:?} from {from:?}, because it is file-private",
            ),
        }
    }
}

enum VisErrorReason {
    NotExported,
    FilePrivate,
}

pub macro print_cases($test_func:ident: $($name:ident $($arg:expr),*;)*) {
    $crate::print_test::cases! {
        $(
            fn $name(name, ctx) {
                $test_func(name, $($arg)*, ctx);
            }
        )*
    }
}

#[cfg(test)]
pub mod load_test_util {
    use std::future::Future;

    use crate::{FileRef, Loader, LoaderCtx, LoaderFut, ModuleMeta, Modules};
    use mini_alloc::IdentStr;
    use pollster::FutureExt;

    pub macro bail($message:literal $diags:ident $ctx:ident $control:ident) {
        $ctx.push_str($message);
        $ctx.push('\n');
        $ctx.push_str($diags.view());
        $diags.clear();
        $control;
    }

    pub struct DelayedLoaderMock<T> {
        delay_ms: u32,
        inner: T,
    }

    impl<T> DelayedLoaderMock<T> {
        pub fn new(delay_ms: u32, inner: T) -> Self {
            Self { delay_ms, inner }
        }
    }

    impl<T: Loader> Loader for DelayedLoaderMock<T> {
        fn create_loader(
            &mut self,
            ctx: LoaderCtx<'_>,
            from: Option<FileRef>,
            id: IdentStr,
        ) -> Result<FileRef, impl LoaderFut> {
            let delay_ms = self.delay_ms;
            self.inner
                .create_loader(ctx, from, id)
                .map_err(|fut| async move {
                    struct Timer {
                        spawn_time: std::time::Instant,
                        duration: std::time::Duration,
                        started: bool,
                    }
                    impl Timer {
                        fn new(duration: std::time::Duration) -> Self {
                            Self {
                                spawn_time: std::time::Instant::now(),
                                duration,
                                started: false,
                            }
                        }
                    }
                    impl Future for Timer {
                        type Output = ();

                        fn poll(
                            mut self: std::pin::Pin<&mut Self>,
                            cx: &mut std::task::Context<'_>,
                        ) -> std::task::Poll<Self::Output> {
                            if !self.started {
                                self.started = true;
                                let waker = cx.waker().clone();
                                let duration = self.duration;
                                std::thread::spawn(move || {
                                    std::thread::sleep(duration);
                                    waker.wake_by_ref();
                                });
                                return std::task::Poll::Pending;
                            }

                            if self.spawn_time.elapsed() < self.duration {
                                return std::task::Poll::Pending;
                            }

                            std::task::Poll::Ready(())
                        }
                    }
                    Timer::new(std::time::Duration::from_millis(delay_ms as u64)).await;
                    fut.await
                })
        }
    }

    pub fn load_modules(
        mods: &mut Modules,
        mut loader: impl Loader,
        ctx: &mut String,
    ) -> Option<ModuleMeta> {
        let mut diagnostics = crate::Diagnostics::default();

        let root = IdentStr::from_str("root");

        let loader_ctx = crate::ModuleLoader::new(mods, &mut diagnostics);

        let meta = loader_ctx.update(root, &mut loader).block_on();

        if !diagnostics.view().is_empty() {
            ctx.push_str("module loading diagnostics:\n");
            ctx.push_str(diagnostics.view());
        }

        if meta.is_none() {
            ctx.push_str("module loading failed\n");
        }

        meta
    }
}

pub struct LoaderMock<'a> {
    modules: HashMap<&'a str, &'a str>,
    loaded: HashMap<IdentStr, FileRef>,
}

impl<'a> LoaderMock<'a> {
    pub fn new(mut files: &'a str) -> Self {
        let delim = '`';
        let mut modules = HashMap::new();

        if !files.contains(delim) {
            modules.insert("root", files);

            return Self {
                modules,
                loaded: HashMap::new(),
            };
        }

        while let Some((_, rest)) = files.split_once(delim) {
            let (name, rest) = rest.split_once(char::is_whitespace).unwrap();
            let (source, rest) = rest.split_once(delim).unwrap();
            modules.insert(name, source);
            files = rest;
        }

        Self {
            modules,
            loaded: HashMap::new(),
        }
    }

    fn load(
        &mut self,
        mut ctx: crate::LoaderCtx<'_>,
        _: Option<crate::FileRef>,
        id: mini_alloc::IdentStr,
    ) -> Result<crate::FileRef, io::Result<LoaderRes>> {
        if let Some(&file) = self.loaded.get(&id) {
            return Ok(file);
        }

        let id_str = id.as_str();

        let Some(&source) = self.modules.get(id_str) else {
            return Err(Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("module {} not found", id_str),
            )));
        };

        let file = File::new(id.as_str().into(), source.to_owned());
        let slot = ctx.reserve_slot();
        self.loaded.insert(id, slot);

        Err(Ok(LoaderRes { file, slot }))
    }
}

impl<'a> Loader for LoaderMock<'a> {
    fn create_loader(
        &mut self,
        ctx: LoaderCtx<'_>,
        from: Option<FileRef>,
        id: IdentStr,
    ) -> Result<FileRef, impl LoaderFut> {
        self.load(ctx, from, id).map_err(|fut| async move { fut })
    }
}

#[cfg(test)]
mod test {
    use crate::{
        load_modules, print_cases, DelayedLoaderMock, Loader, LoaderMock, ModuleRef, Modules,
    };

    fn perform_test(_: &str, source: &str, ctx: &mut String) {
        let loader = LoaderMock::new(source);
        perform_test_low(loader, ctx);
    }

    fn perfrom_delayed_test(_: &str, source: &str, ctx: &mut String) {
        let delay_ms = 50;
        let now = std::time::Instant::now();
        let loader = DelayedLoaderMock::new(delay_ms, LoaderMock::new(source));
        perform_test_low(loader, ctx);

        let delay = if cfg!(miri) { 1500 } else { 250 };
        ctx.push_str(&format!(
            "in time bounds (<250): {:?}",
            now.elapsed().as_millis() < delay
        ));

        if now.elapsed().as_millis() >= delay {
            ctx.push_str(&format!("{:?}", now.elapsed()));
        }
    }

    fn perform_test_low(loader: impl Loader, ctx: &mut String) {
        let mut modules = crate::Modules::new();

        let meta = load_modules(&mut modules, loader, ctx);

        let Some(meta) = meta else {
            return;
        };

        ctx.push_str("order:\n");
        for &module in &meta.order {
            ctx.push_str(modules.name_of(module));
            ctx.push('\n');
        }

        ctx.push_str("graph:\n");
        fn log_graph(parent: ModuleRef, modules: &Modules, depth: usize, ctx: &mut String) {
            for _ in 0..depth {
                ctx.push_str("  ");
            }

            ctx.push_str(modules.name_of(parent));
            ctx.push('\n');

            let deps = modules.entities[parent].deps;
            for &child in &modules.deps[deps] {
                log_graph(child, modules, depth + 1, ctx);
            }
        }
        log_graph(meta.root, &modules, 0, ctx);
    }

    print_cases! { perfrom_delayed_test:
        delayed_load "
            `root
                a: :{a}
                b: :{b}
                c: :{c}
                d: :{d}
                e: :{e}
            `
            `a :{b}`
            `b `
            `c :{f} :{g}`
            `d :{e}`
            `e `
            `f :{b}`
            `g `
        ";
    }

    print_cases! { perform_test:
        one_file "hello: || \"hello\"";
        cycle "
            `root
                a: :{a}
            `
            `a
                b: :{root}
            `
        ";
        common_dependency "
            `root
                a: :{a}
                b: :{b}
            `
            `a
                common: :{common}
            `
            `b
                common: :{common}
            `
            `common
                c: || \"c\"
            `
        ";
        self_import "root: :{root}";
        nonexistatnt_import "
            `root
                a: :{a}
            `
            `a
                b: :{b}
            `
        ";
        builtin ":{bi}";
    }
}

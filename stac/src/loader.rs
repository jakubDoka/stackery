use core::fmt;
use std::{future::Future, io};

use crate::{BitSet, Diagnostics, File, FileRef, Files, ShadowStore, Slice, VecStore};
use mini_alloc::IdentStr;

mod loader_impl;

pub type ModuleRefRepr = u16;
pub type ModuleRef = FileRef;
pub type ModuleDeps = Slice<ModuleRef, ModuleRefRepr>;
pub type ModuleShadowStore<T> = ShadowStore<File, ModuleRefRepr, T>;

#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct Module {
    deps: ModuleDeps,
}

#[allow(dead_code)]
pub struct ModuleMeta {
    order: Vec<ModuleRef>,
    root: ModuleRef,
    updated_modules: BitSet,
}

#[allow(dead_code)]
impl ModuleMeta {
    pub(crate) fn order(&self) -> &[ModuleRef] {
        &self.order
    }

    pub(crate) fn root(&self) -> ModuleRef {
        self.root
    }

    pub(crate) fn is_updated(&self, module: ModuleRef) -> bool {
        self.updated_modules.contains(module.index())
    }

    pub(crate) fn no_changes(&self) -> bool {
        !self.updated_modules.contains(self.root.index())
    }

    pub(crate) fn changed(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.order().iter().copied().filter(|&m| self.is_updated(m))
    }
}

#[derive(Default)]
pub struct Modules {
    eintities: ModuleShadowStore<Module>,
    deps: VecStore<ModuleRef, ModuleRefRepr>,
    files: Files,
}

impl Modules {
    pub const BUILTIN: ModuleRef = ModuleRef::const_new(0);
    pub const BUILTIN_NAME: &'static str = "bi";

    pub fn new() -> Self {
        Self::default().init()
    }

    pub fn init(mut self) -> Self {
        let builtin = File::new(IdentStr::from_str("bi"), String::new());
        assert_eq!(self.files.push(builtin), Self::BUILTIN);
        self
    }

    pub(crate) fn name_of(&self, module: ModuleRef) -> &IdentStr {
        self.files[module].name()
    }

    pub(crate) fn files(&self) -> &Files {
        &self.files
    }

    #[allow(dead_code)]
    pub(crate) fn len(&self) -> usize {
        self.files().len()
    }

    pub(crate) fn module_file(&self, module: ModuleRef) -> FileRef {
        module // legacy
    }

    pub(crate) fn deps_of(&self, module_ref: ModuleRef) -> impl Iterator<Item = ModuleRef> + '_ {
        self.deps[self.eintities[module_ref].deps].iter().copied()
    }
}

pub struct LoaderCtx<'a> {
    pub files: &'a mut Files,
}

pub trait Loader {
    fn load(
        &mut self,
        ctx: LoaderCtx<'_>,
        from: Option<FileRef>,
        id: IdentStr,
    ) -> impl Future<Output = Result<FileRef, io::Error>> + 'static;
}

pub struct ModuleLoader<'a, L: Loader> {
    loader: &'a mut L,
    modules: &'a mut Modules,
    diagnostics: &'a mut Diagnostics,
}

impl<'a, L: Loader> ModuleLoader<'a, L> {
    pub fn new(
        loader: &'a mut L,
        modules: &'a mut Modules,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            loader,
            modules,
            diagnostics,
        }
    }
}

#[cfg(test)]
pub mod test_util {
    use std::{collections::HashMap, future::Future};

    use crate::{File, FileRef, Loader, LoaderCtx};
    use mini_alloc::IdentStr;

    #[macro_export]
    macro_rules! print_cases {
        ($test_func:ident: $($name:ident $($arg:expr),*;)*) => {
            print_test::cases! {
                $(
                    fn $name(ctx) {
                        $test_func($($arg)*, ctx);
                    }
                )*
            }
        };
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
        fn load(
            &mut self,
            ctx: LoaderCtx<'_>,
            from: Option<FileRef>,
            id: IdentStr,
        ) -> impl Future<Output = Result<FileRef, std::io::Error>> + 'static {
            let delay_ms = self.delay_ms;
            let fut = self.inner.load(ctx, from, id);
            async move {
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
            }
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
            ctx: crate::LoaderCtx<'_>,
            _: Option<crate::FileRef>,
            id: mini_alloc::IdentStr,
        ) -> Result<crate::FileRef, std::io::Error> {
            if let Some(&file) = self.loaded.get(&id) {
                return Ok(file);
            }

            let id_str = id.as_str();

            let &source = self.modules.get(id_str).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("module {} not found", id_str),
                )
            })?;

            let file = File::new(id.clone(), source.to_owned());
            let file = ctx.files.push(file);

            self.loaded.insert(id, file);

            Ok(file)
        }
    }

    impl<'a> Loader for LoaderMock<'a> {
        fn load(
            &mut self,
            ctx: crate::LoaderCtx<'_>,
            f: Option<crate::FileRef>,
            id: mini_alloc::IdentStr,
        ) -> impl Future<Output = Result<crate::FileRef, std::io::Error>> {
            let res = self.load(ctx, f, id);
            async move { res }
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
    from: IdentStr,
    origin: IdentStr,
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
                "cannot access item in {origin} from {from}, because it is not exported",
            ),
            VisErrorReason::FilePrivate => write!(
                f,
                "cannot access item in {origin} from {from}, because it is file-private",
            ),
        }
    }
}

enum VisErrorReason {
    NotExported,
    FilePrivate,
}

#[cfg(test)]
mod test {
    use mini_alloc::IdentStr;
    use pollster::FutureExt;

    use crate::{print_cases, DelayedLoaderMock, Loader, LoaderMock, ModuleRef, Modules};

    fn perform_test(source: &str, ctx: &mut String) {
        let loader = LoaderMock::new(source);
        perform_test_low(loader, ctx);
    }

    fn pefrom_delayed_test(source: &str, ctx: &mut String, delay_ms: u32) {
        let loader = DelayedLoaderMock::new(delay_ms, LoaderMock::new(source));
        perform_test_low(loader, ctx);
    }

    fn perform_test_low<L: Loader>(mut loader: L, ctx: &mut String) {
        let mut diagnostics = crate::Diagnostics::default();
        let mut modules = crate::Modules::new();

        let root = IdentStr::from_str("root");

        let loader_ctx = crate::ModuleLoader::new(&mut loader, &mut modules, &mut diagnostics);

        let meta = loader_ctx.update(root).block_on();

        if !diagnostics.diagnostic_view().is_empty() {
            ctx.push_str("diagnostics:\n");
            ctx.push_str(diagnostics.diagnostic_view());
        }

        let Some(meta) = meta else {
            ctx.push_str("no meta");
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

            let deps = modules.eintities[parent].deps;
            for &child in &modules.deps[deps] {
                log_graph(child, modules, depth + 1, ctx);
            }
        }
        log_graph(meta.root, &modules, 0, ctx);
    }

    #[test]
    fn test() {
        print_test::case("delayed::loading", |ctx| {
            let now = std::time::Instant::now();
            pefrom_delayed_test(
                "
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
                ",
                ctx,
                50,
            );
            let delay = if cfg!(miri) { 1500 } else { 250 };
            ctx.push_str(&format!(
                "in time bounds (<250): {:?}",
                now.elapsed().as_millis() < delay
            ));

            if now.elapsed().as_millis() >= delay {
                ctx.push_str(&format!("{:?}", now.elapsed()));
            }
        })
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

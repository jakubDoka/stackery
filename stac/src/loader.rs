use core::fmt;
use std::{future::Future, io};

use crate::{BitSet, Diagnostics, File, FileRef, Files, ShadowStore, Slice, VecStore};
use mini_alloc::IdentStr;

mod loader_impl;

pub type ModuleRefRepr = u16;
pub type ModuleRef = FileRef;
pub type ModuleDeps = Slice<ModuleRef, ModuleRefRepr>;

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
    eintities: ShadowStore<File, ModuleRefRepr, Module>,
    deps: VecStore<ModuleRef, ModuleRefRepr>,
    files: Files,
}

impl Modules {
    pub(crate) fn name_of(&self, module: ModuleRef) -> &IdentStr {
        self.files[module].name()
    }

    pub(crate) fn files(&self) -> &Files {
        &self.files
    }

    pub(crate) fn preserved(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.files()
            .iter()
            .filter_map(|(id, module)| (!module.is_dirty()).then_some(id))
    }

    #[allow(dead_code)]
    pub(crate) fn len(&self) -> usize {
        self.files().len()
    }

    pub(crate) fn changed(&self) -> impl Iterator<Item = ModuleRef> + '_ {
        self.files().iter().filter_map(|(id, module)| {
            let is_changed = module.is_dirty();
            is_changed.then_some(id)
        })
    }

    pub(crate) fn module_file(&self, module: ModuleRef) -> FileRef {
        module // legacy
    }

    pub(crate) fn deps_of(
        &self,
        module: ModuleRef,
    ) -> impl Iterator<Item = (IdentStr, ModuleRef)> + '_ {
        let module = &self.eintities[module];
        self.deps[module.deps]
            .iter()
            .map(move |&m| (self.name_of(m).clone(), m))
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

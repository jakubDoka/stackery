use std::{collections::VecDeque, future::Future, io, pin::Pin};

use crate::{
    BitSet, CycleDetector, FileRef, Graph, ImportLexer, Loader, LoaderCtx, LoaderRes, Module,
    ModuleLoader, ModuleMeta, ModuleRef, Modules, PoolStore, Severty,
};
use mini_alloc::IdentStr;

#[derive(Default)]
struct CacheLoaderRes {
    import_stack: Vec<(IdentStr, usize)>,
    module_stack: Vec<ModuleRef>,
    reached_modules: BitSet,
}

impl<'a> ModuleLoader<'a> {
    pub async fn update(mut self, root: IdentStr, loader: &mut impl Loader) -> Option<ModuleMeta> {
        let mut res = CacheLoaderRes::default();

        let root = self.load_modules(&mut res, root, loader).await?;
        let mut order = self.detect_cycles(root)?;
        self.remove_ignored(&mut order);
        self.remove_unreachable(&res);
        let updated_modules = self.mark_updated(&order);

        Some(ModuleMeta {
            order,
            root,
            updated_modules,
        })
    }

    fn remove_ignored(&mut self, order: &mut Vec<ModuleRef>) {
        order.retain(|&module| module != Modules::BUILTIN);
    }

    fn remove_unreachable(&mut self, res: &CacheLoaderRes) {
        self.modules
            .entities
            .retain(|id, _| res.reached_modules.contains(id.index()));

        let mut module_ranges = self
            .modules
            .entities
            .values_mut()
            .map(|m| &mut m.deps)
            .collect::<Vec<_>>();

        module_ranges.sort_unstable();

        self.modules.deps.preserve_chunks(module_ranges);
    }

    fn mark_updated(&mut self, order: &[ModuleRef]) -> BitSet {
        let mut updated_set = BitSet::with_capacity(self.modules.files().len());

        for &module in order {
            let module_ent = &self.modules.entities[module];

            let updated = self.modules.files[module].is_dirty();
            let indirectly_updated = self.modules.deps[module_ent.deps]
                .iter()
                .any(|&dep| updated_set.contains(dep.index()));

            if updated || indirectly_updated {
                updated_set.insert(module.index());
            }
        }

        updated_set
    }

    async fn load_modules(
        &mut self,
        res: &mut CacheLoaderRes,
        root_name: IdentStr,
        loader: &mut impl Loader,
    ) -> Option<ModuleRef> {
        let root = match self.load_file(ImportOrigin::Root, root_name.clone(), loader) {
            Ok(root) => root,
            Err(fut) => self.handle_file_load_result(LoadRes::Loaded(fut.await))?.0,
        };

        let mut queue = Queue::new();
        self.add_module(res, root, root_name, &mut queue, loader);
        let mut last_from = root;

        while let Some(lf) = Pin::new(&mut queue).await {
            let Some((file, ImportOrigin::File { file: from, .. }, name)) =
                self.handle_file_load_result(lf)
            else {
                continue;
            };

            self.add_module(res, file, name, &mut queue, loader);

            if last_from != from {
                let elems = res.module_stack.drain(..);
                self.modules.entities[last_from].deps = self.modules.deps.extend(elems);
                last_from = from;
            }

            res.module_stack.push(file);
        }

        let elems = res.module_stack.drain(..);
        self.modules.entities[last_from].deps = self.modules.deps.extend(elems);

        Some(root)
    }

    fn load_file(
        &mut self,
        from: ImportOrigin,
        name: IdentStr,
        loader: &mut impl Loader,
    ) -> Result<FileRef, impl Future<Output = LoadedFile> + 'static> {
        let ctx = LoaderCtx {
            files: &mut self.modules.files,
        };
        let from_file = from.file();
        match loader.create_loader(ctx, from_file, name.clone()) {
            Ok(id) => Ok(id),
            Err(fut) => Err(async move {
                let file = fut.await;
                LoadedFile { file, from, name }
            }),
        }
    }

    fn handle_file_load_result(
        &mut self,
        res: LoadRes,
    ) -> Option<(FileRef, ImportOrigin, IdentStr)> {
        let (fref, from, name) = match res {
            LoadRes::Cached(cached) => {
                let CachedFile { id, from, name } = cached;
                (Ok(id), from, name)
            }
            LoadRes::Loaded(loaded) => {
                let LoadedFile { file, from, name } = loaded;
                let fref = file.map(|res| self.modules.files.fill_reserved(res.slot, res.file));
                (fref, from, name)
            }
        };

        match fref {
            Ok(loaded_file) => Some((loaded_file, from, name)),
            Err(err) if let ImportOrigin::File { file, pos } = from => {
                let span = self.modules.files()[file].span_for_offset(pos, file);
                self.diagnostics
                    .builder(&self.modules.files)
                    .footer(Severty::Error, "unable to load a module")
                    .footer(Severty::Note, format_args!("loader error: {err}"))
                    .annotation(Severty::Error, span, "imported here")
                    .terminate()?;
            }
            Err(err) => {
                self.diagnostics
                    .builder(&self.modules.files)
                    .footer(Severty::Error, "unable to load a root module")
                    .footer(Severty::Note, format_args!("loader error: {err}"))
                    .footer(Severty::Note, format_args!("the import string is {}", &name))
                    .terminate()?;
            }
        }
    }

    fn detect_cycles(&mut self, root_module: ModuleRef) -> Option<Vec<ModuleRef>> {
        let mut graph = Graph::new();
        for module in self.modules.entities.values() {
            let deps = self.modules.deps[module.deps]
                .iter()
                .map(|&dep| dep.index());

            graph.add_node(deps);
        }

        let mut detector = CycleDetector::new();
        if let Some(cycle) = detector.detect(root_module.index(), &graph) {
            let mut builder = self
                .diagnostics
                .builder(&self.modules.files)
                .footer(Severty::Error, "cyclic dependency detected")
                .footer(
                    Severty::Note,
                    "the following modules are involved in the cycle:",
                );

            for &module in cycle.iter().rev() {
                let name = PoolStore::by_index(self.modules.files(), module).name();
                builder = builder.footer(Severty::Error, format_args!(" -> {name:?}"));
            }

            builder.terminate()?;
        }

        Some(
            detector
                .order()
                .iter()
                .map(|&index| PoolStore::index_to_ref(self.modules.files(), index))
                .collect(),
        )
    }

    fn add_module<'b>(
        &mut self,
        res: &mut CacheLoaderRes,
        file: FileRef,
        name: IdentStr,
        queue: &mut Queue<LoadRes>,
        loader: &mut impl Loader,
    ) {
        if res.reached_modules.insert(file.index()) {
            self.modules.entities[file] = Module {
                name,
                ..Default::default()
            };
            self.collect_imports(file, res, queue, loader);
        }
    }

    fn collect_imports<'b>(
        &mut self,
        file: ModuleRef,
        res: &mut CacheLoaderRes,
        queue: &mut Queue<LoadRes>,
        loader: &mut impl Loader,
    ) {
        let source = self.modules.files[file].source();
        ImportLexer::new(&source)
            .map(|(import, start)| (IdentStr::from_str(import), start))
            .collect_into(&mut res.import_stack);

        res.import_stack.sort_unstable();
        res.import_stack.dedup_by(|a, b| a.0 == b.0);

        for (name, pos) in res.import_stack.drain(..) {
            let from = ImportOrigin::File { file, pos };

            if name.as_str() == Modules::BUILTIN_NAME {
                let id = Modules::BUILTIN;
                queue.push_ready(LoadRes::Cached(CachedFile { id, from, name }));
                continue;
            }

            match self.load_file(from, name.clone(), loader) {
                Ok(id) => queue.push_ready(LoadRes::Cached(CachedFile { id, from, name })),
                Err(fut) => queue.push(async move { LoadRes::Loaded(fut.await) }),
            }
        }
    }
}

struct CachedFile {
    id: FileRef,
    from: ImportOrigin,
    name: IdentStr,
}

struct LoadedFile {
    file: io::Result<LoaderRes>,
    from: ImportOrigin,
    name: IdentStr,
}

enum LoadRes {
    Loaded(LoadedFile),
    Cached(CachedFile),
}

#[derive(Clone, Copy)]
enum ImportOrigin {
    File { file: FileRef, pos: usize },
    Root,
}

impl ImportOrigin {
    fn file(&self) -> Option<FileRef> {
        match self {
            ImportOrigin::File { file, .. } => Some(*file),
            ImportOrigin::Root => None,
        }
    }
}

type QueueFut<T> = Pin<Box<dyn Future<Output = T>>>;

enum QueueItem<T> {
    Pending(QueueFut<T>),
    Ready(T),
}

struct Queue<T> {
    queue: VecDeque<QueueItem<T>>,
}

impl<T> Queue<T> {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    fn push<F: Future<Output = T> + 'static>(&mut self, fut: F) {
        self.queue.push_back(QueueItem::Pending(Box::pin(fut)));
    }

    fn push_ready(&mut self, builtin: T) {
        self.queue.push_back(QueueItem::Ready(builtin));
    }
}

impl<T: Unpin> Future for Queue<T> {
    type Output = Option<T>;

    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        for file in self.queue.iter_mut() {
            let QueueItem::Pending(fut) = file else {
                continue;
            };

            match fut.as_mut().poll(cx) {
                std::task::Poll::Ready(res) => {
                    *file = QueueItem::Ready(res);
                }
                std::task::Poll::Pending => {}
            }
        }

        let Some(item) = self.queue.pop_front() else {
            return std::task::Poll::Ready(None);
        };

        let QueueItem::Ready(res) = item else {
            self.queue.push_front(item);
            return std::task::Poll::Pending;
        };

        std::task::Poll::Ready(Some(res))
    }
}

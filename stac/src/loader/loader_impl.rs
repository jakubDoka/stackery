use std::{collections::VecDeque, future::Future, io, pin::Pin};

use crate::{
    BitSet, CycleDetector, FileRef, Graph, ImportLexer, Loader, LoaderCtx, Module, ModuleLoader,
    ModuleMeta, ModuleRef, PoolStore, Severty,
};
use mini_alloc::{hashbrown, InternedStr};

#[derive(Default)]
struct CacheLoaderRes {
    import_stack: Vec<(InternedStr, usize)>,
    module_stack: Vec<ModuleRef>,
    reached_modules: BitSet,
}

impl<'a, L: Loader> ModuleLoader<'a, L> {
    pub async fn update(mut self, root: InternedStr) -> Option<ModuleMeta> {
        let mut res = CacheLoaderRes::default();

        let root = self.load_modules(&mut res, root).await?;
        let order = self.detect_cycles(root)?;
        self.remove_unreachable(&res);
        let updated_modules = self.mark_updated(&order);

        Some(ModuleMeta {
            order,
            root,
            updated_modules,
        })
    }

    fn remove_unreachable(&mut self, res: &CacheLoaderRes) {
        self.modules
            .eintities
            .retain(|id, _| res.reached_modules.contains(id.index()));

        let mut module_ranges = self
            .modules
            .eintities
            .values_mut()
            .map(|m| &mut m.deps)
            .collect::<Vec<_>>();

        module_ranges.sort_unstable();

        self.modules.deps.preserve_chunks(module_ranges);
    }

    fn mark_updated(&mut self, order: &[ModuleRef]) -> BitSet {
        let mut updated_set = BitSet::with_capacity(self.modules.eintities.len());

        for &module in order {
            let module_ent = &self.modules.eintities[module];

            let updated = self.modules.files[module_ent.file].is_dirty();
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
        root: InternedStr,
    ) -> Option<ModuleRef> {
        let loaded = self.load_file(None, root).await;
        let root = self.handle_file_load_result(loaded)?;

        let mut queue = ParallelFileLoader::new();
        let root_module = self.add_module(res, root, &mut queue);
        let mut last_from = root_module;

        while let Some(lf) = Pin::new(&mut queue).await && let Some((from, _)) = lf.from {
            let Some(file) = self.handle_file_load_result(lf) else {
                continue;
            };

            let module = self.add_module(res, file, &mut queue);

            if last_from != from {
                let elems = res.module_stack.drain(..);
                self.modules.eintities[last_from].deps = self.modules.deps.extend(elems);
                last_from = from;
            }

            res.module_stack.push(module);
        }

        let elems = res.module_stack.drain(..);
        self.modules.eintities[last_from].deps = self.modules.deps.extend(elems);

        Some(root_module)
    }

    fn load_file(
        &mut self,
        file_info: Option<(ModuleRef, usize)>,
        import: InternedStr,
    ) -> impl Future<Output = LoadedFile> + 'static {
        let ctx = LoaderCtx {
            files: &mut self.modules.files,
        };
        let from_file = file_info.map(|(f, ..)| self.modules.eintities[f].file);
        let fut = self.loader.load(ctx, from_file, import.clone());
        async move {
            LoadedFile {
                id: fut.await,
                from: file_info,
                name: import,
            }
        }
    }

    fn handle_file_load_result(
        &mut self,
        LoadedFile { id, from, name }: LoadedFile,
    ) -> Option<FileRef> {
        match id {
            Ok(id) => Some(id),
            Err(err) if let Some((from_module, pos)) = from => {
                let from_file = self.modules.eintities[from_module].file;
                let span = self.modules.files.span_for_offset(pos, from_file);
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
        for opt_module in self.modules.eintities.unfiltered_values() {
            let Some(module) = opt_module else {
                graph.add_node([]);
                continue;
            };

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
                let file = PoolStore::by_index(&self.modules.eintities, module).file;
                let name = self.modules.files[file].name();
                let name_str = name.as_str();
                builder = builder.footer(Severty::Error, format_args!(" -> {}", name_str));
            }

            builder.terminate()?;
        }

        Some(
            detector
                .order()
                .iter()
                .map(|&index| PoolStore::index_to_ref(&self.modules.eintities, index))
                .collect(),
        )
    }

    fn add_module<'b>(
        &mut self,
        res: &mut CacheLoaderRes,
        file: FileRef,
        queue: &mut ParallelFileLoader,
    ) -> ModuleRef {
        let module = match self.modules.loaded_modules.entry(file) {
            hashbrown::hash_map::Entry::Occupied(entry) => *entry.get(),
            hashbrown::hash_map::Entry::Vacant(entry) => {
                let module = Module::new(file);
                let module = self.modules.eintities.push(module);
                entry.insert(module);
                self.collect_imports(module, res, queue);

                module
            }
        };

        res.reached_modules.insert(module.index());

        module
    }

    fn collect_imports<'b>(
        &mut self,
        from: ModuleRef,
        res: &mut CacheLoaderRes,
        queue: &mut ParallelFileLoader,
    ) {
        let file = self.modules.eintities[from].file;
        let source = self.modules.files[file].source();
        ImportLexer::new(&source)
            .map(|(import, start)| (InternedStr::from_str(import), start))
            .collect_into(&mut res.import_stack);

        res.import_stack.sort_unstable();
        res.import_stack.dedup_by(|a, b| a.0 == b.0);

        for (name, pos) in res.import_stack.drain(..) {
            let fut = self.load_file(Some((from, pos)), name);
            queue.push(fut);
        }
    }
}

struct LoadedFile {
    id: Result<FileRef, io::Error>,
    from: Option<(ModuleRef, usize)>,
    name: InternedStr,
}

type QueueFut = Pin<Box<dyn Future<Output = LoadedFile>>>;

enum QueueItem {
    Pending(QueueFut),
    Ready(LoadedFile),
}

struct ParallelFileLoader {
    queue: VecDeque<QueueItem>,
}

impl ParallelFileLoader {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    fn push<F: Future<Output = LoadedFile> + 'static>(&mut self, fut: F) {
        self.queue.push_back(QueueItem::Pending(Box::pin(fut)));
    }
}

impl Future for ParallelFileLoader {
    type Output = Option<LoadedFile>;

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

#[cfg(test)]
mod test {
    use mini_alloc::InternedStr;
    use pollster::FutureExt;

    use crate::{DelayedLoaderMock, Loader, LoaderMock, ModuleRef, Modules};

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
        let mut modules = crate::Modules::default();

        let root = InternedStr::from_str("root");

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

    macro_rules! cases {
        ($(
            $name:ident $sources:literal
        )*) => {print_test::cases! {$(
            fn $name(ctx) {
                perform_test($sources, ctx);
            }
        )*}};
    }

    cases! {
        one_file "`root
            hello: || \"hello\"
        `"
        cycle "
            `root
                a: :{a}
            `
            `a
                b: :{root}
            `
        "
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
        "
        self_import "`root
                root: :{root}
        `"
        nonexistatnt_import "
            `root
                a: :{a}
            `
            `a
                b: :{b}
            `
        "
    }
}

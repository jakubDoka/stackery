use crate::*;
use mini_alloc::*;

#[derive(Default)]
struct CacheLoaderRes {
    import_stack: Vec<(ModuleRef, InternedStr, usize)>,
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
        let root = self.load_file(None, root).await?;
        let root_module = self.add_module(res, root);

        let mut last_from = root_module;
        let mut cursor = 0;
        while let Some(&(from, import, pos)) = res.import_stack.get(cursor) {
            cursor += 1;

            let from_file = self.modules.eintities[from].file;
            let Some(file) = self.load_file(Some((from_file, pos)), import).await else {
                continue;
            };

            let module = self.add_module(res, file);

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

    async fn load_file(
        &mut self,
        file_info: Option<(FileRef, usize)>,
        import: InternedStr,
    ) -> Option<FileRef> {
        let ctx = LoaderCtx {
            files: &mut self.modules.files,
            interner: &self.interner,
        };
        match self.loader.load(ctx, file_info.map(|(f, ..)| f), import).await {
            Ok(id) => Some(id),
            Err(err) if let Some((from_file, pos)) = file_info => {
                let span = self.modules.files.span_for_pos(pos, from_file);
                self.diagnostics
                    .builder(&self.modules.files, self.interner)
                    .footer(Severty::Error, "unable to load a module")
                    .footer(Severty::Note, format_args!("loader error: {err}"))
                    .annotation(Severty::Error, span, "imported here")
                    .terminate()?;
            }
            Err(err) => {
                self.diagnostics
                    .builder(&self.modules.files, self.interner)
                    .footer(Severty::Error, "unable to load a root module")
                    .footer(Severty::Note, format_args!("loader error: {err}"))
                    .footer(Severty::Note, format_args!("the import string is {}", &self.interner[import]))
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
                .map(|&dep| dep.index() as u32);

            graph.add_node(deps);
        }

        let mut detector = CycleDetector::new();
        if let Some(cycle) = detector.detect(root_module.index() as u32, &graph) {
            let mut builder = self
                .diagnostics
                .builder(&self.modules.files, self.interner)
                .footer(Severty::Error, "cyclic dependency detected")
                .footer(
                    Severty::Note,
                    "the following modules are involved in the cycle:",
                );

            for &module in cycle.iter().rev() {
                let file = PoolStore::by_index(&self.modules.eintities, module as usize).file;
                let name = self.modules.files[file].name();
                let name_str = &self.interner[name];
                builder = builder.footer(Severty::Error, format_args!(" -> {}", name_str));
            }

            builder.terminate()?;
        }

        Some(
            detector
                .order()
                .iter()
                .map(|&index| PoolStore::index_to_ref(&self.modules.eintities, index as usize))
                .collect(),
        )
    }

    fn add_module(&mut self, res: &mut CacheLoaderRes, file: FileRef) -> ModuleRef {
        // let &mut module = self.modules.loaded_modules.entry(file).or_insert_with(|| {
        //     let module = Module::new(file);
        //     let module = self.modules.eintities.push(module);
        //     self.collect_imports(res, module);
        //     res.module_stack.push(module);
        //     module
        // });
        // module
        //
        let module = match self.modules.loaded_modules.entry(file) {
            hashbrown::hash_map::Entry::Occupied(entry) => *entry.get(),
            hashbrown::hash_map::Entry::Vacant(entry) => {
                let module = Module::new(file);
                let module = self.modules.eintities.push(module);
                entry.insert(module);
                self.collect_imports(res, module);

                module
            }
        };

        res.reached_modules.insert(module.index());

        module
    }

    fn collect_imports(&mut self, res: &mut CacheLoaderRes, from: ModuleRef) {
        let prev_len = res.import_stack.len();

        let file = self.modules.eintities[from].file;
        let source = self.modules.files[file].source();
        let imports = ImportLexer::new(&source)
            .map(|(import, start)| (from, self.interner.intern(import), start));

        res.import_stack.extend(imports);
        res.import_stack[prev_len..].sort_unstable();
        let (uniques, _) =
            res.import_stack[prev_len..].partition_dedup_by_key(|(_, import, _)| *import);
        let unique_len = uniques.len();
        res.import_stack.truncate(prev_len + unique_len);
    }
}

#[cfg(test)]
mod test {
    use mini_alloc::StrInterner;
    use pollster::FutureExt;

    use crate::loader::test_util::LoaderMock;
    use crate::{ModuleRef, Modules};

    fn perform_test(sources: &str, ctx: &mut String) {
        let mut loader = LoaderMock::new(sources);

        let mut diagnostics = crate::Diagnostics::default();
        let mut modules = crate::Modules::default();
        let interner = mini_alloc::StrInterner::default();

        let root = interner.intern("root");

        let loader_ctx =
            crate::ModuleLoader::new(&mut loader, &mut modules, &interner, &mut diagnostics);

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
            ctx.push_str(&interner[modules.name_of(module)]);
            ctx.push('\n');
        }

        ctx.push_str("graph:\n");
        fn log_graph(
            parent: ModuleRef,
            interner: &StrInterner,
            modules: &Modules,
            depth: usize,
            ctx: &mut String,
        ) {
            for _ in 0..depth {
                ctx.push_str("  ");
            }

            ctx.push_str(&interner[modules.name_of(parent)]);
            ctx.push('\n');

            let deps = modules.eintities[parent].deps;
            for &child in &modules.deps[deps] {
                log_graph(child, interner, modules, depth + 1, ctx);
            }
        }
        log_graph(meta.root, &interner, &modules, 0, ctx);
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

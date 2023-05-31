use mini_alloc::InternedStr;

use crate::*;

#[derive(Default)]
struct CacheLoaderRes {
    import_stack: Vec<(ModuleRef, InternedStr, usize)>,
    module_stack: Vec<ModuleRef>,
}

impl<'a, L: Loader> CacheLoader<'a, L> {
    pub async fn update(&mut self, root: File) -> Option<ModuleRef> {
        let mut res = CacheLoaderRes::default();

        let (root, order) = self.load_modules(&mut res, root).await?;

        Some(root)
    }

    async fn load_modules(
        &mut self,
        res: &mut CacheLoaderRes,
        root: File,
    ) -> Option<(ModuleRef, Vec<ModuleRef>)> {
        let root = self.modules.files.add(root);
        let root_module = self.add_module(res, root);

        let mut last_from = root_module;
        let mut cursor = 0;
        while let Some(&(from, import, pos)) = res.import_stack.get(cursor) {
            cursor += 1;

            let from_file = self.modules.eintities[from].file;
            let Some(file) = self.load_file(Some(from_file), import, pos).await else {
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

        let order = self.detect_cycles(root_module)?;

        Some((root_module, order))
    }

    async fn load_file(
        &mut self,
        from_file: Option<FileRef>,
        import: InternedStr,
        pos: usize,
    ) -> Option<FileRef> {
        let ctx = LoaderCtx {
            files: &mut self.modules.files,
            interner: &self.interner,
        };
        match self.loader.load(ctx, from_file, import).await {
            Ok(id) => Some(id),
            Err(err) if let Some(from_file) = from_file => {
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

            let deps = self
                .modules
                .deps
                .view(module.deps)
                .as_slice()
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
                let file = CachePool::by_index(&self.modules.eintities, module as usize).file;
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
                .map(|&index| CachePool::index_to_ref(&self.modules.eintities, index as usize))
                .collect(),
        )
    }

    fn add_module(&mut self, res: &mut CacheLoaderRes, file: FileRef) -> CacheRef<Module> {
        let &mut module = self.modules.loaded_modules.entry(file).or_insert_with(|| {
            let module = Module::new(file);
            self.modules.eintities.push(module)
        });
        self.modules.loaded_modules.insert(file, module);
        self.collect_imports(res, module);
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

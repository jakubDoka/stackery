use std::{
    collections::HashMap,
    fmt::Display,
    mem,
    ops::{Index, IndexMut},
};

use scoped_arena::Scope;

use crate::{Errors, Parser, ParserContext};

pub trait LoaderSource {
    async fn load(&mut self, id: &str) -> Result<String, impl Display>;
}

#[derive(Default)]
pub struct RootSource {
    all_deps: HashMap<String, SourceId>,
    sources: Sources,
    root: Option<SourceId>,
}

impl RootSource {
    pub async fn reload(
        &mut self,
        code: &str,
        scope: &Scope<'_>,
        errors: &mut Errors,
        loader: &mut impl LoaderSource,
    ) {
        let &mut root = self
            .root
            .get_or_insert_with(|| self.sources.push(Source::default()));
        self.sources[root].code = code.to_string();
        self.sources[root].deps.clear();

        let mut stack = vec![root];

        while let Some(id) = stack.pop() {
            let mut source = mem::take(&mut self.sources[id]);
            let parser = Parser::new(scope, &source.code, None, errors);
            let (imports, ctx) = parser.imports();

            for import in imports {
                if let Some(dep) = self.all_deps.get(import.id) {
                    source.deps.insert(import.id.to_string(), *dep);
                    continue;
                }

                let code = match loader.load(&import.id).await {
                    Ok(code) => code,
                    Err(err) => {
                        errors
                            .error(import.alias.span.into())
                            .message("failed to load script")
                            .context(err);
                        continue;
                    }
                };

                let dep = self.sources.push(Source {
                    code,
                    ..Default::default()
                });

                self.sources[id].deps.insert(import.id.to_string(), dep);
                stack.push(dep);
            }

            source.code_start = ctx;
            self.sources[id] = source;
        }

        self.mark_and_sweep();
    }

    fn mark_and_sweep(&mut self) {
        self.sources
            .values_mut()
            .for_each(|source| source.marked = false);

        let mut stack = vec![self.root.unwrap()];

        while let Some(id) = stack.pop() {
            let source = &mut self.sources[id];
            if mem::replace(&mut source.marked, true) {
                continue;
            }
            stack.extend(source.deps.values());
        }

        self.sources.retain(|source| source.marked);
    }
}

#[derive(Default)]
pub struct Source {
    deps: HashMap<String, SourceId>,
    code_start: ParserContext,
    code: String,
    marked: bool,
}

impl Source {
    pub fn deps(&self) -> impl Iterator<Item = (&str, SourceId)> + '_ {
        self.deps.iter().map(|(id, dep)| (id.as_str(), *dep))
    }

    pub fn source(&self) -> &str {
        &self.code
    }

    pub fn code_start(&self) -> ParserContext {
        self.code_start.clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceId(usize);

impl SourceId {
    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Default)]
struct Sources {
    sources: Vec<Option<Source>>,
    free: Vec<SourceId>,
}

impl Sources {
    fn push(&mut self, source: Source) -> SourceId {
        match self.free.pop() {
            Some(id) => {
                self.sources[id.0] = Some(source);
                id
            }
            None => {
                let id = SourceId(self.sources.len());
                self.sources.push(Some(source));
                id
            }
        }
    }

    fn values_mut(&mut self) -> impl Iterator<Item = &mut Source> {
        self.sources.iter_mut().flatten()
    }

    fn retain(&mut self, mut f: impl FnMut(&mut Source) -> bool) {
        for source_opt in self.sources.iter_mut() {
            if let Some(source) = source_opt.as_mut() && !f(source) {
                *source_opt = None;
            }
        }
    }
}

impl Index<SourceId> for Sources {
    type Output = Source;

    fn index(&self, id: SourceId) -> &Self::Output {
        self.sources[id.0].as_ref().unwrap()
    }
}

impl IndexMut<SourceId> for Sources {
    fn index_mut(&mut self, id: SourceId) -> &mut Self::Output {
        self.sources[id.0].as_mut().unwrap()
    }
}

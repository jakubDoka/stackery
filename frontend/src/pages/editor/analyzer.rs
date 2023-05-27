use dioxus::prelude::UseState;
use stac::*;

use crate::api;

pub struct Analyzer {
    //arena_base: ArenaBase,
    root: RootSource,
    errors: Errors,
}

impl Default for Analyzer {
    fn default() -> Self {
        Self {
            //arena_base: ArenaBase::default(),
            root: RootSource::default(),
            errors: Errors::default(),
        }
    }
}

impl Analyzer {
    pub async fn check(&mut self, source: String, status: UseState<String>) {
        todo!()
    }

    pub fn errors(&mut self) -> impl Iterator<Item = Error> + '_ {
        self.errors.drain_errors()
    }
}

struct LoaderImpl(UseState<String>);

impl LoaderSource for LoaderImpl {
    async fn load(&mut self, id: &str) -> Result<String, impl std::fmt::Display> {
        self.0.set(format!("loading: {}", id));
        api::posts::get_code(id).await
    }
}

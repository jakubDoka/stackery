use dioxus::prelude::UseState;
use stac::*;

use crate::api;

pub struct Analyzer {
    scope: scoped_arena::Scope<'static>,
    root: RootSource,
    errors: Errors,
}

impl Default for Analyzer {
    fn default() -> Self {
        Self {
            scope: scoped_arena::Scope::new(),
            root: RootSource::default(),
            errors: Errors::default(),
        }
    }
}

impl Analyzer {
    pub async fn check(&mut self, source: String, status: UseState<String>) {
        self.root
            .reload(
                &source,
                &self.scope,
                &mut self.errors,
                &mut LoaderImpl(status.clone()),
            )
            .await;

        status.set("code checked".to_owned());
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

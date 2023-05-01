use std::{fmt::Display, mem, ops::Range};

use crate::SourceId;

#[derive(Default, Debug)]
pub struct Errors {
    current_source: Option<SourceId>,
    errors: Vec<Error>,
}

impl Errors {
    pub fn set_source(&mut self, source: Option<SourceId>) {
        self.current_source = source;
    }
    pub fn error<'a>(&'a mut self, span: Range<usize>) -> ErrorBuilder<'a> {
        ErrorBuilder::new(self, span)
    }

    pub fn drain_errors(&mut self) -> impl Iterator<Item = Error> + '_ {
        self.errors.drain(..)
    }
}

pub struct ErrorBuilder<'ctx> {
    errors: &'ctx mut Errors,
    error: Error,
}

impl<'ctx> ErrorBuilder<'ctx> {
    pub fn new(errors: &'ctx mut Errors, span: Range<usize>) -> Self {
        Self {
            error: Error {
                source: errors.current_source,
                span,
                message: String::new(),
            },
            errors,
        }
    }

    pub fn message(mut self, message: impl Into<String>) -> Self {
        self.error.message = message.into();
        self
    }

    pub fn context(mut self, message: impl Display) -> Self {
        self.error.message.push_str(": ");
        self.error.message.push_str(&message.to_string());
        self
    }

    pub fn terminate(self) -> Option<!> {
        None
    }
}

impl Drop for ErrorBuilder<'_> {
    fn drop(&mut self) {
        self.errors.errors.push(mem::take(&mut self.error));
    }
}

#[derive(Default, Debug)]
pub struct Error {
    pub source: Option<SourceId>,
    pub span: Range<usize>,
    pub message: String,
}

mod interp_impl;
mod layout;

use crate::{Diagnostics, FuncRef, InstrKind, Instrs, Module, ModuleRef, Resolved};

pub use self::layout::Layout;

type OffsetRepr = i32;

pub struct Local {
    id: u32,
}

#[derive(Default)]
pub struct Ir {
    pub instrs: Vec<InstrKind>,
}

pub struct Interpreter<'ctx> {
    diags: &'ctx mut Diagnostics,
    modules: &'ctx Module,
    instrs: &'ctx Instrs,
}

pub struct Entry {
    pub module: ModuleRef,
    pub func: FuncRef,
    pub inputs: Vec<Resolved>,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(diags: &'ctx mut Diagnostics, modules: &'ctx Module, instrs: &'ctx Instrs) -> Self {
        Self {
            diags,
            modules,
            instrs,
        }
    }
}

use crate::{FuncRef, ModuleRef, Resolved};

use super::{Interpreter, Ir};

struct Res {
    module: ModuleRef,
    func: FuncRef,
    ir: Ir,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn eval(mut self, module: ModuleRef, func: FuncRef) -> Option<(Ir, Option<Resolved>)> {
        let mut res = Res {
            module,
            func,
            ir: Default::default(),
        };

        Some((res.ir, None))
    }
}

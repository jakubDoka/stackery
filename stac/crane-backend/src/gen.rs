use cranelift_codegen::settings::{self, Flags};
use cranelift_object::{ObjectBuilder, ObjectModule};
use stac::{Diagnostics, Instrs, Layout, Modules, Ref};
use target_lexicon::Triple;

mod emit_impl;

pub type CodeRef = Ref<(), u32>;

pub struct Generator {
    _target: Triple,
    module: ObjectModule,
}

impl Generator {
    pub fn new(target: Triple) -> Self {
        let isa = cranelift_codegen::isa::lookup(target.clone())
            .unwrap()
            .finish(Flags::new(settings::builder()))
            .unwrap();
        let module = ObjectModule::new(
            ObjectBuilder::new(
                isa,
                String::from("stac"),
                cranelift_module::default_libcall_names(),
            )
            .unwrap(),
        );

        Self {
            _target: target,
            module,
        }
    }

    pub fn finish(self, _entry: CodeRef) -> Vec<u8> {
        todo!();
    }
}

pub struct Emmiter<'ctx> {
    _diags: &'ctx mut Diagnostics,
    gen: &'ctx mut Generator,
    _modules: &'ctx Modules,
    _instrs: &'ctx Instrs,
    arch: Layout,
}

impl<'ctx> Emmiter<'ctx> {
    pub fn new(
        diags: &'ctx mut Diagnostics,
        gen: &'ctx mut Generator,
        modules: &'ctx Modules,
        instrs: &'ctx Instrs,
        arch: Layout,
    ) -> Self {
        Self {
            _diags: diags,
            gen,
            _modules: modules,
            _instrs: instrs,
            arch,
        }
    }
}

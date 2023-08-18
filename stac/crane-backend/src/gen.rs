use cranelift_codegen::{
    ir,
    settings::{self, Flags},
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use stac::{Diagnostics, FuncInst, Instrs, Layout, Modules, Ref, Types};
use target_lexicon::Triple;

mod emit_impl;

pub type CodeRef = Ref<(), u32>;

pub struct Generator {
    _target: Triple,
    module: ObjectModule,
    func_mapping: Vec<cranelift_module::FuncId>,
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
            func_mapping: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<u8> {
        self.module.finish().emit().unwrap()
    }

    pub fn declare_func(&mut self, id: FuncInst, sig: ir::Signature) -> cranelift_module::FuncId {
        let name = Self::func_name(id);
        let linkage = Linkage::Export;
        let f = self.module.declare_function(&name, linkage, &sig).unwrap();
        self.func_mapping.push(f);
        f
    }

    pub fn use_func(
        &mut self,
        id: FuncInst,
        got_em: Option<ir::FuncRef>,
        func: &mut ir::Function,
    ) -> (ir::FuncRef, usize) {
        let id = self.func_mapping[id as usize];
        let fref = got_em.unwrap_or_else(|| self.module.declare_func_in_func(id, func));
        let param_count = self
            .module
            .declarations()
            .get_function_decl(id)
            .signature
            .params
            .len();
        (fref, param_count)
    }

    fn func_name(id: FuncInst) -> String {
        if id == 0 {
            String::from("main")
        } else {
            format!("func_{}", id)
        }
    }
}

pub struct Emmiter<'ctx> {
    diags: &'ctx mut Diagnostics,
    gen: &'ctx mut Generator,
    modules: &'ctx Modules,
    _instrs: &'ctx Instrs,
    types: &'ctx Types,
    arch: Layout,
    dump_ir: bool,
}

impl<'ctx> Emmiter<'ctx> {
    pub fn new(
        diags: &'ctx mut Diagnostics,
        gen: &'ctx mut Generator,
        modules: &'ctx Modules,
        instrs: &'ctx Instrs,
        types: &'ctx Types,
        arch: Layout,
        dump_ir: bool,
    ) -> Self {
        Self {
            diags,
            gen,
            modules,
            _instrs: instrs,
            types,
            arch,
            dump_ir,
        }
    }
}

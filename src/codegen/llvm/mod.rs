pub mod codegen;
mod core;
pub mod builtins;

pub struct LLVMGenerator {
    context: core::Context,
    modules: Vec<core::Module>, // Or single?
}

impl LLVMGenerator {
    pub fn new() -> Self {
        LLVMGenerator {
            context: core::Context::new(),
            modules: vec![],
        }
    }

    pub fn add_module(&mut self) {
        // TODO
    }

    pub fn dump_ir(&self) {
        // TODO
    }

    pub fn dump_ir_to_file(&self) -> () {
        // TODO
    }
}

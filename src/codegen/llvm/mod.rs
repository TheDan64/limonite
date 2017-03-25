pub mod codegen;
mod core;
pub mod builtins;

use syntax::expr::ExprWrapper;

pub struct LLVMGenerator {
    context: core::Context,
    builder: core::Builder,
    modules: Vec<core::Module>, // Or single?
}

impl LLVMGenerator {
    pub fn new() -> Self {
        let context = core::Context::new();
        let builder = context.create_builder();

        LLVMGenerator {
            context: context,
            builder: builder,
            modules: vec![],
        }
    }

    pub fn add_module(&mut self, name: &str, ast: &ExprWrapper) {
        // TODO
    }

    pub fn dump_ir(&self) {
        // TODO
    }

    pub fn dump_ir_to_file(&self) -> () {
        // TODO
    }
}

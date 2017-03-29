pub mod codegen;
mod core;
pub mod builtins;

use syntax::expr::ExprWrapper;

pub struct LLVMGenerator {
    context: core::Context,
    builder: core::Builder,
    main_module: Option<core::Module>, // Or modules?
}

impl LLVMGenerator {
    pub fn new() -> Self {
        let context = core::Context::new();
        let builder = context.create_builder();

        LLVMGenerator {
            context: context,
            builder: builder,
            main_module: None,
        }
    }

    pub fn add_module(&mut self, name: &str, ast: &ExprWrapper) {
        // TODO: Support more modules

        if self.main_module.is_none() {
            self.main_module = Some(self.context.create_module(name));
        }
    }

    pub fn dump_ir(&self) {
        // TODO
    }

    pub fn dump_ir_to_file(&self) -> () {
        // TODO
    }

    pub fn run(&self) {
        let main_module = self.main_module.as_ref().unwrap();

        assert!(main_module.verify(true)); // TODO: print param as cli flag

        let execution_engine = main_module.create_execution_engine();
        let main = main_module.get_named_function("main").unwrap();

        execution_engine.run_function_as_main(main);
    }
}

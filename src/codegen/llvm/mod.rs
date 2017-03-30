pub mod codegen;
mod core;
pub mod builtins;

use syntax::expr::ExprWrapper;

pub struct LLVMGenerator {
    context: core::Context,
    builder: core::Builder,
    main_module: Option<core::Module>,
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

    pub fn add_main_module(&mut self, ast: &ExprWrapper) {
        if self.main_module.is_some() {
            panic!("Cannot override main module");
        }

        let main_module = self.context.create_module("main");

        // TODO: Wrap, setup main block
        self.generate_ir(&main_module, ast);

        self.main_module = Some(main_module);
    }

    pub fn dump_ir(&self) {
        if let Some(ref module) = self.main_module {
            module.dump();
        }
    }

    pub fn save_binary(&self) -> () {
        // TODO
    }

    pub fn run(&self) {
        let main_module = self.main_module.as_ref().unwrap();

        assert!(main_module.verify(true)); // TODO: print param as cli flag

        let execution_engine = match main_module.create_execution_engine() {
            Ok(ee) => ee,
            Err(s) => panic!("Failed to initialize execution_engine: {}", s),
        };

        let pass_manager = main_module.create_function_pass_manager();
        let target_data = execution_engine.get_target_data();
        let data_layout = target_data.get_data_layout();

        main_module.set_data_layout(data_layout);
        pass_manager.add_target_data(target_data);

        // TODO: Add more passes here

        pass_manager.initialize();

        let main = match main_module.get_named_function("main") {
            Some(main) => main,
            None => panic!("Could not find main function to enter into")
        };

        execution_engine.run_function_as_main(main);
    }

    pub fn generate_ir(&self, module: &core::Module, ast: &ExprWrapper) {

    }
}

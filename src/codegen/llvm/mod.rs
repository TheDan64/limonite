pub mod codegen;
mod core;
pub mod builtins;

use syntax::expr::{Expr, ExprWrapper};
use syntax::literals::Literals;

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

    pub fn add_main_module(&mut self, ast: ExprWrapper) {
        if self.main_module.is_some() {
            panic!("Cannot override main module");
        }

        let main_module = self.context.create_module("main");

        // TODO: Only run whole script as "main" if there isn't a main defined already
        let ast = ExprWrapper::default(Expr::FnDecl("main".into(), vec![], None, ast));

        self.generate_ir(&main_module, &ast);

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
            Err(s) => panic!("LLVMExecutionError: Failed to initialize execution_engine: {}", s),
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
            None => panic!("LLVMExecutionError: Could not find main function to run")
        };

        execution_engine.run_function_as_main(main);
    }

    pub fn generate_ir(&self, module: &core::Module, ast: &ExprWrapper) -> Option<core::Value> {
        match ast.get_expr() {
            &Expr::Block(ref exprs) => {
                let mut last_value = None;

                for expr in exprs {
                    last_value = self.generate_ir(module, expr);
                }

                last_value
            },
            &Expr::FnCall(ref name, ref args) => {
                let function = match module.get_named_function(name) {
                    Some(function) => function,
                    None => {
                        println!("LLVMGenError: Could not find function {}", name);

                        return None; // REVIEW: Should this panic? We should've already known it was missing and safely unwrap
                    }
                };

                let num_params = function.count_params() as usize;

                if num_params != args.len() {
                    println!("LLVMGenError: Function {} requires {} args. Found {}", name, args.len(), num_params);

                    return None; // REVIEW: panic?
                }

                let mut arg_values = Vec::with_capacity(num_params);

                for arg in args {
                    let value = self.generate_ir(module, arg).unwrap();

                    arg_values.push(value);
                }

                // REVIEW:
                // let is_void_return_type = function.get_return_type() == self.context.void_type();
                // Original impl checked if return_type.is_none() to set name to empty string...
                // Apparently void functions don't get saved return values. Maybe this check could be
                // baked into the build call?

                Some(self.builder.build_call(function, arg_values, name)) // REVIEW: maybe tmp_ + name? Unclear if same name as fn is bad..
            },
            &Expr::Literal(ref literal_type) => {
                match literal_type {
                    &Literals::UTF8Char(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
                    e => panic!("LLVMGenError: Unsupported literal type {}", e)
                }
            },
            &Expr::FnDecl(ref name, ref arg_defs, ref return_type, ref body_expr) => {
                // TODO: Support args types and return types
                let return_type = match return_type {
                    &Some(ref t) => panic!("TODO"),
                    &None => self.context.void_type().fn_type(vec![], false),
                };

                let function = module.add_function(name, return_type);

                let bb_enter = self.context.append_basic_block(&function, "enter");

                self.builder.position_at_end(&bb_enter);
                self.builder.insert_instruction(self.generate_ir(module, body_expr).unwrap()); // FIXME: unwrap

                // REVIEW: This will return the last generated value... is that what we want?
                // Or should it go back to the global scope after generating ir?
                // self.generate_ir(module, body_expr);

                Some(bb_enter.get_terminator())
            },
            e => panic!("LLVMGenError: Unsupported codegen: {:?}", e)
        }
    }
}

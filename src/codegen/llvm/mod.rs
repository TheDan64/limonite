mod core;
mod std;

use syntax::expr::{Expr, ExprWrapper};
use syntax::literals::Literals;
use codegen::llvm::std::string::{print_function_definition, string_type};

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

    pub fn add_main_module(&mut self, mut ast: ExprWrapper) {
        if self.main_module.is_some() {
            panic!("Cannot override main module");
        }

        let main_module = self.context.create_module("main");

        // TODO: Only run whole script as "main" if there isn't a main defined already
        // And only add return None if there isn't a return None defined already
        if let &mut Expr::Block(ref mut blocks) = ast.get_mut_expr() {
            blocks.push(ExprWrapper::default(Expr::Return(None)));
        }

        let ast = ExprWrapper::default(Expr::FnDecl("main".into(), vec![], None, ast));

        print_function_definition(&self.builder, &self.context, &main_module);

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

        println!("DEBUG: Start Verify");
        assert!(main_module.verify(true)); // TODO: print param as cli flag
        println!("DEBUG: End Verify");

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

        let main = match main_module.get_function("main") {
            Some(main) => main,
            None => panic!("LLVMExecutionError: Could not find main function to run")
        };

        self.builder.build_return(None);

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
                let function = match module.get_function(name) {
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

                Some(self.builder.build_call(&function, arg_values, name)) // REVIEW: maybe tmp_ + name? Unclear if same name as fn is bad..
            },
            &Expr::Literal(ref literal_type) => {
                match literal_type {
                    &Literals::UTF8Char(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
                    &Literals::UTF8String(ref val) => {
                        let string_type = string_type(&self.context);
                        let i8_type = self.context.i8_type();
                        let i8_ptr_type = i8_type.ptr_type(0);
                        let i32_type = self.context.i32_type();
                        let i64_type = self.context.i64_type();

                        let len = i64_type.const_int(val.len() as u64, false);
                        // TODO: Get capacity too
                        // let const_str_char =
                        let mut chars = Vec::with_capacity(val.len()); // REVIEW: is rust string len in chars or bytes? Probably bytes?

                        for chr in val.bytes() {
                            chars.push(i8_type.const_int(chr as u64, false));
                        }

                        let const_str_array = i8_type.const_array(chars);

                        let stack_struct = self.builder.build_stack_allocation(&string_type, "string_struct");

                        let mut struct_index = vec![i32_type.const_int(0, false), i32_type.const_int(1, false)];

                        let len_ptr = self.builder.build_gep(&stack_struct, &mut struct_index, "len_ptr");

                        let store_len = self.builder.build_store(&len, &len_ptr);

                        let mut struct_index = vec![i32_type.const_int(0, false), i32_type.const_int(0, false)];

                        let str_ptr = self.builder.build_gep(&stack_struct, &mut struct_index, "str_ptr");

                        let heap_ptr = self.builder.build_heap_allocation(&i8_type, "str");
                        // let heap_ptr = self.builder.build_array_heap_allocation(&i8_type, &const_str_array, "str");

                        // let stored = self.builder.build_store(&heap_ptr, &str_ptr);

                        self.builder.build_store(&const_str_array, &heap_ptr);

                        // self.builder.build_insert_value(&const_str_array, &heap_ptr, 0, "insert");

                        let tmp = self.builder.build_load(&stack_struct, "string");

                        module.dump();

                        Some(tmp)
                    },
                    e => panic!("LLVMGenError: Unsupported literal type {}", e)
                }
            },
            &Expr::FnDecl(ref name, ref arg_defs, ref return_type, ref body_expr) => {
                // TODO: Support args types and return types
                let return_type = match return_type {
                    &Some(ref t) => panic!("TODO: Return types"),
                    &None => self.context.void_type().fn_type(&mut vec![], false),
                };

                let function = module.add_function(name, return_type);

                let bb_enter = self.context.append_basic_block(&function, "enter");

                self.builder.position_at_end(&bb_enter);
                // self.builder.insert_instruction(); // FIXME: unwrap

                // REVIEW: This will return the last generated value... is that what we want?
                // Or should it go back to the global scope after generating ir?
                // self.generate_ir(module, body_expr);

                // Note: Terminator is None if the block is not well formed
                self.generate_ir(module, body_expr)
            },
            &Expr::Return(ref return_type_expr) => {
                match return_type_expr {
                    &Some(ref return_type) => match self.generate_ir(module, return_type) {
                        Some(t) => Some(self.builder.build_return(Some(t))),
                        None => panic!("LLVMGenError: Could not generate return type IR"),
                    },
                    &None => Some(self.builder.build_return(None)),
                }
            },
            e => panic!("LLVMGenError: Unsupported codegen: {:?}", e)
        }
    }
}

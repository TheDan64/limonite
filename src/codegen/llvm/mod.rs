mod core;
mod std;

use codegen::llvm::std::string::{print_function_definition, string_type};
use self::core::{Builder, Context, Module, Type, Value};
use std::collections::HashMap;
use syntax::expr::{Expr, ExprWrapper};
use syntax::literals::Literals;
use syntax::op::{InfixOp, UnaryOp};

pub struct LLVMGenerator {
    context: Context,
    builder: Builder,
    main_module: Option<Module>,
}

impl LLVMGenerator {
    pub fn new() -> Self {
        let context = Context::new();
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

        self.generate_ir(&main_module, &ast, &mut HashMap::new());

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

    pub fn get_function_address(&self, fn_name: &str) -> Option<u64> {
        // REVIEW: Will the function still work if EE drops?
        let main_module = self.main_module.as_ref().expect("Could not find a main module");

        assert!(main_module.verify(true)); // TODO: print param as cli flag

        let execution_engine = match main_module.create_execution_engine() {
            Ok(ee) => ee,
            Err(s) => panic!("LLVMExecutionError: Failed to initialize execution_engine: {}", s),
        };

        execution_engine.get_function_address(fn_name)
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
        // pass_manager.add_optimize_memcpy_pass();

        pass_manager.initialize();

        let main = match main_module.get_function("main") {
            Some(main) => main,
            None => panic!("LLVMExecutionError: Could not find main function to run")
        };

        execution_engine.run_function_as_main(main);
    }

    pub fn generate_ir(&self, module: &Module, ast: &ExprWrapper, scoped_variables: &mut HashMap<String, Value>) -> Option<Value> { // TODO: Result makes more sense. Maybe Result<Value, Enum(Error, ErrorVec)>?
        // REVIEW: Should scoped_variables take a COW keys?

        match ast.get_expr() {
            &Expr::Block(ref exprs) => {
                let mut last_value = None;

                for expr in exprs {
                    last_value = self.generate_ir(module, expr, scoped_variables);
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
                    let value = self.generate_ir(module, arg, scoped_variables).unwrap();

                    arg_values.push(value);
                }

                // REVIEW:
                // let is_void_return_type = function.get_return_type() == self.context.void_type();
                // Original impl checked if return_type.is_none() to set name to empty string...
                // Apparently void functions don't get saved return values. Maybe this check could be
                // baked into the build call?

                Some(self.builder.build_call(&function, &arg_values, name)) // REVIEW: maybe tmp_ + name? Unclear if same name as fn is bad..
            },
            &Expr::Literal(ref literal_type) => {
                match literal_type {
                    &Literals::UTF8Char(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
                    &Literals::UTF8String(ref val) => {
                        let string_type = string_type(&self.context);
                        let void_type = self.context.void_type();
                        let bool_type = self.context.bool_type();
                        let i8_type = self.context.i8_type();
                        let i8_ptr_type = i8_type.ptr_type(0);
                        let i32_type = self.context.i32_type();
                        let i64_type = self.context.i64_type();
                        let i8_array_type = i8_type.array_type(val.len() as u32);
                        let i32_one = i32_type.const_int(1, false);
                        let bool_false = bool_type.const_int(0, false);

                        let len = i64_type.const_int(val.len() as u64, false);

                        let mut chars = Vec::with_capacity(val.len());

                        for chr in val.bytes() {
                            chars.push(i8_type.const_int(chr as u64, false));
                        }

                        let const_str_array = i8_array_type.const_array(chars);

                        let global_str = module.add_global(&i8_array_type, &Some(const_str_array), "global_str");

                        let stack_struct = self.builder.build_stack_allocation(&string_type, "string_struct");

                        let str_ptr = self.builder.build_gep(&stack_struct, &vec![0, 0], "str_ptr");
                        let len_ptr = self.builder.build_gep(&stack_struct, &vec![0, 1], "len_ptr");
                        let cap_ptr = self.builder.build_gep(&stack_struct, &vec![0, 2], "cap_ptr");

                        self.builder.build_store(&len, &len_ptr);
                        self.builder.build_store(&len, &cap_ptr);

                        let i8_heap_array = self.builder.build_array_heap_allocation(&i8_array_type, &(val.len() as u64), "i8_heap_array");
                        let i8_heap_ptr = self.builder.build_pointer_cast(&i8_heap_array, &i8_ptr_type, "i8_heap_ptr");

                        self.builder.build_store(&i8_heap_ptr, &str_ptr);

                        let memcpy_fn = match module.get_function("llvm.memcpy.p0i8.p0i8.i64") {
                            Some(f) => f,
                            None => {
                                let i8_ptr_type2 = i8_type.ptr_type(0);
                                let mut args = vec![i8_ptr_type, i8_ptr_type2, i64_type, i32_type, bool_type];

                                let fn_type2 = void_type.fn_type(&mut args, false);

                                module.add_function("llvm.memcpy.p0i8.p0i8.i64", fn_type2)
                            }
                        };

                        let global_i8_ptr = self.builder.build_gep(&global_str, &vec![0, 0], "global_i8_ptr");

                        self.builder.build_call(&memcpy_fn, &vec![i8_heap_ptr, global_i8_ptr, len, i32_one, bool_false], "llvm.memcpy.p0i8.p0i8.i64"); // REVIEW: val.len() as u64 doesn't seem to work. Says type is invalid... due to missing context?

                        Some(stack_struct)
                    },
                    &Literals::I32Num(ref val) => Some(self.context.i32_type().const_int(*val as u64, true)),
                    &Literals::I64Num(ref val) => Some(self.context.i64_type().const_int(*val as u64, true)),
                    &Literals::U32Num(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
                    &Literals::U64Num(ref val) => Some(self.context.i64_type().const_int(*val as u64, false)),
                    &Literals::F32Num(ref val) => Some(self.context.f32_type().const_float(*val as f64)),
                    &Literals::F64Num(ref val) => Some(self.context.f64_type().const_float(*val as f64)),
                    &Literals::Bool(ref val) => Some(self.context.bool_type().const_int(*val as u64, false)),
                    &Literals::_None => panic!("LLVMGenError: Unimplemented for NoneType")
                }
            },
            &Expr::InfixOp(ref op, ref lhs_exprwrapper, ref rhs_exprwrapper) => {
                let (lhs_val, rhs_val) =  match (self.generate_ir(module, lhs_exprwrapper, scoped_variables), self.generate_ir(module, rhs_exprwrapper, scoped_variables)) {
                    (Some(val1), Some(val2)) => (val1, val2),
                    (Some(_), None) => unreachable!("LLVMGenError: InfixOp only LHS contains value"),
                    (None, Some(_)) => unreachable!("LLVMGenError: InfixOp only RHS contains value"),
                    (None, None) => unreachable!("LLVMGenError: InfixOp has no values")
                };

                // Adding different types should never happen if SA is doing it's job, right?
                match op {
                    &InfixOp::Add => Some(self.builder.build_add(&lhs_val, &rhs_val, "add")),
                    &InfixOp::Sub => Some(self.builder.build_sub(&lhs_val, &rhs_val, "sub")),
                    &InfixOp::Mul => Some(self.builder.build_mul(&lhs_val, &rhs_val, "mul")),
                    &InfixOp::Div => match (lhs_val, rhs_val) {
                        // LLVMBuildFDiv, LLVMBuildSDiv, LLVMBuildUDiv
                        _ => panic!("LLVMGenError: Unimplemented infix operator div")
                    },
                    &InfixOp::Mod => match (lhs_val, rhs_val) {
                        _ => panic!("LLVMGenError: Unimplemented infix operator mod")
                    },
                    &InfixOp::Pow => match (lhs_val, rhs_val) {
                        _ => panic!("LLVMGenError: Unimplemented infix operator pow")
                    },
                    &InfixOp::Equ => match (lhs_val, rhs_val) {
                        // LLVMBuildICmp, LLVMBuildFCmp?
                        _ => panic!("LLVMGenError: Unimplemented infix operator equ")
                    }
                }
            },
            // Needs further testing
            &Expr::UnaryOp(ref op, ref expr) => {
                match op {
                    &UnaryOp::Negate => self.generate_ir(module, expr, scoped_variables).map(|val| self.builder.build_neg(&val, "neg")),
                    &UnaryOp::Not => self.generate_ir(module, expr, scoped_variables).map(|val| self.builder.build_not(&val, "not")),
                }
            },
            &Expr::FnDecl(ref name, ref arg_defs, ref return_type, ref body_expr) => {
                let mut fn_variable_scope = HashMap::new(); // REVIEW: This should exclude globals
                let mut arg_types: Vec<Type> = arg_defs.iter().map(|&(_, ref type_string)| self.string_to_type(&type_string[..])).collect();

                // TODO: Support args types and return types
                let return_type = match return_type {
                    &Some(ref type_string) => self.string_to_type(&type_string[..]),
                    &None => self.context.void_type(),
                };

                let function = module.add_function(name, return_type.fn_type(&mut arg_types, false));

                // REVIEW: This can be unclear zipping "function" will get the iterator of it's params
                let name_value_data = arg_defs.iter().map(|&(ref name, _)| name).zip(function.params());

                for (name, mut param_value) in name_value_data {
                    param_value.set_name(&name);
                    fn_variable_scope.insert(name.to_string(), param_value.as_value()); // REVIEW: Cow?
                }

                let bb_enter = self.context.append_basic_block(&function, "enter");

                self.builder.position_at_end(&bb_enter);
                // self.builder.insert_instruction(); // FIXME: unwrap

                // REVIEW: This will return the last generated value... is that what we want?
                // Or should it go back to the global scope after generating ir?
                // self.generate_ir(module, body_expr);
                self.generate_ir(module, body_expr, &mut fn_variable_scope)
            },
            &Expr::Return(ref return_type_expr) => {
                match return_type_expr {
                    &Some(ref return_type) => match self.generate_ir(module, return_type, scoped_variables) {
                        Some(t) => Some(self.builder.build_return(Some(t))),
                        None => unreachable!("LLVMGenError: Hit unreachable return type generation")
                    },
                    &None => Some(self.builder.build_return(None)),
                }
            },
            &Expr::Var(ref name) => {
                match scoped_variables.get(name) {
                    Some(val) => Some(*val),
                    None => unreachable!("LLVMGenError: Unknown variable {} was uncaught", name)
                }
            },
            _ => unimplemented!()
        }
    }

    fn string_to_type(&self, name: &str) -> Type {
        match name {
            "bool" => self.context.bool_type(),
            "f32" => self.context.f32_type(),
            "i32" => self.context.i32_type(),
            "u32" => self.context.i32_type(),
            "f64" => self.context.f64_type(),
            "i64" => self.context.i64_type(),
            "u64" => self.context.i64_type(),
            "void" => self.context.void_type(), // TODO: Not use name "void"
            _ => unimplemented!() // LLVMGetTypeByName()?
        }
    }
}

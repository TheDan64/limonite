extern crate rustc;

use self::rustc::lib::llvm::*;
use syntax::ast::expr::*;
use std::collections::HashMap;

pub struct Context {
    context: ContextRef,
    module: ModuleRef,
    builder: BuilderRef,
    named_values: HashMap<String, ValueRef>
}

impl Context {
    pub fn new(module_name: &str) -> Context {
        unsafe {
            let context = LLVMContextCreate();
            let module_name_c_str = c_str_ptr(module_name);
            let module = LLVMModuleCreateWithNameInContext(module_name_c_str, context);
            let builder = LLVMCreateBuilderInContext(context);
            let named_values = HashMap::new();

            Context {
                context: context,
                module: module,
                builder: builder,
                named_values: named_values
            }
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

pub trait CodeGen {
    fn gen_code(&self, context: &mut Context) -> Option<ValueRef>;
}

impl CodeGen for ExprWrapper {
    fn gen_code(&self, context: &mut Context) -> Option<ValueRef> {
        self.get_expr().gen_code(context)
    }
}

impl CodeGen for Expr {
    fn gen_code(&self, context: &mut Context) -> Option<ValueRef> {
        match *self {
            Expr::Block(ref vec) => {
                for expr in vec {
                    expr.gen_code(context);
                }

                None
            },
            Expr::Ident(ref name) => {
                match context.named_values.get(name) {
                    Some(val) => Some(*val),
                    None => {
                        // Add a standard error writing system?
                        println!("Could not find func {}", name);

                        None
                    }
                }
            },
            Expr::FnCall(ref name, ref args) => {
                // This expr should always contain a string
                unsafe {
                    let function = LLVMGetNamedFunction(context.module, c_str_ptr(name));

                    if function.is_null() {
                        // ToDo: Add standardized error message writing?
                        println!("Error: Function {} not found.", name);
                        return None;
                    }

                    if LLVMCountParams(function) as usize != args.len() {
                        println!("Incorrect number of arguments");
                        return None;
                    }

                    let mut arg_values = Vec::new();

                    for arg in args {
                        if let Some(value) = arg.gen_code(context) {
                            arg_values.push(value);
                        }

                        return None;
                    }

                    Some(LLVMBuildCall(context.builder, function, arg_values.as_ptr(), arg_values.len() as u32, c_str_ptr("calltmp")))
                }
            },
            // Expr::InfixOp(ref op, ref expr1, ref expr2) => {
            //     let lhs = expr1.get_expr();
            //     let rhs = expr2.get_expr();

            //     if let (&Expr::Const(ref lhs_const), &Expr::Const(ref rhs_const)) = (lhs, rhs) {
            //         match (lhs_const, rhs_const) {
            //             (&Const::I32Num(lhs_num), &Const::I32Num(rhs_num)) => {
            //                 // No idea if this works

            //                 let name = concat!("idk", "\0").as_ptr() as *const i8;
            //                 // Returns *mut Value_opaque
            //                 LLVMBuildAdd(llvm_builder, lhs_num, rhs_num, name);
            //             },
            //             _ => ()
            //         }

            //     }
            // },
            Expr::NoOp => None,
            _ => None
        }
    }
}

// Helper functions
fn c_str_ptr(rust_str: &str) -> *const i8 {
    format!("{}\0", rust_str).as_ptr() as *const i8
}

extern crate llvm_sys;

use syntax::ast::expr::*;
use std::collections::HashMap;
use std::ffi::CString;
use std::boxed;
use self::llvm_sys::core::*;
use self::llvm_sys::*;

// Struct to keep track of data needed to build IR
pub struct Context {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
    named_values: HashMap<String, *mut LLVMValue>
}

impl Context {
    pub fn new(module_name: &str) -> Context {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(c_str_ptr(module_name), context);
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

    // Dump the IR to stdout
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
    fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue>;
}

impl CodeGen for ExprWrapper {
    fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue> {
        self.get_expr().gen_code(context)
    }
}

impl CodeGen for Expr {
    fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue> {
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

                    // as_ptr gets *const not *mut so boxing -> raw gets *mut ptr
                    let args_ptr = boxed::into_raw(Box::new(*arg_values.as_ptr()));

                    Some(LLVMBuildCall(context.builder, function, args_ptr, arg_values.len() as u32, c_str_ptr("calltmp")))
                }
            },
            Expr::NoOp => None,
            _ => None
        }
    }
}

// Helper functions
fn c_str_ptr(rust_str: &str) -> *const i8 {
    match CString::new(rust_str.as_bytes()) {
        Ok(string) => string,
        Err(err) => panic!(err)
    }.as_ptr()
}

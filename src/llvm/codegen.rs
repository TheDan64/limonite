extern crate llvm_sys;

use std::collections::HashMap;
use std::ffi::CString;
use self::llvm_sys::core::*;
use self::llvm_sys::*;
use syntax::ast::expr::*;
use syntax::ast::consts::*;

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

    pub fn get_context(&self) -> *mut LLVMContext {
        self.context
    }

    pub fn get_module(&self) -> *mut LLVMModule {
        self.module
    }

    pub fn get_builder(&self) -> *mut LLVMBuilder {
        self.builder
    }

    pub fn clear_values(&mut self) {
        self.named_values.clear();
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
                let mut gen = None;

                for expr in vec {
                    gen = expr.gen_code(context);
                }

                gen
            },
            Expr::Ident(ref name) => {
                match context.named_values.get(name) {
                    Some(val) => Some(*val),
                    None => {
                        println!("Could not find ident {}", name);

                        None
                    }
                }
            },
            Expr::Const(ref const_type) => {
                match const_type {
                    &Const::UTF8String(ref val) => {
                        unsafe {
                            // Types
                            let int8 = LLVMInt8TypeInContext(context.get_context());
                            let int32 = LLVMInt32TypeInContext(context.get_context());
                            let int8_ptr = LLVMPointerType(int8, val.len() as u32);

                            // Values
                            let len = LLVMConstInt(int32, val.len() as u64, 0); // 0 = unsigned?

                            // Alloca might be less portable than malloc, but I read it is overall better.
                            // Should look into this.
                            let string = LLVMBuildAlloca(context.get_builder(), int8_ptr, c_str_ptr("alloca"));

                            let struct_fields = vec![len, string];

                            Some(LLVMConstStructInContext(context.get_context(), struct_fields.as_ptr() as *mut _, 2, 0))
                        }
                    },
                    _ => {
                        println!("Error: Codegen unimplemented for {:?}", const_type);
                        None
                    }
                }
            },
            Expr::FnCall(ref name, ref args) => {
                // This expr should always contain a string
                unsafe {
                    let function = LLVMGetNamedFunction(context.module, c_str_ptr(name));

                    if function.is_null() {
                        // TODO: Add standardized error message writing?
                        println!("Error: Function {} not found.", name);
                        return None;
                    }

                    let arg_count = LLVMCountParams(function) as usize;

                    if arg_count != args.len() {
                        println!("Error: Function {} requires {} argument(s), {} given.", name, arg_count, args.len());
                        return None;
                    }

                    let mut arg_values = Vec::new();

                    for arg in args {
                        match arg.gen_code(context) {
                            Some(value) => arg_values.push(value),
                            None => {
                                println!("Fatal Error: Argument {:?} codegen failed!", arg);
                                return None
                            }
                        }
                    }

                    // Void functions don't get saved return values
                    let ret_var = if LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function))) == LLVMVoidTypeInContext(context.context) {
                        c_str_ptr("")
                    } else {
                        let mut tmp = name.to_string();
                        tmp.push_str("tmp");

                        c_str_ptr(&tmp)
                    };

                    Some(LLVMBuildCall(context.builder, function, arg_values.as_ptr() as *mut _, arg_values.len() as u32, ret_var))
                 }
            },
            Expr::NoOp => None,
            _ => None
        }
    }
}

// Helper function
pub fn c_str_ptr(rust_str: &str) -> *const i8 {
    match CString::new(rust_str.as_bytes()) {
        Ok(string) => string,
        Err(err) => panic!(err)
    }.as_ptr()
}

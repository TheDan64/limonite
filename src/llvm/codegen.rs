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
    unsafe fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue>;
}

impl CodeGen for ExprWrapper {
    unsafe fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue> {
        self.get_expr().gen_code(context)
    }
}

impl CodeGen for Expr {
    unsafe fn gen_code(&self, context: &mut Context) -> Option<*mut LLVMValue> {
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
                        // Types
                        let array_type1 = LLVMArrayType(LLVMInt8TypeInContext(context.get_context()), val.len() as u32);
                        let ptr_type1 = LLVMPointerType(array_type1, 0);
                        let string_struct_type = LLVMGetNamedGlobal(context.get_module(), c_str_ptr("struct.string"));
                        if string_struct_type.is_null() {
                            println!("TMP Error: Cannot find builtin string type!");
                        }
                        let int8_type = LLVMInt8TypeInContext(context.get_context());
                        let int8_ptr_type = LLVMPointerType(int8_type, 0);
                        let int32_type = LLVMInt32TypeInContext(context.get_context());
                        let string_type_fields = vec![LLVMInt32TypeInContext(context.get_context()),
                                      LLVMPointerType(LLVMInt8TypeInContext(context.get_context()), 0)];

                        // Values
                        let len = LLVMConstInt(int32_type, val.len() as u64, 0);

                        // Make a global string constant and assign the value:
                        let const_str_var = LLVMAddGlobal(context.get_module(), array_type1, c_str_ptr("conststr"));
                        let mut chars = Vec::new();
                        for chr in val.bytes() {
                            chars.push(LLVMConstInt(int8_type, chr as u64, 0));
                        }

                        let const_str_array = LLVMConstArray(int8_type, chars.as_ptr() as *mut _, chars.len() as u32);
                        LLVMSetInitializer(const_str_var, const_str_array);
                        LLVMSetGlobalConstant(const_str_var, 1);

                        // Alloca might be less portable than malloc, but I read it is overall better.
                        // Should look into this.
                        let twelve = LLVMConstInt(int32_type, 10, 0);
                        let args = vec![twelve, twelve];
                        let allocated_str_ptr = LLVMBuildAlloca(context.get_builder(), int8_ptr_type, c_str_ptr("strptr"));
                        let element_ptr = LLVMBuildGEP(context.get_builder(), const_str_var, args.as_ptr() as *mut _, 0, c_str_ptr(""));
                        let store = LLVMBuildStore(context.get_builder(), element_ptr, allocated_str_ptr); // not correct, GEP not working?

                        let struct_fields = vec![len, allocated_str_ptr];

                        Some(LLVMConstStructInContext(context.get_context(), struct_fields.as_ptr() as *mut _, 2, 0))
                    },
                    _ => {
                        println!("Error: Codegen unimplemented for {:?}", const_type);
                        None
                    }
                }
            },
            Expr::FnCall(ref name, ref args) => {
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

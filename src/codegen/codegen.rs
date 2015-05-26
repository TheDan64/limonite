extern crate llvm_sys;

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::str;
use self::llvm_sys::core::*;
use self::llvm_sys::analysis::*;
use self::llvm_sys::execution_engine::*;
use self::llvm_sys::prelude::*;
use self::llvm_sys::target::*;
//use self::llvm_sys::transforms::scalar::*;
use syntax::ast::op::*;
use syntax::ast::expr::*;
use syntax::ast::literals::*;
use syntax::core::types::*;

// Struct to keep track of data needed to build IR
pub struct Context {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    execution_engine: LLVMExecutionEngineRef,
    fn_pass_manager: LLVMPassManagerRef,
    named_values: HashMap<String, LLVMValueRef>,
}

impl Context {
    pub unsafe fn new(module_name: &str) -> Context {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
        LLVMLinkInInterpreter();

        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(c_str_ptr(module_name), context);
        let builder = LLVMCreateBuilderInContext(context);
        let named_values = HashMap::new();
        let mut execution_engine = 0 as LLVMExecutionEngineRef;
        let mut error_msg = 0 as *mut i8;

        if LLVMCreateExecutionEngineForModule(&mut execution_engine, module, &mut error_msg) == 1 {
            panic!("{}", str::from_utf8(CStr::from_ptr(error_msg).to_bytes()).unwrap())
        }

        let fn_pass_manager = LLVMCreateFunctionPassManagerForModule(module);
        let target_data = LLVMGetExecutionEngineTargetData(execution_engine);
        let data_layout = LLVMCopyStringRepOfTargetData(target_data);
        LLVMSetDataLayout(module, data_layout);
        LLVMAddTargetData(target_data, fn_pass_manager);
        LLVMDisposeMessage(data_layout);

        // Add desired passes to pass manager here
//        LLVMAddInstructionCombiningPass(fn_pass_manager);

        LLVMInitializeFunctionPassManager(fn_pass_manager);

        Context {
            context: context,
            module: module,
            builder: builder,
            execution_engine: execution_engine,
            fn_pass_manager: fn_pass_manager,
            named_values: named_values,
        }
    }

    // Dump the IR to stdout
    pub unsafe fn dump(&self) {
        LLVMDumpModule(self.module);
    }

    // Verifies that the llvm code is valid. Optionally prints
    // the error message, else abort.
    pub unsafe fn verify(&self) {
        let action = LLVMVerifierFailureAction::LLVMPrintMessageAction;
        let msg = vec![c_str_ptr("")];

        LLVMVerifyModule(self.module, action, msg.as_ptr() as *mut _);
        LLVMDisposeMessage(msg[0] as *mut _);
    }

    pub unsafe fn run(&self) -> u64 {
        let main = LLVMGetNamedFunction(self.module, c_str_ptr("main"));
        let result = LLVMRunFunction(self.execution_engine, main, 0, 0 as *mut LLVMGenericValueRef);

        LLVMGenericValueToInt(result, 1)
    }

    // Struct getters
    pub fn get_context(&self) -> LLVMContextRef {
        self.context
    }

    pub fn get_module(&self) -> LLVMModuleRef {
        self.module
    }

    pub fn get_builder(&self) -> LLVMBuilderRef {
        self.builder
    }

    pub fn clear_values(&mut self) {
        self.named_values.clear();
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.fn_pass_manager);
//            LLVMDisposeExecutionEngine(self.execution_engine); // signal 4s for some reason
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

pub trait CodeGen {
    unsafe fn gen_code(&self, context: &mut Context) -> Option<LLVMValueRef>;
}

impl CodeGen for ExprWrapper {
    unsafe fn gen_code(&self, context: &mut Context) -> Option<LLVMValueRef> {
        self.get_expr().gen_code(context)
    }
}

impl CodeGen for Expr {
    unsafe fn gen_code(&self, context: &mut Context) -> Option<LLVMValueRef> {
        match *self {
            Expr::Block(ref vec) => {
                let mut gen = None;

                for expr in vec {
                    gen = expr.gen_code(context);
                }

                gen
            },
            Expr::Var(ref name) => {
                match context.named_values.get(name) {
                    Some(val) => Some(*val),
                    None => panic!("CodeGen Error: Unknown variable named {}", name)
                }
            },
            Expr::Literal(ref literal_type) => {
                match *literal_type {
                    Literals::UTF8Char(ref val) => {
                        let i32_type = LLVMInt32TypeInContext(context.get_context());
                        Some(LLVMConstInt(i32_type, *val as u64, 0))
                    },
                    Literals::UTF8String(ref val) => {
                        // Types
                        let array_type1 = LLVMArrayType(LLVMInt8TypeInContext(context.get_context()), val.len() as u32);
                        // let string_struct_type = LLVMGetNamedGlobal(context.get_module(), c_str_ptr("struct.string"));
                        // if string_struct_type.is_null() {
                        //     println!("TMP Error: Cannot find builtin string type!");
                        // }
                        let i8_type = LLVMInt8TypeInContext(context.get_context());
                        let i8_ptr_type = LLVMPointerType(i8_type, 0);
                        let i32_type = LLVMInt32TypeInContext(context.get_context());
                        let i64_type = LLVMInt64TypeInContext(context.get_context());
                        // let string_type_fields = vec![LLVMInt32TypeInContext(context.get_context()),
                        //               LLVMPointerType(LLVMInt8TypeInContext(context.get_context()), 0)];

                        // Values
                        let len = LLVMConstInt(i64_type, val.len() as u64, 0);

                        // Make a global string constant and assign the value:
                        let const_str_var = LLVMAddGlobal(context.get_module(), array_type1, c_str_ptr("str"));
                        let mut chars = Vec::new();
                        for chr in val.bytes() {
                            chars.push(LLVMConstInt(i8_type, chr as u64, 0));
                        }

                        let const_str_array = LLVMConstArray(i8_type, chars.as_ptr() as *mut _, chars.len() as u32);
                        LLVMSetInitializer(const_str_var, const_str_array);
                        LLVMSetGlobalConstant(const_str_var, 1);

                        let args = vec![LLVMConstInt(i32_type, 0, 0), LLVMConstInt(i32_type, 0, 0)];
                        let allocated_str_ptr = LLVMBuildAlloca(context.get_builder(), i8_ptr_type, c_str_ptr("a"));
                        let mallocated_ptr = LLVMBuildMalloc(context.get_builder(), i8_type, c_str_ptr("m"));

                        LLVMSetTailCall(mallocated_ptr, 0);
                        LLVMBuildStore(context.get_builder(), mallocated_ptr, allocated_str_ptr);

                        let element_ptr = LLVMBuildGEP(context.get_builder(), const_str_var, args.as_ptr() as *mut _, 2, c_str_ptr(""));
                        LLVMBuildStore(context.get_builder(), element_ptr, allocated_str_ptr);

                        let loaded_ptr = LLVMBuildLoad(context.get_builder(), allocated_str_ptr, c_str_ptr("l"));

                        // String type generated here should eventually be preloaded type
                        let string_type_fields = vec![LLVMPointerType(i8_type, 0), i64_type];
                        let string_type = LLVMStructTypeInContext(context.get_context(), string_type_fields.as_ptr() as *mut _, 2, 0);
                        let undef = LLVMGetUndef(string_type);

                        let i = LLVMBuildInsertValue(context.get_builder(), undef, loaded_ptr, 0, c_str_ptr("i"));
                        Some(LLVMBuildInsertValue(context.get_builder(), i, len, 1, c_str_ptr("i")))
                    },
                    Literals::I32Num(ref val) => {
                        let ty = LLVMInt32TypeInContext(context.get_context());
                        Some(LLVMConstInt(ty, *val as u64, 1))
                    },
                    Literals::I64Num(ref val) => {
                        let ty = LLVMInt64TypeInContext(context.get_context());
                        Some(LLVMConstInt(ty, *val as u64, 1))
                    },
                    Literals::U32Num(ref val) => {
                        let ty = LLVMInt32TypeInContext(context.get_context());
                        Some(LLVMConstInt(ty, *val as u64, 0))
                    },
                    Literals::U64Num(ref val) => {
                        let ty = LLVMInt64TypeInContext(context.get_context());
                        Some(LLVMConstInt(ty, *val, 0))
                    },
                    Literals::F32Num(ref val) => {
                        let ty = LLVMFloatTypeInContext(context.get_context());
                        Some(LLVMConstReal(ty, *val as f64))
                    },
                    Literals::F64Num(ref val) => {
                        let ty = LLVMDoubleTypeInContext(context.get_context());
                        Some(LLVMConstReal(ty, *val))
                    },
                    Literals::Bool(ref val) => {
                        let ty = LLVMInt1TypeInContext(context.get_context());
                        Some(LLVMConstInt(ty, *val as u64, 0))
                    },
                    Literals::_None => panic!("CodeGen Error: Unimplemented for {:?}", literal_type)
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
                    println!("CodeGen Error: Function {} requires {} argument(s), {} given.", name, arg_count, args.len());
                    return None;
                }

                let mut arg_values = Vec::new();

                for arg in args {
                    match arg.gen_code(context) {
                        Some(value) => arg_values.push(value),
                        None => {
                            println!("Fatal CodeGen Error: Argument {:?} codegen failed!", arg);
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
            Expr::InfixOp(ref op, ref lhs_exprwrapper, ref rhs_exprwrapper) => {
                let (lhs_val, rhs_val) =  match (lhs_exprwrapper.gen_code(context), rhs_exprwrapper.gen_code(context)) {
                    (Some(val1), Some(val2)) => (val1, val2),
                    (Some(_), None) => unreachable!("CodeGen Error: InfixOp only LHS contains value"),
                    (None, Some(_)) => unreachable!("CodeGen Error: InfixOp only RHS contains value"),
                    (None, None) => unreachable!("CodeGen Error: InfixOp has no values")
                };

                // Needs testing, what happens when adding diff types? LLVM error? Is that SA's job?
                match *op {
                    InfixOp::Add => match (lhs_val, rhs_val) {
                        _ => Some(LLVMBuildAdd(context.get_builder(), lhs_val, rhs_val, c_str_ptr("add")))
                    },
                    InfixOp::Sub => match (lhs_val, rhs_val) {
                        _ => Some(LLVMBuildSub(context.get_builder(), lhs_val, rhs_val, c_str_ptr("sub")))
                    },
                    InfixOp::Div => match (lhs_val, rhs_val) {
                        // LLVMBuildFDiv, LLVMBuildSDiv, LLVMBuildUDiv
                        _ => panic!("CodeGen Error: Unimplemented infix operator div")
                    },
                    InfixOp::Mul => match (lhs_val, rhs_val) {
                        _ => Some(LLVMBuildMul(context.get_builder(), lhs_val, rhs_val, c_str_ptr("mul")))
                    },
                    InfixOp::Mod => match (lhs_val, rhs_val) {
                        _ => panic!("CodeGen Error: Unimplemented infix operator mod")
                    },
                    InfixOp::Pow => match (lhs_val, rhs_val) {
                        _ => panic!("CodeGen Error: Unimplemented infix operator pow")
                    },
                    InfixOp::Equ => match (lhs_val, rhs_val) {
                        // LLVMBuildICmp, LLVMBuildFCmp?
                        _ => panic!("CodeGen Error: Unimplemented infix operator equ")
                    },
                }
            },
            // Needs further testing
            Expr::UnaryOp(ref op, ref expr) => {
                match *op {
                    UnaryOp::Negate => match expr.gen_code(context) {
                        Some(val) => Some(LLVMBuildNeg(context.get_builder(), val, c_str_ptr("neg"))),
                        None => None
                    },
                    UnaryOp::Not => match expr.gen_code(context) {
                        Some(val) => Some(LLVMBuildNot(context.get_builder(), val, c_str_ptr("not"))),
                        None => None
                    }
                }
            },
            // TODO: Support constant variables - is that just a SA check?
            Expr::VarDecl(ref _const, ref name, ref val_type, ref expr) => {
                assert!(val_type.is_some(), "CodeGen Error: Variable declaration not given a type by codegen phase");

                // Assign to a literal
                match val_type.as_ref().unwrap().parse::<Types>() {
                    Ok(_) => {
                        match expr.gen_code(context) {
                            Some(val) => {
                                // Couldn't figure out how to not clone this string
                                context.named_values.insert(name.clone(), val);

                                Some(val)
                            },
                            None => None
                        }
                    },
                    // Assign from a custom type
                    Err(_) => panic!("CodeGen Error: Unimplemented var declaration for {}", name)
                }
            },
            Expr::If(ref cond_expr, ref body_expr, ref opt_else_expr) => {
                // Need to know value type (float or int?)

                let cond_val = cond_expr.gen_code(context);
                let body_val = body_expr.gen_code(context);
                let opt_else_val = match opt_else_expr {
                    &Some(ref expr) => Some(expr.gen_code(context)),
                    &None => None
                };

                let block = LLVMGetInsertBlock(context.get_builder());
                let parent_block = LLVMGetBasicBlockParent(block);

                LLVMAppendBasicBlockInContext(context.get_context(), parent_block, c_str_ptr("body"));
                LLVMAppendBasicBlockInContext(context.get_context(), parent_block, c_str_ptr("else"));

                None
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

extern crate llvm_sys;

use std::ptr;
use self::llvm_sys::core::*;
//use self::llvm_sys::*;
use llvm::codegen::{Context, c_str_ptr};

// Generate LLVM IR for functions built into the language
pub fn generate_builtins(context: &mut Context) {
    generate_print(context);
}

fn generate_print(context: &mut Context) {
// Add a print fn that returns nothing (void)
    unsafe {
        let void = LLVMVoidTypeInContext(context.get_context());
        let fn_type = LLVMFunctionType(void, ptr::null_mut(), 0, 0);
        let print_fn = LLVMAddFunction(context.get_module(), c_str_ptr("print"), fn_type);

        // Create a basic block to generate code in
        let bb = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr("entry"));

        LLVMPositionBuilderAtEnd(context.get_builder(), bb);
        LLVMBuildRetVoid(context.get_builder());
    }
}

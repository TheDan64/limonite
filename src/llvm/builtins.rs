extern crate llvm_sys;

use std::ptr;
use self::llvm_sys::core::*;
//use self::llvm_sys::*;
use llvm::codegen::{Context, c_str_ptr};

// Generate LLVM IR for functions built into the language
pub fn generate_builtins(context: &mut Context) {
    generate_print(context);

    // Generate a `main` function for the script to be housed
    unsafe {
        let int32 = LLVMInt32TypeInContext(context.get_context());
        let fn_type = LLVMFunctionType(int32, ptr::null_mut(), 0, 0);
        let main = LLVMAddFunction(context.get_module(), c_str_ptr("main"), fn_type);
        let bb = LLVMAppendBasicBlockInContext(context.get_context(), main, c_str_ptr("entry"));

        // Goes to the end of the block
        LLVMPositionBuilderAtEnd(context.get_builder(), bb);

        // Adds a return statement
        LLVMBuildRet(context.get_builder(), LLVMConstInt(int32, 1, 1));

        // Reposition to the start of the block
        LLVMPositionBuilder(context.get_builder(), bb, LLVMGetFirstInstruction(bb));
    }
}

fn generate_print(context: &mut Context) {
    unsafe {
        let void = LLVMVoidTypeInContext(context.get_context());
        // 2nd arg is param types (*mut LLVMTypeRef) and 3rd is param count. 4 is variable # of args t/f?
        let fn_type = LLVMFunctionType(void, ptr::null_mut(), 0, 0);
        let print_fn = LLVMAddFunction(context.get_module(), c_str_ptr("print"), fn_type);

        // Create a basic block to generate code in
        let bb = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr("entry"));

        LLVMPositionBuilderAtEnd(context.get_builder(), bb);
        LLVMBuildRetVoid(context.get_builder());
    }
}

extern crate llvm_sys;

use std::ptr;
use self::llvm_sys::core::*;
//use self::llvm_sys::*;
use llvm::codegen::{Context, c_str_ptr};

// Generate LLVM IR for functions built into the language
pub unsafe fn generate_builtins(context: &mut Context) {
    generate_types(context);
    generate_print(context);

    // Generate a `main` function for the script to be housed
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

unsafe fn generate_print(context: &mut Context) {
    let void = LLVMVoidTypeInContext(context.get_context());
    let mut string_type_fields = vec![LLVMInt32TypeInContext(context.get_context())];
    let ptr_type = LLVMPointerType(LLVMInt8TypeInContext(context.get_context()), 0);

    string_type_fields.push(ptr_type);

    let string_type = LLVMStructTypeInContext(context.get_context(), string_type_fields.as_ptr() as *mut _, 2, 0);
    let args = vec![string_type];

    // 2nd arg is param types (*mut LLVMTypeRef) and 3rd is param count. 4 is variable # of args t/f?
    let fn_type = LLVMFunctionType(void, args.as_ptr() as *mut _, 1, 0);
    let print_fn = LLVMAddFunction(context.get_module(), c_str_ptr("print"), fn_type);

    // Name the param
    let param = LLVMGetFirstParam(print_fn);
    LLVMSetValueName(param, c_str_ptr("str"));

    // Create a basic block to generate code in
    let bb = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr(""));

    // Generate print code here

    LLVMPositionBuilderAtEnd(context.get_builder(), bb); // Not needed?
    LLVMBuildRetVoid(context.get_builder());
}

unsafe fn generate_types(context: &mut Context) {
    // Define a string structure that looks like:
    // string {
    //     len: i32,
    //     array: i8*
    // }

    let string_type_fields = vec![LLVMInt32TypeInContext(context.get_context()),
                  LLVMPointerType(LLVMInt8TypeInContext(context.get_context()), 0)];

//    let struct_type = LLVMStructType(string_type_fields.as_ptr() as *mut _, 2, 0);
//    let const_str_var = LLVMAddGlobal(context.get_module(), struct_type, c_str_ptr("struct.String"));


    let string_struct_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), c_str_ptr("string"));
    LLVMStructSetBody(string_struct_type, string_type_fields.as_ptr() as *mut _, 2, 0);
//    let string_struct = LLVMStructType(struct_type, 2, 0);
}

extern crate llvm_sys;

use std::ptr;
use self::llvm_sys::core::*;
use self::llvm_sys::*;
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
    // Types
    let void = LLVMVoidTypeInContext(context.get_context());
    let string_type_fields = vec![LLVMInt32TypeInContext(context.get_context()),
                  LLVMPointerType(LLVMInt8TypeInContext(context.get_context()), 0)];
    let int32_type = LLVMInt32TypeInContext(context.get_context());
    let string_type = LLVMStructTypeInContext(context.get_context(), string_type_fields.as_ptr() as *mut _, 2, 0);

    let args = vec![string_type];

    // 2nd arg is param types (*mut LLVMTypeRef) and 3rd is param count. 4 is variable # of args t/f?
    let fn_type = LLVMFunctionType(void, args.as_ptr() as *mut _, 1, 0);
    let print_fn = LLVMAddFunction(context.get_module(), c_str_ptr("print"), fn_type);

    // Name the param
    let param = LLVMGetFirstParam(print_fn);
    LLVMSetValueName(param, c_str_ptr("str"));

    // Create basic blocks to generate code in
    let start_block = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr("start"));
    let loop_block = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr("loop"));
    let end_block = LLVMAppendBasicBlockInContext(context.get_context(), print_fn, c_str_ptr("end"));

    // Start
    LLVMPositionBuilderAtEnd(context.get_builder(), start_block);

    let zero = LLVMConstInt(int32_type, 0, 0);
    let op = LLVMIntPredicate::LLVMIntEQ;
    let offset_ptr = LLVMBuildAlloca(context.get_builder(), int32_type, c_str_ptr("offsetptr"));
    LLVMBuildStore(context.get_builder(), zero, offset_ptr);
    let len = LLVMBuildExtractValue(context.get_builder(), param, 0, c_str_ptr("len"));
    let str_ptr = LLVMBuildExtractValue(context.get_builder(), param, 1, c_str_ptr("strptr"));
    let cmp = LLVMBuildICmp(context.get_builder(), op, len, zero, c_str_ptr("cmp"));

    // Branch to end if string len is 0
    LLVMBuildCondBr(context.get_builder(), cmp, end_block, loop_block);

    // Loop
    LLVMPositionBuilderAtEnd(context.get_builder(), loop_block);

    let offset_val = LLVMBuildLoad(context.get_builder(), offset_ptr, c_str_ptr("offsetsval"));
    let ptr_iter = LLVMBuildAdd(context.get_builder(), str_ptr, offset_val, c_str_ptr("iter"));
    let mut putchar_fn = LLVMGetNamedFunction(context.get_module(), c_str_ptr("putchar"));
    if putchar_fn.is_null() {
        let fn_type2 = LLVMFunctionType(int32_type, vec![int32_type].as_ptr() as *mut _, 1, 0);
        putchar_fn = LLVMAddFunction(context.get_module(), c_str_ptr("putchar"), fn_type2);
    }

    // Print char at ptr_iter here
    LLVMBuildCall(context.get_builder(), putchar_fn, vec![ptr_iter].as_ptr() as *mut _, 1, c_str_ptr(""));

    // TODO: Increment offset_val? (move its init out of the loop?)

    // TODO: If equal to len, branch to end else branch to loop

    LLVMBuildBr(context.get_builder(), end_block);

    // End
    LLVMPositionBuilderAtEnd(context.get_builder(), end_block);
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

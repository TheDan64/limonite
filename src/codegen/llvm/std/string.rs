extern crate llvm_sys; // TODO: Remove. Should fully rely on inkwell eventually
extern crate inkwell;

use self::inkwell::builder::Builder;
use self::inkwell::context::Context;
use self::inkwell::module::Module;
use self::llvm_sys::LLVMIntPredicate; // TODO: Remove
use codegen::llvm::std::vec::vec_type;

pub fn define_string_type(context: &Context) {
    let i8_ptr_type = context.i8_type().ptr_type(0);

    vec_type(&context, i8_ptr_type, "std.string.String")
}

// TODO: Move out of the string file:
pub fn define_print_function(builder: &Builder, context: &Context, module: &Module) {
    // Types
    let void = context.void_type();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let i32_ptr_type = i32_type.ptr_type(0);
    let string_type_ptr = module.get_type("std.string.String").expect("LLVMGenError: Could not find String definition").ptr_type(0);

    let mut args = vec![string_type_ptr];

    let fn_type = void.fn_type(&mut args, false);

    let print_fn = module.add_function("print", fn_type);

    let mut param = print_fn.get_first_param().expect("Print function should have at least one param");

    param.set_name("str");

    // Create basic blocks to generate code in
    let entry_block = context.append_basic_block(&print_fn, "entry");
    let loop_block = context.append_basic_block(&print_fn, "loop");
    let end_block = context.append_basic_block(&print_fn, "end");

    builder.position_at_end(&entry_block);

    let i64_zero = i64_type.const_int(0, false);

    let op = LLVMIntPredicate::LLVMIntEQ; // REVIEW: Shouldn't need to touch LLVM directly

    let offset_ptr = builder.build_stack_allocation(&i64_type, "offsetptr");

    builder.build_store(&i64_zero, &offset_ptr);

    // REVIEW: Maybe we can bake in offsets into a StructType struct so that
    // the following is less manual?
    let str_ptr_ptr = builder.build_gep(&param.as_value(), &vec![0, 0], "strptrptr");
    let str_ptr = builder.build_load(&str_ptr_ptr, "str_ptr");
    let len_ptr = builder.build_gep(&param.as_value(), &vec![0, 1], "len_ptr");
    let len = builder.build_load(&len_ptr, "len");

    let cmp = builder.build_int_compare(op, &len, &i64_zero, "cmp");

    // Branch to end if string len is 0
    builder.build_conditional_branch(&cmp, &end_block, &loop_block);

    // Loop
    builder.position_at_end(&loop_block);

    let offset = builder.build_load(&offset_ptr, "offset");
    let mut indices = vec![offset];

    let iter_ptr = builder.build_gep(&str_ptr, &indices, "iterptr");

    let offset = indices.pop().unwrap();

    let iterptr32 = builder.build_pointer_cast(&iter_ptr, &i32_ptr_type, "iterptr32"); // REVIEW: cast necessary?
    let iter32 = builder.build_load(&iterptr32, "iter");

    let putchar_fn = match module.get_function("putchar") {
        Some(f) => f,
        None => {
            let fn_type2 = i32_type.fn_type(&mut vec![context.i32_type()], false);

            module.add_function("putchar", fn_type2)
        }
    };

    // Print char at ptr_iter here
    builder.build_call(&putchar_fn, &vec![iter32], "putchar");

    // Increment the offset
    let i64_one = i64_type.const_int(1, false);
    let inc = builder.build_int_add(&offset, &i64_one, "inc");

    builder.build_store(&inc, &offset_ptr);

    // Branch if offset equals len, else keep looping
    let op = LLVMIntPredicate::LLVMIntEQ; // REVIEW: LLVM should be hidden
    let cmp2 = builder.build_int_compare(op, &inc, &len, "cmp2");

    builder.build_conditional_branch(&cmp2, &end_block, &loop_block);

    // End
    builder.position_at_end(&end_block);

    let newline = i32_type.const_int('\n' as u64, false);

    builder.build_call(&putchar_fn, &vec![newline], "putchar");

    builder.build_return(None);
}

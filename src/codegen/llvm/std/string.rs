extern crate llvm_sys;

use codegen::llvm::core::{Builder, Context, Module, Type};
use self::llvm_sys::{LLVMOpcode, LLVMIntPredicate}; // TODO: Remove

pub fn string_type(context: &Context) -> Type {
    // TODO: I think real Strings have another field for capacity,
    // so it knows when to reallocate
    let field_types = vec![
        context.i8_type().ptr_type(0),
        context.i64_type(),
    ];

    context.struct_type(field_types)
}

// TODO: Move out of the string file:
pub fn print_function_definition(builder: &Builder, context: &Context, module: &Module) {
    // Types
    let void = context.void_type();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let i32_ptr_type = i32_type.ptr_type(0);
    let string_type = string_type(context);

    let mut args = vec![string_type];

    let fn_type = void.fn_type(&mut args, false);

    let print_fn = module.add_function("print", fn_type);

    let mut param = print_fn.get_first_param();

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
    let str_ptr = builder.build_extract_value(&param, 0, "strptr");
    let len = builder.build_extract_value(&param, 1, "len");

    let cmp = builder.build_int_compare(op, &len, &i64_zero, "cmp");

    // Branch to end if string len is 0
    builder.build_conditional_branch(&cmp, &end_block, &loop_block);

    // Loop
    builder.position_at_end(&loop_block);

    let offset = builder.build_load(&offset_ptr, "offset");
    let mut indices = vec![offset];

    let iter_ptr = builder.build_gep(&str_ptr, &mut indices, "iterptr");

    let offset = indices.pop().unwrap();

    let op = LLVMOpcode::LLVMBitCast; // REVIEW: LLVM shouldn't be exposed
    let iterptr32 = builder.build_cast(op, iter_ptr, i32_ptr_type, "iterptr32");
    let iter32 = builder.build_load(&iterptr32, "iter");

    let putchar_fn = match module.get_function("putchar") {
        Some(f) => f,
        None => {
            let fn_type2 = i32_type.fn_type(&mut vec![context.i32_type()], false);

            module.add_function("putchar", fn_type2)
        }
    };

    // Print char at ptr_iter here
    builder.build_call(&putchar_fn, vec![iter32], "");

    // Increment the offset
    let i64_one = i64_type.const_int(1, false);
    let inc = builder.build_add(&offset, &i64_one, "inc");

    builder.build_store(&inc, &offset_ptr);

    // Branch if offset equals len, else keep looping
    let op = LLVMIntPredicate::LLVMIntEQ; // REVIEW: LLVM should be hidden
    let cmp2 = builder.build_int_compare(op, &inc, &len, "cmp2");

    builder.build_conditional_branch(&cmp2, &end_block, &loop_block);

    // End
    builder.position_at_end(&end_block);

    let newline = i32_type.const_int('\n' as u64, false);

    builder.build_call(&putchar_fn, vec![newline], "");

    builder.build_return(None);

    println!("{:?}", module.get_function("print"));
}

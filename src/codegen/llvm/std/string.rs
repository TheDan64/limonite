use crate::codegen::llvm::TyValCache;
use crate::codegen::llvm::std::vec::vec_type;
use crate::codegen::llvm::{FnDecl, FnType, FnValue, LLVMValue, LLVMType};

use inkwell::{AddressSpace, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};

pub struct LimeString;

impl LLVMType for LimeString {
    const FULL_PATH: &'static str = "std::String";

    fn codegen<'ctx>(context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        string_type(context).into()
    }
}

pub struct PrintString;

impl FnType for PrintString {
    const FULL_PATH: &'static str = "std::print";

    fn build_ty<'ctx>(ctx: &'ctx Context) -> FunctionType<'ctx> {
        let void_type = ctx.void_type();
        let param_types = &[
            // Ideally this would be context.get_type(LimeString::FULL_PATH) but
            // LLVM doesn't allow this from context yet, only via module...
            LimeString::codegen(ctx).ptr_type(AddressSpace::Generic).into(),
        ];

        void_type.fn_type(param_types, false).into()
    }
}

impl FnDecl for PrintString {}

impl FnValue for PrintString {
    fn build_val<'ctx>(builder: &Builder<'ctx>, context: &'ctx Context, print_fn: FunctionValue<'ctx>, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        // Types
        let void = context.void_type();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);
        let string_type_ptr = module.get_type(LimeString::FULL_PATH).expect("LLVMGenError: Could not find String definition").ptr_type(AddressSpace::Generic);
        let i32_zero = i32_type.const_int(0, false);
        let i32_one = i32_type.const_int(1, false);
        let i64_zero = i64_type.const_int(0, false);

        let param = print_fn
            .get_first_param()
            .expect("Print function should have at least one param")
            .into_pointer_value();

        // param.set_name("str");

        // Create basic blocks to generate code in
        let entry_block = context.append_basic_block(print_fn, "entry");
        let loop_block = context.append_basic_block(print_fn, "loop");
        let end_block = context.append_basic_block(print_fn, "end");

        builder.position_at_end(entry_block);

        let op = IntPredicate::EQ;
        let offset_ptr = builder.build_alloca(i64_type, "offsetptr");

        builder.build_store(offset_ptr, i64_zero);

        // REVIEW: Maybe we can bake in offsets into a StructType struct so that
        // the following is less manual?
        let str_ptr_ptr = unsafe { builder.build_gep(param, &[i32_zero, i32_zero], "strptrptr") };
        let str_ptr = builder.build_load(str_ptr_ptr, "str_ptr").into_pointer_value();
        let len_ptr = unsafe { builder.build_gep(param, &[i32_zero, i32_one], "len_ptr") };
        let len = builder.build_load(len_ptr, "len").into_int_value();
        let cmp = builder.build_int_compare(op, len, i64_zero, "cmp");

        // Branch to end if string len is 0
        builder.build_conditional_branch(cmp, end_block, loop_block);

        // Loop
        builder.position_at_end(loop_block);

        let offset = builder.build_load(offset_ptr, "offset").into_int_value();
        let mut indices = vec![offset];

        let iter_ptr = unsafe { builder.build_gep(str_ptr, &indices, "iterptr") };

        let offset = indices.pop().unwrap();

        let iterptr32 = builder.build_pointer_cast(iter_ptr, i32_ptr_type, "iterptr32"); // REVIEW: cast necessary?
        let iter32 = builder.build_load(iterptr32, "iter").into_int_value();

        let putchar_fn = module.get_function("putchar").unwrap();

        // Print char at ptr_iter here
        builder.build_call(putchar_fn, &[iter32.into()], "putchar");

        // Increment the offset
        let i64_one = i64_type.const_int(1, false);
        let inc = builder.build_int_add(offset, i64_one, "inc");

        builder.build_store(offset_ptr, inc);

        // Branch if offset equals len, else keep looping
        let cmp2 = builder.build_int_compare(op, inc, len, "cmp2");

        builder.build_conditional_branch(cmp2, end_block, loop_block);

        // End
        builder.position_at_end(end_block);

        let newline = i32_type.const_int('\n' as u64, false);

        builder.build_call(putchar_fn, &[newline.into()], "putchar");
        builder.build_return(None);

        print_fn.into()
    }
}

// impl LLVMValue for LimeString {
//     fn codegen<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context) -> BasicValueEnum<'ctx> {

//     }
// }

fn string_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
    vec_type(context, context.i8_type().into(), LimeString::FULL_PATH)
}

// TODO: Move out of the string file:
pub fn define_print_function<'ctx>(builder: Builder<'ctx>, context: &'ctx Context, module: Module<'ctx>) {
    // Types
    let void = context.void_type();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);
    let string_type_ptr = module.get_type(LimeString::FULL_PATH).expect("LLVMGenError: Could not find String definition").ptr_type(AddressSpace::Generic);
    let i32_zero = i32_type.const_int(0, false);
    let i32_one = i32_type.const_int(1, false);
    let i64_zero = i64_type.const_int(0, false);

    let args = &[string_type_ptr.into()];

    let fn_type = void.fn_type(args, false);

    let print_fn = module.add_function(LimeString::FULL_PATH, fn_type, None);

    let param = print_fn
        .get_first_param()
        .expect("Print function should have at least one param")
        .into_pointer_value();

    // param.set_name("str");

    // Create basic blocks to generate code in
    let entry_block = context.append_basic_block(print_fn, "entry");
    let loop_block = context.append_basic_block(print_fn, "loop");
    let end_block = context.append_basic_block(print_fn, "end");

    builder.position_at_end(entry_block);

    let op = IntPredicate::EQ;
    let offset_ptr = builder.build_alloca(i64_type, "offsetptr");

    builder.build_store(offset_ptr, i64_zero);

    // REVIEW: Maybe we can bake in offsets into a StructType struct so that
    // the following is less manual?
    let str_ptr_ptr = unsafe { builder.build_gep(param, &[i32_zero, i32_zero], "strptrptr") };
    let str_ptr = builder.build_load(str_ptr_ptr, "str_ptr").into_pointer_value();
    let len_ptr = unsafe { builder.build_gep(param, &[i32_zero, i32_one], "len_ptr") };
    let len = builder.build_load(len_ptr, "len").into_int_value();
    let cmp = builder.build_int_compare(op, len, i64_zero, "cmp");

    // Branch to end if string len is 0
    builder.build_conditional_branch(cmp, end_block, loop_block);

    // Loop
    builder.position_at_end(loop_block);

    let offset = builder.build_load(offset_ptr, "offset").into_int_value();
    let mut indices = vec![offset];

    let iter_ptr = unsafe { builder.build_gep(str_ptr, &indices, "iterptr") };

    let offset = indices.pop().unwrap();

    let iterptr32 = builder.build_pointer_cast(iter_ptr, i32_ptr_type, "iterptr32"); // REVIEW: cast necessary?
    let iter32 = builder.build_load(iterptr32, "iter").into_int_value();

    let putchar_fn = match module.get_function("putchar") {
        Some(f) => f,
        None => {
            let fn_type2 = i32_type.fn_type(&[context.i32_type().into()], false);

            module.add_function("putchar", fn_type2, None)
        }
    };

    // Print char at ptr_iter here
    builder.build_call(putchar_fn, &[iter32.into()], "putchar");

    // Increment the offset
    let i64_one = i64_type.const_int(1, false);
    let inc = builder.build_int_add(offset, i64_one, "inc");

    builder.build_store(offset_ptr, inc);

    // Branch if offset equals len, else keep looping
    let cmp2 = builder.build_int_compare(op, inc, len, "cmp2");

    builder.build_conditional_branch(cmp2, end_block, loop_block);

    // End
    builder.position_at_end(end_block);

    let newline = i32_type.const_int('\n' as u64, false);

    builder.build_call(putchar_fn, &[newline.into()], "putchar");
    builder.build_return(None);
}

use crate::codegen::llvm::std::vec::vec_type;
use crate::codegen::llvm::{FnDecl, FnValue, Type};

use inkwell::{AddressSpace, IntPredicate};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{FunctionType, StructType};
use inkwell::values::{BasicValue, FunctionValue};

pub struct LimeString;

impl<'ctx> Type<'ctx, StructType<'ctx>> for LimeString {
    const FULL_PATH: &'static str = "String";

    fn build_ty(context: &'ctx Context, _module: &Module<'ctx>) -> StructType<'ctx> {
        string_type(context).into()
    }
}

pub struct PrintString;

impl<'ctx> Type<'ctx, FunctionType<'ctx>> for PrintString {
    const FULL_PATH: &'static str = "print";

    fn build_ty(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionType<'ctx> {
        let void_type = ctx.void_type();
        // Ideally this would be context.get_type(LimeString::FULL_PATH) but
        // LLVM doesn't allow this from context yet, only via module... (even
        // though the data lives on the context)
        let str_ty = match module.get_struct_type(LimeString::FULL_PATH) {
            Some(ty) => ty,
            None => LimeString::build_ty(ctx, module),
        };
        let param_types = &[
            str_ty.ptr_type(AddressSpace::Generic).into(),
        ];

        void_type.fn_type(param_types, false).into()
    }
}

impl FnDecl<'_> for PrintString {}

impl<'ctx> FnValue<'ctx> for PrintString {
    fn build_val(context: &'ctx Context, print_fn: FunctionValue<'ctx>, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        let builder = context.create_builder();

        // Types
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);
        let i32_zero = i32_type.const_int(0, false);
        let i32_one = i32_type.const_int(1, false);
        let i64_zero = i64_type.const_int(0, false);

        let param = print_fn
            .get_first_param()
            .expect("Print function should have at least one param")
            .into_pointer_value();

        param.set_name("str");

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

fn string_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
    vec_type(context, context.i8_type().into(), LimeString::FULL_PATH)
}

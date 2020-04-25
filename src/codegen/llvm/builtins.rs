use crate::codegen::llvm::{LLVMFnType, LLVMFnValue};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

pub struct PutcharBuiltin;

impl LLVMFnType for PutcharBuiltin {
    const FULL_PATH: &'static str = "putchar";

    fn build_ty<'ctx>(ctx: &'ctx Context) -> FunctionType<'ctx> {
        let i32_type = ctx.i32_type();

        i32_type.fn_type(&[i32_type.into()], false)
    }
}

impl LLVMFnValue for PutcharBuiltin {
    fn build_val<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context, fn_ty: FunctionType<'ctx>, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        module.add_function(PutcharBuiltin::FULL_PATH, fn_ty, None)
    }
}

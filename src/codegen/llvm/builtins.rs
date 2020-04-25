use crate::codegen::llvm::{FnDecl, FnType};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

pub struct PutcharBuiltin;

impl FnType for PutcharBuiltin {
    const FULL_PATH: &'static str = "putchar";

    fn build_ty<'ctx>(ctx: &'ctx Context) -> FunctionType<'ctx> {
        let i32_type = ctx.i32_type();

        i32_type.fn_type(&[i32_type.into()], false)
    }
}

impl FnDecl for PutcharBuiltin {}

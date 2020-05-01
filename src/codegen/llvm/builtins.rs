use crate::codegen::llvm::{FnDecl, Type};

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;

pub struct PutcharBuiltin;

impl<'ctx> Type<'ctx, FunctionType<'ctx>> for PutcharBuiltin {
    const FULL_PATH: &'static str = "putchar";

    fn build_ty(ctx: &'ctx Context, _module: &Module<'ctx>) -> FunctionType<'ctx> {
        let i32_type = ctx.i32_type();

        i32_type.fn_type(&[i32_type.into()], false)
    }
}

impl FnDecl<'_> for PutcharBuiltin {}

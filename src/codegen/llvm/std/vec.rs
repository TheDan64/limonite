use crate::codegen::llvm::LLVMType;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};

struct LimeVec;

impl LLVMType for LimeVec {
    const FULL_PATH: &'static str = "std::Vec";

    fn codegen<'ctx>(_ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        unimplemented!()
    }
}

pub fn vec_type<'ctx>(context: &'ctx Context, t: BasicTypeEnum<'ctx>, name: &str) -> StructType<'ctx> {
    let field_types = &[
        t.ptr_type(AddressSpace::Generic).into(),
        context.i64_type().into(), // len
        context.i64_type().into(), // cap
    ];

    let struct_type = context.opaque_struct_type(name);

    struct_type.set_body(field_types, false);
    struct_type
}

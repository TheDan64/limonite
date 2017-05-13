use codegen::llvm::core::{Context, Type};

pub fn vec_type(context: &Context, t: Type, type_name: &str) {
    let field_types = vec![
        t,
        context.i64_type(), // len
        context.i64_type(), // cap
    ];

    context.struct_type(field_types, false, type_name);
}

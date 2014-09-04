#[deriving(Show, PartialEq)]
pub enum Types {
    // True, False
    Bool(bool),

    // str
    Str,

    // char
    Char,

    // i32
    Int32Bit,

    // i64
    Int64Bit,

    // u32
    UInt32Bit,

    // u64
    UInt64Bit,

    // f32
    Float32Bit,

    // f64
    Float64Bit,

    // None
    None
}

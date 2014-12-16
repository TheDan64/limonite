use std::str::FromStr;

#[deriving(Copy, Show, PartialEq)]
pub enum Types {
    // bool
    Bool,

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
    NoneType
}

impl FromStr for Types {
    fn from_str(s: &str) -> Option<Types> {
        match s {
            "bool" => Some(Types::Bool),
            "str"  => Some(Types::Str),
            "char" => Some(Types::Char),
            "i32"  => Some(Types::Int32Bit),
            "i64"  => Some(Types::Int64Bit),
            "u32"  => Some(Types::UInt32Bit),
            "u64"  => Some(Types::UInt64Bit),
            "f32"  => Some(Types::Float32Bit),
            "f64"  => Some(Types::Float64Bit),
            "None" => Some(Types::NoneType),
            _      => None
        }
    }
}

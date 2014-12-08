use std::str::FromStr;

#[deriving(Show, PartialEq)]
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
            "bool" => Some(Bool),
            "str"  => Some(Str),
            "char" => Some(Char),
            "i32"  => Some(Int32Bit),
            "i64"  => Some(Int64Bit),
            "u32"  => Some(UInt32Bit),
            "u64"  => Some(UInt64Bit),
            "f32"  => Some(Float32Bit),
            "f64"  => Some(Float64Bit),
            "None" => Some(NoneType),
            _      => None
        }
    }
}

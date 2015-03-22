use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
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
    type Err = ();

    fn from_str(s: &str) -> Result<Types, ()> {
        match s {
            "bool" => Ok(Types::Bool),
            "str"  => Ok(Types::Str),
            "char" => Ok(Types::Char),
            "i32"  => Ok(Types::Int32Bit),
            "i64"  => Ok(Types::Int64Bit),
            "u32"  => Ok(Types::UInt32Bit),
            "u64"  => Ok(Types::UInt64Bit),
            "f32"  => Ok(Types::Float32Bit),
            "f64"  => Ok(Types::Float64Bit),
            "None" => Ok(Types::NoneType),
            _      => Err(())
        }
    }
}

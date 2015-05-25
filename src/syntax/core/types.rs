use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
pub enum Types {
    Bool,
    Str,
    Char,
    Int32Bit,
    Int64Bit,
    UInt32Bit,
    UInt64Bit,
    Float32Bit,
    Float64Bit,
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

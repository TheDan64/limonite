use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
pub enum Types {
    Bool,
    Str,
    Char,
    Int8Bit,
    Int16Bit,
    Int32Bit,
    Int64Bit,
    Int128Bit,
    UInt8Bit,
    UInt16Bit,
    UInt32Bit,
    UInt64Bit,
    UInt128Bit,
    Float32Bit,
    Float64Bit,
    Float128Bit,
    NoneType
}

impl FromStr for Types {
    type Err = ();

    fn from_str(s: &str) -> Result<Types, ()> {
        match s {
            "bool" => Ok(Types::Bool),
            "str"  => Ok(Types::Str),
            "char" => Ok(Types::Char),
            "i8"   => Ok(Types::Int8Bit),
            "i16"  => Ok(Types::Int16Bit),
            "i32"  => Ok(Types::Int32Bit),
            "i64"  => Ok(Types::Int64Bit),
            "i128" => Ok(Types::Int128Bit),
            "u8"   => Ok(Types::UInt8Bit),
            "u16"  => Ok(Types::UInt16Bit),
            "u32"  => Ok(Types::UInt32Bit),
            "u64"  => Ok(Types::UInt64Bit),
            "u128" => Ok(Types::UInt128Bit),
            "f32"  => Ok(Types::Float32Bit),
            "f64"  => Ok(Types::Float64Bit),
            "f128" => Ok(Types::Float128Bit),
            "None" => Ok(Types::NoneType),
            _      => Err(())
        }
    }
}

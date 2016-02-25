use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Literals {
    UTF8String(String),
    UTF8Char(char),
    I32Num(i32),
    I64Num(i64),
    U32Num(u32),
    U64Num(u64),
    F32Num(f32),
    F64Num(f64),
    Bool(bool),
    _None
}

impl fmt::Display for Literals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Literals::UTF8String(_) => "str",
            Literals::UTF8Char(_) => "char",
            Literals::I32Num(_) => "i32",
            Literals::I64Num(_) => "i64",
            Literals::U32Num(_) => "u32",
            Literals::U64Num(_) => "u64",
            Literals::F32Num(_) => "f32",
            Literals::F64Num(_) => "f64",
            Literals::Bool(_) => "bool",
            Literals::_None => "None"
        })
    }
}

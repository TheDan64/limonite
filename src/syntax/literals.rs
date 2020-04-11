// use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'s> {
    UTF8String(&'s str),
    UTF8Char(char),
    I8Num(i8),
    I16Num(i16),
    I32Num(i32),
    I64Num(i64),
    U8Num(u8),
    U16Num(u16),
    U32Num(u32),
    U64Num(u64),
    F32Num(f32),
    F64Num(f64),
    Bool(bool),
    _None
}

// impl fmt::Display for Literals {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", match *self {
//             Literals::UTF8String(_) => "str",
//             Literals::UTF8Char(_) => "char",
//             Literals::I8Num(_) => "i8",
//             Literals::I16Num(_) => "i16",
//             Literals::I32Num(_) => "i32",
//             Literals::I64Num(_) => "i64",
//             Literals::U8Num(_) => "u8",
//             Literals::U16Num(_) => "u16",
//             Literals::U32Num(_) => "u32",
//             Literals::U64Num(_) => "u64",
//             Literals::F32Num(_) => "f32",
//             Literals::F64Num(_) => "f64",
//             Literals::Bool(_) => "bool",
//             Literals::_None => "None"
//         })
//     }
// }

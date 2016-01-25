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

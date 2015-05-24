#[derive(Debug, PartialEq)]
pub enum Literals {
	// UTF-8 Strings, "This is a string"
	UTF8String(String),
	// UTF-8 Char, 'c'
	UTF8Char(char),
	// 32bit signed
	I32Num(i32),
	// 64bit signed
	I64Num(i64),
	// 32bit unsigned
	U32Num(u32),
	// 64bit unsigned
	U64Num(u64),
	// 32bit float
	F32Num(f32),
	// 64bit float
	F64Num(f64),
	// Bool: True or False
	Bool(bool),
	// None
	_None
}

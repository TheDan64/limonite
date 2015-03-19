#[derive(Debug, PartialEq)]
pub enum InfixOp {
	// A + B
	Add,
	// A - B
	Sub,
	// A / B
	Div,
	// A * B
	Mul,
	// A % B
	Mod,
	// A ^ B
	Pow,
	// A equals B (traditionally A == B)
	Equ
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
	// -A
	Negate,
	// not A (traditionally !A)
	Not
}

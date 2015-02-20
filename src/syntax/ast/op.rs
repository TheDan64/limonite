#[derive(Debug, PartialEq)]
pub enum NumOp {
	// A + B
	NumAdd,
	// A - B
	NumSub,
	// A / B
	NumDiv,
	// A * B
	NumMul,
	// A % B
	NumMod,
	// A ^ B
	NumPow
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
	// -A
	UnaryNegate,
	// not A (traditionally !A)
	UnaryNot
}

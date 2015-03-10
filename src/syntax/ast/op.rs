#[derive(Debug, PartialEq)]
pub enum InfixOp {
	// A + B
	InfixAdd,
	// A - B
	InfixSub,
	// A / B
	InfixDiv,
	// A * B
	InfixMul,
	// A % B
	InfixMod,
	// A ^ B
	InfixPow
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
	// -A
	UnaryNegate,
	// not A (traditionally !A)
	UnaryNot
}

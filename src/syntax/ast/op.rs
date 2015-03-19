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

impl InfixOp {
	fn get_precedence(&self) -> usize {
        // Precedence(High -> Low):
        // 9. () | [] .
        // 8. not | negate
        // 7. * / %
        // 6. + -
        // 5. < | <= | > | >=
        // 4. == !=
        // 3. bitwise and | bitwise or | bitwise xor | ^ (pow - not sure where this goes)
        // 2. logical and | logical or
        // 1. ,
		match *self {
			InfixOp::Mul => 7,
			InfixOp::Div => 7,
			InfixOp::Mod => 7,
			InfixOp::Add => 6,
			InfixOp::Sub => 6,
			InfixOp::Equ => 4,
			InfixOp::Pow => 3 // Not sure about this one
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
	// -A
	Negate,
	// not A (traditionally !A)
	Not
}

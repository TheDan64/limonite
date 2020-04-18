use crate::lexical::{Keyword::Equals, Symbol, TokenKind};

use std::convert::TryFrom;

use Symbol::{Asterisk, Caret, GreaterThan, GreaterThanEqual, LessThan, LessThanEqual, Minus, Percent, Plus, PlusEquals, Slash};

#[derive(Clone, Copy, Debug, PartialEq)]
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
    Equ,
    // A < B
    Lt,
    // A <= B
    Lte,
    // A > B
    Gt,
    // A >= B
    Gte,
    // A += B
    AddEq,
}

impl InfixOp {
    pub fn returns_bool(&self) -> bool {
        match *self {
            InfixOp::Lt  => true,
            InfixOp::Lte => true,
            InfixOp::Gt  => true,
            InfixOp::Gte => true,
            InfixOp::Equ => true,
            _ => false,
        }
    }

    pub fn binding_power(&self) -> (u8, u8) {
        // Old list, needs translation to binding power?:
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
        match self {
            InfixOp::Equ | InfixOp::Gt | InfixOp::Gte | InfixOp::Lt | InfixOp::Lte => (2, 1),
            // '?' => (4, 3),
            InfixOp::Add | InfixOp::AddEq | InfixOp::Sub => (5, 6),
            InfixOp::Mul | InfixOp::Div => (7, 8),
            // '.' => (14, 13),
            op => unimplemented!("{:?}", op),
        }
    }
}

impl<'s> TryFrom<TokenKind<'s>> for InfixOp {
    type Error = ();

    fn try_from(tok: TokenKind<'s>) -> Result<Self, Self::Error> {
        match tok {
            TokenKind::Keyword(Equals) => Ok(InfixOp::Equ),
            TokenKind::Symbol(Asterisk) => Ok(InfixOp::Mul),
            TokenKind::Symbol(Caret) => Ok(InfixOp::Pow),
            TokenKind::Symbol(GreaterThan) => Ok(InfixOp::Gt),
            TokenKind::Symbol(GreaterThanEqual) => Ok(InfixOp::Gte),
            TokenKind::Symbol(LessThan) => Ok(InfixOp::Lt),
            TokenKind::Symbol(LessThanEqual) => Ok(InfixOp::Lte),
            TokenKind::Symbol(Minus) => Ok(InfixOp::Sub),
            TokenKind::Symbol(Percent) => Ok(InfixOp::Mod),
            TokenKind::Symbol(Plus) => Ok(InfixOp::Add),
            TokenKind::Symbol(Slash) => Ok(InfixOp::Div),
            TokenKind::Symbol(PlusEquals) => Ok(InfixOp::AddEq),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    // -A
    Negate,
    // not A (traditionally !A)
    Not
}

impl UnaryOp {
    pub fn binding_power(&self) -> ((), u8) {
        match self {
            UnaryOp::Negate => ((), 9),
            UnaryOp::Not => todo!("UnaryOp::Not.binding_power()"),
        }
    }
}

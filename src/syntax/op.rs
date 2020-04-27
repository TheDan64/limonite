use crate::lexical::{Keyword::Equals, Symbol::*, TokenKind};

use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InfixOp {
    // A + B
    Add,
    // A += B
    AddEq,
    // A / B
    Div,
    // A /= B
    DivEq,
    // A equals B (traditionally A == B)
    Equ,
    // A > B
    Gt,
    // A >= B
    Gte,
    // A < B
    Lt,
    // A <= B
    Lte,
    // A % B
    Mod,
    // A %= B
    ModEq,
    // A * B
    Mul,
    // A *= B
    MulEq,
    // A ^ B
    Pow,
    // A - B
    Sub,
    // A -= B
    SubEq,
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

    pub fn as_str(&self) -> &str {
        match self {
            InfixOp::Add => "+",
            InfixOp::AddEq => "+=",
            InfixOp::Div => "/",
            InfixOp::DivEq => "/=",
            InfixOp::Equ => "equals",
            InfixOp::Gt => ">",
            InfixOp::Gte => ">=",
            InfixOp::Lt => "<",
            InfixOp::Lte => "<=",
            InfixOp::Mod => "%",
            InfixOp::ModEq => "%=",
            InfixOp::Mul => "*",
            InfixOp::MulEq => "*=",
            InfixOp::Pow => "^",
            InfixOp::Sub => "-",
            InfixOp::SubEq => "-=",
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
            TokenKind::Symbol(MinusEquals) => Ok(InfixOp::SubEq),
            TokenKind::Symbol(AsteriskEquals) => Ok(InfixOp::MulEq),
            TokenKind::Symbol(SlashEquals) => Ok(InfixOp::DivEq),
            TokenKind::Symbol(PercentEquals) => Ok(InfixOp::ModEq),
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

    pub fn as_str(&self) -> &str {
        match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "not"
        }
    }
}

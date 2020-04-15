use std::str::FromStr;

#[derive(Copy, Debug, PartialEq, Clone)]
pub enum Symbol {
    ParenOpen,
    ParenClose,
    SBracketOpen,
    SBracketClose,
    CBracketOpen,
    CBracketClose,
    Period,
    Comma,
    Colon,
    Caret,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Tilde,
    Equals,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    PercentEquals,
    RightThinArrow,
}

impl FromStr for Symbol {
    type Err = String;

    fn from_str(s: &str) -> Result<Symbol, String> {
        match s {
            "("  => Ok(Symbol::ParenOpen),
            ")"  => Ok(Symbol::ParenClose),
            "["  => Ok(Symbol::SBracketOpen),
            "]"  => Ok(Symbol::SBracketClose),
            "{"  => Ok(Symbol::CBracketOpen),
            "}"  => Ok(Symbol::CBracketClose),
            "."  => Ok(Symbol::Period),
            ","  => Ok(Symbol::Comma),
            ":"  => Ok(Symbol::Colon),
            "^"  => Ok(Symbol::Caret),
            ">"  => Ok(Symbol::GreaterThan),
            "<"  => Ok(Symbol::LessThan),
            "+"  => Ok(Symbol::Plus),
            "-"  => Ok(Symbol::Minus),
            "*"  => Ok(Symbol::Asterisk),
            "/"  => Ok(Symbol::Slash),
            "%"  => Ok(Symbol::Percent),
            "~"  => Ok(Symbol::Tilde),
            "="  => Ok(Symbol::Equals),
            ">=" => Ok(Symbol::GreaterThanEqual),
            "<=" => Ok(Symbol::LessThanEqual),
            "+=" => Ok(Symbol::PlusEquals),
            "-=" => Ok(Symbol::MinusEquals),
            "*=" => Ok(Symbol::AsteriskEquals),
            "/=" => Ok(Symbol::SlashEquals),
            "%=" => Ok(Symbol::PercentEquals),
            "->" => Ok(Symbol::RightThinArrow),
            s    => Err(format!("Invalid Symbol Token match: {}", s))
        }
    }
}

impl Symbol {
    pub fn binding_power(&self) -> (Option<u8>, u8) {
        match self {
            Symbol::Minus => (None, 0), // left bp not valid for unary op..
            _ => unimplemented!(),
        }
    }
}

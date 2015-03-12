use std::str::FromStr;

#[derive(Copy, Debug, PartialEq)]
pub enum Symbols {
    // '('
    ParenOpen,

    // ')'
    ParenClose,

    // '['
    SBracketOpen,

    // ']'
    SBracketClose,

    // '{'
    CBracketOpen,

    // '}'
    CBracketClose,

    // '.'
    Period,

    // ','
    Comma,

    // ':'
    Colon,

    // '^'
    Caret,

    // '>'
    GreaterThan,

    // '<'
    LessThan,

    // ">="
    GreaterThanEqual,

    // "<="
    LessThanEqual,

    // '+'
    Plus,

    // '-'
    Minus,

    // '*'
    Asterisk,

    // '/'
    Slash,

    // '%'
    Percent,

    // '~'
    Tilde,

    // '='
    Equals,

    // "+="
    PlusEquals,

    // "-="
    MinusEquals,

    // "*="
    AsteriskEquals,

    // "/="
    SlashEquals,

    // "%="
    PercentEquals,

    // "->"
    RightThinArrow
}

impl FromStr for Symbols {
    type Err = String;

    fn from_str(s: &str) -> Result<Symbols, String> {
        match s {
            "("  => Ok(Symbols::ParenOpen),
            ")"  => Ok(Symbols::ParenClose),
            "["  => Ok(Symbols::SBracketOpen),
            "]"  => Ok(Symbols::SBracketClose),
            "{"  => Ok(Symbols::CBracketOpen),
            "}"  => Ok(Symbols::CBracketClose),
            "."  => Ok(Symbols::Period),
            ","  => Ok(Symbols::Comma),
            ":"  => Ok(Symbols::Colon),
            "^"  => Ok(Symbols::Caret),
            ">"  => Ok(Symbols::GreaterThan),
            "<"  => Ok(Symbols::LessThan),
            "+"  => Ok(Symbols::Plus),
            "-"  => Ok(Symbols::Minus),
            "*"  => Ok(Symbols::Asterisk),
            "/"  => Ok(Symbols::Slash),
            "%"  => Ok(Symbols::Percent),
            "~"  => Ok(Symbols::Tilde),
            "="  => Ok(Symbols::Equals),
            ">=" => Ok(Symbols::GreaterThanEqual),
            "<=" => Ok(Symbols::LessThanEqual),
            "+=" => Ok(Symbols::PlusEquals),
            "-=" => Ok(Symbols::MinusEquals),
            "*=" => Ok(Symbols::AsteriskEquals),
            "/=" => Ok(Symbols::SlashEquals),
            "%=" => Ok(Symbols::PercentEquals),
            "->" => Ok(Symbols::RightThinArrow),
            s    => Err(format!("Invalid Symbol Token match: {}", s))
        }
    }
}

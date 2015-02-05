use std::str::FromStr;

#[derive(Copy, Debug, PartialEq)]
pub enum Punctuations {
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

impl FromStr for Punctuations {
    type Err = String;

    fn from_str(s: &str) -> Result<Punctuations, String> {
        match s {
            "("  => Ok(Punctuations::ParenOpen),
            ")"  => Ok(Punctuations::ParenClose),
            "["  => Ok(Punctuations::SBracketOpen),
            "]"  => Ok(Punctuations::SBracketClose),
            "{"  => Ok(Punctuations::CBracketOpen),
            "}"  => Ok(Punctuations::CBracketClose),
            "."  => Ok(Punctuations::Period),
            ","  => Ok(Punctuations::Comma),
            ":"  => Ok(Punctuations::Colon),
            ">"  => Ok(Punctuations::GreaterThan),
            "<"  => Ok(Punctuations::LessThan),
            "+"  => Ok(Punctuations::Plus),
            "-"  => Ok(Punctuations::Minus),
            "*"  => Ok(Punctuations::Asterisk),
            "/"  => Ok(Punctuations::Slash),
            "%"  => Ok(Punctuations::Percent),
            "~"  => Ok(Punctuations::Tilde),
            "="  => Ok(Punctuations::Equals),
            ">=" => Ok(Punctuations::GreaterThanEqual),
            "<=" => Ok(Punctuations::LessThanEqual),
            "+=" => Ok(Punctuations::PlusEquals),
            "-=" => Ok(Punctuations::MinusEquals),
            "*=" => Ok(Punctuations::AsteriskEquals),
            "/=" => Ok(Punctuations::SlashEquals),
            "%=" => Ok(Punctuations::PercentEquals),
            "->" => Ok(Punctuations::RightThinArrow),
            s    => Err(format!("Invalid Punctuation Token match: {}", s))
        }
    }
}

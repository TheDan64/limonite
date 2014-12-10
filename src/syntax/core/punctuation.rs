use std::str::FromStr;

#[deriving(Show, PartialEq)]
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
    fn from_str(s: &str) -> Option<Punctuations> {
        match s {
            "("  => Some(Punctuations::ParenOpen),
            ")"  => Some(Punctuations::ParenClose),
            "["  => Some(Punctuations::SBracketOpen),
            "]"  => Some(Punctuations::SBracketClose),
            "{"  => Some(Punctuations::CBracketOpen),
            "}"  => Some(Punctuations::CBracketClose),
            "."  => Some(Punctuations::Period),
            ","  => Some(Punctuations::Comma),
            ":"  => Some(Punctuations::Colon),
            ">"  => Some(Punctuations::GreaterThan),
            "<"  => Some(Punctuations::LessThan),
            "+"  => Some(Punctuations::Plus),
            "-"  => Some(Punctuations::Minus),
            "*"  => Some(Punctuations::Asterisk),
            "/"  => Some(Punctuations::Slash),
            "%"  => Some(Punctuations::Percent),
            "~"  => Some(Punctuations::Tilde),
            "="  => Some(Punctuations::Equals),
            ">=" => Some(Punctuations::GreaterThanEqual),
            "<=" => Some(Punctuations::LessThanEqual),
            "+=" => Some(Punctuations::PlusEquals),
            "-=" => Some(Punctuations::MinusEquals),
            "*=" => Some(Punctuations::AsteriskEquals),
            "/=" => Some(Punctuations::SlashEquals),
            "%=" => Some(Punctuations::PercentEquals),
            "->" => Some(Punctuations::RightThinArrow),
            _    => None
        }
    }
}

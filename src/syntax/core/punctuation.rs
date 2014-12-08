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

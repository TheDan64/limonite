use std::from_str::FromStr;

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
            "+=" => Some(PlusEquals),
            "-=" => Some(MinusEquals),
            "*=" => Some(AsteriskEquals),
            "/=" => Some(SlashEquals),
            "%=" => Some(PercentEquals),
            "->" => Some(RightThinArrow),
            _    => None
        }
    }
}

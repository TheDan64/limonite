#[deriving(Show)]
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

    // '>='
    GreaterThanEqual,

    // '<='
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

    // '+='
    PlusEquals,

    // '-='
    MinusEquals,

    // '*='
    AsteriskEquals,

    // '/='
    SlashEquals,

    // '%='
    PercentEquals
}

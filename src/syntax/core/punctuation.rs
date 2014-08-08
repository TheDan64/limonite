use std::fmt::Show;

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
    Multiply,

    // '/'
    Divide,

    // '%'
    Modulus,

    // '~'
    Negate,

    // '='
    Assign,

    // '+='
    AddAssign,

    // '-='
    MinusAssign,

    // '*='
    MultiplyAssign,

    // '/='
    DivideAssign,

    // '%='
    ModulusAssign
}

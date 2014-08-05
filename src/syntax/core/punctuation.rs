use std::fmt::Show;

#[deriving(Show)]
pub enum Punctuation {
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

    // ';'
    SemiColon,

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

    // '++'
    Increment,

    // '--'
    Decrement,

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

use std::fmt::Show;
use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuation;

#[deriving(Show)]
pub enum Token {
    // Depricated: to be phased out with next parser update.
    Start,

    // Begin a line with this token
    LineBegin,

    // True, False
    BooleanLiteral(bool),

    // 42, 42u, 0x2A, 0b101010, -42.0, 42f ...
    Numeric(String),

    // Variables, fn names
    Identifier(String),

    // Keep track of scope via indentation
    Indent,
    Dedent(uint),

    // Reserved words
    Keyword(Keywords),

    // (,),[,],:,:,>,<, ...
    Punctuation(Punctuation),

    // >> Singleline and >>> \nMultiline comments\n <<<
    CommentStart(String),
    CommentEnd,

    // End of File
    EOF
}

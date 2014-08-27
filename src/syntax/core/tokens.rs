use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;

#[deriving(Show, PartialEq)]
pub enum Token {

    // True, False
    BooleanLiteral(bool),

    // 42, 42u, 0x2A, 0b101010, -42.0, 42f ...
    Numeric(String),

    // Variables, fn names
    Identifier(String),

    // Count the number of tabs after a newline
    Indent(u32),

    // 'c'
    Char(char),

    // "This is a string"
    Str(String),

    // Reserved words
    Keyword(Keywords),

    // (,),[,],:,:,>,<, ...
    Punctuation(Punctuations),

    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(String),

    // Error message
    Error(String),

    // End of File
    EOF
}

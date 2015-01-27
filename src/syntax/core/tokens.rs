use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;
use syntax::core::types::Types;

#[derive(Show, PartialEq)]
pub enum Token {
    // 42, 42, 0x2A, 0b101010, -42.0, 42 ... and a suffix
    Numeric(String, Option<Types>),

    // Variables, fn names
    Identifier(String),

    // Count the number of tabs after a newline
    Indent(usize),

    // True, False
    BoolLiteral(bool),

    // 'c'
    CharLiteral(char),

    // "This is a string"
    StrLiteral(String),

    // Reserved words
    Keyword(Keywords),

    // (,),[,],:,:,>,<, ...
    Punctuation(Punctuations),

    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(String),

    // Error message
    Error(String),

    // Types: i64, f32, None, ..
    Type(Types),

    // End of File
    EOF
}

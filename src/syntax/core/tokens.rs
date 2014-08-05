use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Show;

use syntax::core::keywords::Keywords;

#[deriving(Show)]
pub enum Token {
    // Just for initializing the lexer
    Start,

    // Variables, fn names
    Identifier(String),

    // Keep track of whitespace
    Indent(u64),

    // Reserved words
    Keyword(Keywords),

    // Numeric Literals
    Number(String),

    // >> Comments. Eventually multiline as well
    //Comment(String),

    // End of file
    EOF
}

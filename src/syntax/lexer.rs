#![allow(dead_code)]
#![feature(globs)]

pub mod core {
    pub mod keywords;
}


pub enum Token {
    // True, False
    BooleanLiteral(bool),

    // Variables, fn names
    Identifier(String),

    // Reserved words
    Keyword(core::keywords::Keyword),

    // >> Comments. Eventually multiline as well
    Comment(String),

    // End of file
    EOF
}



pub struct Lexer<B> {
    pub token : Token,
    pub line_number : uint,
    pub column_number : uint,
    buffer : B
}
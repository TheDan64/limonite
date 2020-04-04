use crate::lexical::types::Type;
use crate::span::Spanned;

pub type Token = Spanned<Type>;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // 42, 42, 0x2A, 0b101010, -42.0, 42 ... and a suffix
    Numeric(String, Option<Type>),
    // Variables, fn names
    Identifier(String),
    // Count the number of tabs after a newline
    Indent(u64),
    // True, False
    BoolLiteral(bool),
    // 'c'
    CharLiteral(char),
    // "This is a string"
    StrLiteral(String),
    // Reserved words
    // Keyword(Keywords),
    // (,),[,],:,:,>,<, ...
    // Symbol(Symbols),
    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(String),
    // Error message
    // Error(String),
}

use crate::lexical::keywords::Keyword;
use crate::lexical::types::Type;
use crate::span::Spanned;

pub type Token<'s> = Spanned<TokenKind<'s>>;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind<'s> {
    // 42, 42, 0x2A, 0b101010, -42.0, 42 ... and a suffix
    Numeric(String, Option<Type>),
    // Variables, fn names
    Identifier(&'s str),
    // Count the number of tabs after a newline
    Indent(Spanned<&'s str>, u64),
    // True, False
    BoolLiteral(bool),
    // 'c'
    CharLiteral(char),
    // "This is a string"
    StrLiteral(String),
    // Reserved words
    Keyword(Keyword),
    // (,),[,],:,:,>,<, ...
    // Symbol(Symbols),
    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(CommentKind<'s>),
    // Error message
    // Error(String),
}

// TODO: Move elsewhere?
#[derive(Debug, PartialEq, Clone)]
pub enum CommentKind<'s> {
    Single(Spanned<&'s str>),
    Multi(String),
}

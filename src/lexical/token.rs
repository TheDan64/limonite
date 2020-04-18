use crate::lexical::keywords::Keyword;
use crate::lexical::symbols::Symbol;
use crate::span::Spanned;

pub type Token<'s> = Spanned<TokenKind<'s>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'s> {
    // 42, 42, 0x2A, 0b101010, -42.0, 42 ... and an optional suffix
    Numeric(Spanned<&'s str>, Option<Spanned<&'s str>>),
    // Variables, fn names
    Identifier(&'s str),
    // Count the number of tabs after a newline
    Indent(u32),
    // True, False
    BoolLiteral(bool),
    // 'c'
    CharLiteral(char),
    // "This is a string"
    StrLiteral(&'s str),
    // Reserved words
    Keyword(Keyword),
    // (,),[,],:,:,>,<, ...
    Symbol(Symbol),
    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(CommentKind<'s>),
}

// TODO: Move elsewhere?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CommentKind<'s> {
    Single(Spanned<&'s str>),
    Multi(Spanned<&'s str>),
}

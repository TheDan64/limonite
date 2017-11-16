use lexical::keywords::Keywords;
use lexical::symbols::Symbols;
use lexical::types::Types;

#[derive(Debug, PartialEq, Clone)]
pub enum Tokens {
    // 42, 42, 0x2A, 0b101010, -42.0, 42 ... and a suffix
    Numeric(String, Option<Types>),
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
    Keyword(Keywords),
    // (,),[,],:,:,>,<, ...
    Symbol(Symbols),
    // >> Singleline and >>> \nMultiline comments\n <<<
    Comment(String),
    // Error message
    Error(String),
}

impl Tokens {
    pub fn expect(&self, token: Tokens) -> bool {
        *self == token
    }
}

use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuation;

pub enum Token {
    // Just for initializing the lexer
    Start,

    // True, False
    BooleanLiteral(bool),

    // Variables, fn names
    Identifier(String),

    // Keep track of whitespace
    Indent,

    // Reserved words
    Keyword(Keywords),

    // (,),[,],:,:,>,<, ...
    Punctuation(Punctuation),

    // >> Comments. Eventually multiline as well
    //Comment(String),

    // End of file
    EOF
}

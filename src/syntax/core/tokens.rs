use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuation;

pub enum Token {
    // Begin a line with this token
    LineBegin,

    // True, False
    BooleanLiteral(bool),

    // Variables, fn names
    Identifier(String),

    // Keep track of scope via indentation
    Indent,
    Dedent(size),

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

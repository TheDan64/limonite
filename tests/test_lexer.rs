extern crate limonite;

use std::io::File;
use std::io::BufferedReader;
use limonite::syntax::lexer::{Lexer, Tokenizer};
use limonite::syntax::core::tokens::{Token, BooleanLiteral, Comment, Keyword, Identifier, Indent, Punctuation, Str, EOF};
use limonite::syntax::core::keywords::{If, Print};
use limonite::syntax::core::punctuation::{Comma, ParenOpen, ParenClose};

fn cmp_tokens(mut lexer: Lexer, vec: Vec<Token>) {
    let mut tok: Token;

    for desired_tok in vec.iter() {
        tok = lexer.get_tok();
 
        if tok == *desired_tok { continue; }

        fail!(format!("Unexpected token `{0}` found. Expected `{1}`.", tok, desired_tok));
    }
}

#[test]
fn test_hello_world() {
    let path = Path::new("tests/lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    let desired_output = vec![Comment(" Hello World!".to_string()), Indent(0),
                              Indent(0),
                              Keyword(Print), Punctuation(ParenOpen), Str("Hello World!".to_string()), Punctuation(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_indentation() {
    let path = Path::new("tests/lang/test_indentation.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    let desired_output = vec![Comment("\n    Test of indentation and a few keywords.\n".to_string()), Indent(0),
                              Indent(0),
                              Keyword(If), BooleanLiteral(true), Punctuation(Comma), Indent(1),
                              Identifier("func".to_string()), Punctuation(ParenOpen), Punctuation(ParenClose), Indent(0),
                              Indent(1),
                              Keyword(If), BooleanLiteral(false), Punctuation(Comma), Indent(2),
                              Identifier("func2".to_string()), Punctuation(ParenOpen), Punctuation(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

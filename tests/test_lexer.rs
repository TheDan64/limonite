extern crate limonite;

use std::io::File;
use std::io::BufferedReader;
use limonite::syntax::lexer::{Lexer, Tokenizer};
use limonite::syntax::core::tokens::{Token, Comment, Keyword, Identifier, Indent, Punctuation, Str, EOF};
use limonite::syntax::core::keywords::{Print};
use limonite::syntax::core::punctuation::{ParenOpen, ParenClose};

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
                              Comment(" Let's compile!".to_string()), Indent(0),
                              Indent(0),
                              Keyword(Print), Punctuation(ParenOpen), Str("Hello World!".to_string()), Punctuation(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

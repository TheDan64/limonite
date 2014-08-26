extern crate limonite;

use std::io::File;
use std::io::BufferedReader;
use limonite::syntax::lexer::{Lexer, Tokenizer};
use limonite::syntax::core::tokens;

#[test]
fn test_hello_world() {
    let path = Path::new("tests/lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));
    let mut lexer = Lexer::new(file);

    loop {
        match lexer.get_tok() {
            tokens::EOF => break,
            _ => continue
        }
    }

}

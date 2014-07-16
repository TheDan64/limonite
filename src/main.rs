#![allow(unused_variable)] //temp

use std::io::File;
use std::io::BufferedReader;

pub mod syntax {
    pub mod lexer;
}

fn main() {
	// Example lexer usage
    let path = Path::new("tests/lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));

    let mut lexer = syntax::lexer::Lexer::new(file);
    let token = lexer.get_tok();
}

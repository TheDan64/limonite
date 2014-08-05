#![allow(unused_variable)] //temp
#![allow(unused_imports)] //temp

use std::io::File;
use std::io::BufferedReader;

use syntax::lexer::Lexer;

#[test]
fn open_file() {
    let path = Path::new("lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));

    let mut lexer = Lexer::new(file);
    let token = lexer.get_tok();
}

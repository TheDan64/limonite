#![crate_name = "limonite"]
#![crate_type = "bin"]
#![feature(macro_rules)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use std::io::BufferedReader;
use std::io::File;

use syntax::lexer::Lexer;
use syntax::parser::Parser;

pub mod syntax;

fn main() {
    debug!("AASDF");
    let filename = "tests/lang/test_hello_world.lim";
    let path = Path::new(filename);
    let file = match File::open(&path) {
        Ok(f)  => f,
        Err(e) => fail!("Failed to open file. File error: {}", e),
    };
    let input_string = BufferedReader::new(file).read_to_string().unwrap();

    let lexer = Lexer::new(input_string.as_slice());
    let mut parser = Parser::new(lexer);
    parser.parse()
}

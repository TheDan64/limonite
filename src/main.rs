#![crate_name = "limonite"]
#![crate_type = "bin"]
#![feature(macro_rules)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use std::os;
use std::io::BufferedReader;
use std::io::File;

use syntax::lexer::Lexer;
use syntax::parser::Parser;

pub mod syntax;

// ToDo: Fill this out with usage and version info?
fn display_info() {
    // Display stuff about limonite.
    println!("Usage: limonite [FILE]");
}

fn main() {
    match os::args().len() {
        0 ... 1 => return display_info(),
        2       => (),
        _       => return // Invalid?
    }

    let ref file_name = os::args()[1];
    let path = Path::new(file_name);
    let file = match File::open(&path) {
        Ok(f)  => f,
        Err(e) => panic!("Failed to open file. File error: {}", e)
    };

    let input_string = BufferedReader::new(file).read_to_string().unwrap();

    let lexer = Lexer::new(input_string.as_slice());
    let mut parser = Parser::new(lexer);

    parser.parse()
}

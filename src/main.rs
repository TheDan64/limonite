#![crate_name = "limonite"]
#![crate_type = "bin"]
#![feature(collections)]
#![allow(dead_code)]

use std::env;
use std::io::{BufReader, Read};
use std::fs::File;
use std::path::Path;

use syntax::lexer::Lexer;
use syntax::parser::Parser;

pub mod syntax;

// ToDo: Fill this out with usage and version info?
fn display_info() {
    // Display stuff about limonite.
    println!("Usage: limonite [FILE]");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return display_info();
    }

    let ref file_name = args[1];
    let path = Path::new(file_name);
    let file = match File::open(&path) {
        Ok(f)  => f,
        Err(e) => panic!("Failed to open file. File error: {}", e)
    };

    let mut input_string = String::new();
    if let Err(e) = BufReader::new(file).read_to_string(&mut input_string) {
        panic!("{}", e);
    }

    let lexer = Lexer::new(&input_string);
    let mut parser = Parser::new(lexer, true);

    parser.parse();
}

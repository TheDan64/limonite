#![crate_name = "limonite"]
#![crate_type = "bin"]

use std::env;
use std::old_io::BufferedReader;
use std::old_io::File;

use syntax::lexer::Lexer;
use syntax::parser::Parser;

pub mod syntax;

// ToDo: Fill this out with usage and version info?
fn display_info() {
    // Display stuff about limonite.
    println!("Usage: limonite [FILE]");
}

fn main() {
    let args: Vec<String> = env::args().map(|x| x.into_string().unwrap()).collect();

    if args.len() != 2 {
        return display_info();
    }

    let ref file_name = args[1];
    let path = Path::new(file_name);
    let file = match File::open(&path) {
        Ok(f)  => f,
        Err(e) => panic!("Failed to open file. File error: {}", e)
    };

    let input_string = BufferedReader::new(file).read_to_string().unwrap();

    let lexer = Lexer::new(&input_string[]);
    let mut parser = Parser::new(lexer);

    parser.parse();
}

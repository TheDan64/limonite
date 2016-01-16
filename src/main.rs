#![crate_name = "limonite"]
#![crate_type = "bin"]
#![allow(dead_code)]

extern crate docopt;
extern crate rustc_serialize;
#[macro_use]
extern crate log;
extern crate env_logger;

use std::env;
use std::io::{BufReader, Read};
use std::fs::File;
use std::path::Path;
use docopt::Docopt;
use syntax::lexer::Lexer;
use syntax::parser::Parser;
use codegen::codegen::codegen;

pub mod syntax;
pub mod codegen;

static USAGE: &'static str = "\
Usage: limonite <file>
       limonite (-s | --stdin)
       limonite (-v | --version)

Options:
    -h, --help      Display this message
    -s, --stdin     Read input from stdin
    -v, --version   Displays current version
";

#[derive(RustcDecodable)]
struct Args {
    pub arg_file: String,
    pub flag_stdin: bool,
    pub flag_version: bool
}

fn main() {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());

    // TODO: Compile the version in from the .toml
    if args.flag_version {
        let version = env!("CARGO_PKG_VERSION");

        return println!("limonite {}", version);
    }

    let input_string = if !args.flag_stdin {
        // Read from file
        let ref file_name = &args.arg_file;
        let path = Path::new(file_name);
        let file = match File::open(&path) {
            Ok(f)  => f,
            Err(e) => panic!("Failed to open file. File error: {}", e)
        };

        let mut input_string = String::new();
        if let Err(e) = BufReader::new(file).read_to_string(&mut input_string) {
            panic!(e);
        }

        input_string
    } else {
        // Read from stdin
        let mut input_string = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut input_string) {
            panic!(e);
        }

        input_string
    };

    // Tokanize the input
    let lexer = Lexer::new(&input_string);

    // Parse & Build an AST
    let mut parser = Parser::new(lexer);

    // Could just unwrap this?:
    let ast_root = match parser.parse() {
        Some(ast) => ast,
        None => return,
    };

    // TODO: Semantic Analysis

    // Run Code Gen
    unsafe {
        codegen("module1", ast_root, true);
    }
}

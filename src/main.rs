#![crate_name = "limonite"]
#![crate_type = "bin"]
#![allow(dead_code)]
use std::env;
use std::io::{BufReader, Read};
use std::fs::File;
use std::path::Path;
use syntax::lexer::Lexer;
use syntax::parser::Parser;
use llvm::codegen::{CodeGen, Context};
use llvm::builtins::generate_builtins;

pub mod syntax;
pub mod llvm;

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

    // Tokanize the input
    let lexer = Lexer::new(&input_string);

    // Parse & Build an AST
    let mut parser = Parser::new(lexer);

    let ast_root = match parser.parse() {
        Some(ast) => ast,
        None => return,
    };

    // ToDo: Semantic Analysis
    // Run Code Gen
    // ToDo: Add a flag for disabling code gen
    let mut context = Context::new("module1");

    generate_builtins(&mut context);
    ast_root.gen_code(&mut context);

    // ToDo: Add a flag for dumping ir to stdout

    context.dump();
}

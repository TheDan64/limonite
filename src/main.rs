#![crate_name = "limonite"]
#![crate_type = "bin"]
#![feature(collections)]
#![allow(dead_code)]
extern crate rustc;

use self::rustc::llvm::{LLVMContextCreate, LLVMModuleCreateWithNameInContext,
                        LLVMCreateBuilderInContext, LLVMDumpModule, LLVMDisposeBuilder};
use llvm::codegen::CodeGen;
use std::env;
use std::io::{BufReader, Read};
use std::fs::File;
use std::path::Path;
use syntax::lexer::Lexer;
use syntax::parser::Parser;

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
    let ast_root = parser.parse();

    // ToDo: Semantic Analysis

    // Avoid going to code gen when generating invalid syntax
    if !parser.generated_valid_syntax() {
        return;
    }

    // Run Code Gen
    // ToDo: Add a flag for disabling code gen?
    unsafe {
        let module_name = concat!("module1", "\0").as_ptr() as *const i8;
        let llvm_context = LLVMContextCreate();
        let llvm_module = LLVMModuleCreateWithNameInContext(module_name, llvm_context);
        let builder = LLVMCreateBuilderInContext(llvm_context);

        ast_root.gen_code(builder);

        // ToDo: Add a flag for dumping ir to stdout?
        LLVMDumpModule(llvm_module);
        LLVMDisposeBuilder(builder);
    }
}

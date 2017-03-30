#![crate_name = "limonite"]
#![crate_type = "bin"]

extern crate docopt;
extern crate rustc_serialize;
#[macro_use]
extern crate log;
extern crate env_logger;

use std::io::{BufReader, Read};
use std::fs::File;
use std::path::Path;
use docopt::Docopt;

use lexical::lexer::Lexer;
use syntax::parser::Parser;
use semantic::analyzer::SemanticAnalyzer;
use semantic::analyzer_trait::ASTAnalyzer;
#[cfg(feature="llvm-backend")]
use codegen::llvm::codegen::codegen; // REVIEW: better codegen name convention + class?
#[cfg(feature="llvm-backend")]
use codegen::llvm::LLVMGenerator;

pub mod lexical;
pub mod syntax;
pub mod semantic;
pub mod codegen;

static USAGE: &'static str = "\
Usage: limonite <file>
       limonite (-d | --dump) <file>
       limonite (-s | --stdin)
       limonite (-v | --version)

Options:
    -d, --dump      Dumps LLVM IR
    -h, --help      Display this message
    -s, --stdin     Read input from stdin
    -v, --version   Displays current version
";

#[derive(RustcDecodable)]
struct Args {
    pub arg_file: String,
    pub flag_dump: bool,
    pub flag_stdin: bool,
    pub flag_version: bool
}

fn main() {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());

    if args.flag_version {
        let version = env!("CARGO_PKG_VERSION");

        return println!("limonite {}", version);
    }

    let input_string = if !args.flag_stdin {
        let ref file_name = &args.arg_file;
        let path = Path::new(file_name);
        let file = match File::open(&path) {
            Ok(f)  => f,
            Err(e) => panic!("Failed to open file: {}", e)
        };

        readable_to_string(BufReader::new(file))
    } else {
        readable_to_string(std::io::stdin())
    };

    // Tokanize the input
    let lexer = Lexer::new(&input_string);

    // Parse & Build an AST
    let mut parser = Parser::new(lexer);

    let mut ast_root = match parser.parse() {
        Some(ast) => ast,
        None => return,
    };

    // TODO: Semantic Analysis
    let mut semantic_analyzer = SemanticAnalyzer::new();
    semantic_analyzer.analyze(&mut ast_root);

    // Run Code Gen
    #[cfg(feature="llvm-backend")]
    // unsafe {
    //     codegen("module1", &ast_root, args.flag_dump);
    // }

    #[cfg(feature="llvm-backend")]
    {
        let mut generator = LLVMGenerator::new();

        generator.add_main_module(&ast_root);

        if args.flag_dump {
            generator.dump_ir();
        }

        generator.run();
    }
}

fn readable_to_string<R: Read>(mut readable: R) -> String {
    let mut input_string = String::new();

    if let Err(e) = readable.read_to_string(&mut input_string) {
        panic!("Failed to read: {}", e);
    }

    input_string
}

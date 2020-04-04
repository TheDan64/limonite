use structopt::clap::ArgGroup;
use structopt::StructOpt;

use std::io::{BufReader, Read};
use std::fs::File;
use std::path::PathBuf;

use lexical::lexer_new::Lexer;
use interner::Interner;
// use syntax::parser::Parser;
// use semantic::analyzer::SemanticAnalyzer;
// use semantic::analyzer_trait::ASTAnalyzer;
// #[cfg(feature="llvm-backend")]
// use codegen::llvm::LLVMGenerator;

pub mod interner;
pub mod lexical;
pub mod span;
pub mod syntax;
pub mod semantic;
pub mod codegen;

#[derive(Debug, StructOpt)]
#[structopt(name = "limc", group = ArgGroup::with_name("file_or_stdin").required(true))]
struct Opt {
    /// Input file
    #[structopt(name = "FILE", parse(from_os_str), group = "file_or_stdin")]
    file: Option<PathBuf>,

    /// Dumps internal info
    #[structopt(short, long)]
    dump: bool,

    /// Read from stdin rather than file
    #[structopt(short, long, group = "file_or_stdin")]
    stdin: bool,
}

fn main() {
    env_logger::init();

    let opt = Opt::from_args();

    dbg!(&opt);

    let input_string = if !opt.stdin {
        let file = match File::open(opt.file.as_ref().unwrap()) {
            Ok(f)  => f,
            Err(e) => panic!("Failed to open file: {}", e)
        };
        readable_to_string(BufReader::new(file))
    } else {
        readable_to_string(std::io::stdin())
    };

    // Create a string interner and insert the filename
    let mut interner = Interner::with_capacity(2);
    let file_id = if let Some(file_name) = opt.file {
        interner.intern(&file_name.to_string_lossy())
    } else {
        interner.intern("stdin")
    };

    // Tokanize the input
    let lexer = Lexer::new(&input_string, file_id);

    // Parse & Build an AST
    // let mut parser = Parser::new(lexer);

    // let mut ast_root = match parser.parse() {
    //     Some(ast) => ast,
    //     None => return,
    // };

    // TODO: Semantic Analysis
    // let mut semantic_analyzer = SemanticAnalyzer::new();
    // semantic_analyzer.analyze(&mut ast_root);

    // Run Code Gen
    // #[cfg(feature="llvm-backend")]
    // {
    //     let mut generator = LLVMGenerator::new();

    //     generator.add_module(ast_root, true, true);
    //     generator.initialize(false);

    //     if args.flag_dump {
    //         generator.dump_ir();
    //     }

    //     generator.run().unwrap_or_else(|msg| panic!("{}", msg));
    // }
}

fn readable_to_string<R: Read>(mut readable: R) -> String {
    let mut input_string = String::new();

    if let Err(e) = readable.read_to_string(&mut input_string) {
        panic!("Failed to read: {}", e);
    }

    input_string
}

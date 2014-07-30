#![allow(dead_code)]
#![allow(unused_variable)]
#![allow(unused_imports)]

use std::io::BufferedReader;
use std::io::File;
use std::io::IoError;
use std::string::String;
use std::io::BufferedReader;

use syntax::lexer::Lexer;
use syntax::core::tokens;
use syntax::core::tokens::Token;

pub struct Error {
    pub line: uint,
    pub column: uint,
    pub messgage: &'static str,
}

pub struct Parser {
    lexer: Lexer<BufferedReader<File>>,
    cur_token: Token,
}

impl Parser {
    pub fn new(filename: &str) -> Parser {
        let path = Path::new(filename);
        let file = match File::open(&path) {
            Ok(f)  => f,
            Err(e) => fail!("Failed to open file. File error: {}", e),
        };
        Parser {
            lexer: Lexer::new(BufferedReader::new(file)),
            cur_token: tokens::Start,
        }
    }

    // Parse the file
    pub fn parse(&mut self) {
        self.lexer.get_tok();
    }
}

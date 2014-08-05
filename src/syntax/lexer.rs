#![allow(dead_code)]
#![allow(unused_variable)]
#![allow(unused_imports)]
#![allow(unused_mut)]

use std::char::{is_whitespace, is_alphabetic, is_alphanumeric};
use std::from_str::FromStr;
use std::io::{IoError, IoResult};
use std::string::String;

use syntax::core::keywords::Keywords;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::punctuation;

pub struct Lexer<B> {
    pub line_number: uint,
    pub column_number: uint,
    lastToken: Token,

    // Buffer where input text is stored
    buffer: B
}

impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(buffer: B) -> Lexer<B> {
        Lexer {
            line_number: 1,
            column_number: 1,
            buffer: buffer,
            lastToken: tokens::LineBegin
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut lastChar;
        let mut string = String::new();

        // Flag for whether or not the input char needs to be reparsed
        let mut reparseFlag = false;

        loop {
            if reparseFlag != true {
                lastChar = match self.buffer.read_char() {
                    Ok(chr) => chr,
                    Err(err) => break
                };
            }
        }

        return tokens::EOF
    }
}

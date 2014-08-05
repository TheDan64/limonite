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

enum States {
    Start,
    Identifier,
    Numeric,
    MultiPunc
}

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
        let mut currChar;
        let mut string = String::new();
        let mut reparseFlag = false;
        let mut state = Start;

        loop {
            // Reuse the previous character if the flag is not set,
            // else grab the next character from the buffer.
            if reparseFlag == true {
                currChar = match self.buffer.read_char() {
                    Ok(chr) => chr,
                    Err(err) => return tokens::EOF
                };

                reparseFlag = false;
            }

            // Match some basic characters
            match currChar {
                // Skip over whitespace
                ' ' => {
                    lastChar = ' ';
                    print!("(space)");
                    continue;
                },

                // Keep track of indentation
                '\t'=> {
                    lastChar = '\t';
                    print!("(tab)");
                    continue;
                },

                // A line begins
                '\n'=> {
                    lastChar = '\n';
                    print!("(nl)");
                    continue;
                },

                _   => println!("Unknown char: ")
            }

        }

        return tokens::EOF;
    }
}

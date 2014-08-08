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
    
    // [_a-zA-z][_a-zA-Z0-9]*
    Identifier,

    // 42, 0x2a, 0b101010, ...
    Numeric,

    // *=, -=, ...
    MultiPunctuation
}

pub struct Lexer<B> {
    pub line_number: uint,
    pub column_number: uint,
    state: States,

    // Buffer where input text is stored
    buffer: B
}

impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(buffer: B) -> Lexer<B> {
        Lexer {
            line_number: 1,
            column_number: 1,
            state: Start,
            buffer: buffer
        }
    }

    pub fn state_to_token(&self, string: String) -> Option<Token> {
        match self.state {
            Start => None,
            Identifier => {
                return Some(tokens::Identifier(string))
            },
            Numeric => {
                return Some(tokens::Numeric(string))
            },
            MultiPunctuation => {
                return Some(tokens::EOF)
            }
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut currChar = ' ';
        let mut indentLevel = 0u;
        let mut string = String::new();
        let mut reparseFlag = false;

        loop {
            // Reuse the previous character if the flag is not set,
            // else grab the next character from the buffer.
            if reparseFlag == false {
                currChar = match self.buffer.read_char() {
                    Ok(chr) => chr,
                    Err(err) => return tokens::EOF
                };
  
                // Keep track of the character position
                if currChar == '\n' {
                    self.line_number = 1;
                    self.column_number += 1;
                }
                else {
                    self.line_number += 1;
                }
            }

            //print!("{}:{}:{} ", self.column_number, self.line_number, currChar);
            print!("{}", currChar);

            // Match some basic characters
            match currChar {
                ' ' => {
                    match self.state_to_token(string.clone()) {
                        // Ignore whitespace
                        None => continue,

                        // End of current state
                        Some(tok) => {
                            reparseFlag = true;
                            return tok;
                        }
                    }
                },

                // Keep track of indentation
                '\t'=> {
                    match self.state_to_token(string.clone()) {
                        _ => print!("") // tmp
                    }
                },

                // A line begins
                '\n'=> {
                    match self.state_to_token(string.clone()) {
                        // In start state, can return newline token
                        None => return tokens::LineBegin,
                        
                        // End of current state
                        Some(tok) => {
                            reparseFlag = true;
                            return tok;
                        }
                    }
                },

                _   => ()
            }
        }
        return tokens::EOF;
    }
}

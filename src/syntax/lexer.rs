#![allow(dead_code)]
#![allow(unused_variable)]

use std::io::IoError;
use std::string::String;
use std::char::is_whitespace;
use std::char::is_alphabetic;
use std::char::is_alphanumeric;

use syntax::core::keywords;
use syntax::core::tokens;
use syntax::core::tokens::Token;

pub struct Lexer<B> {
    pub token: Token,
    pub line_number: uint,
    pub column_number: uint,
    buffer: B
}


impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(buffer: B) -> Lexer<B> {
        Lexer {
            token: tokens::Start,
            line_number: 1,
            column_number: 1,
            buffer: buffer
        }
    }

    // Get a char from the buffer
    fn get_char(&mut self) -> char {
        return match self.buffer.read_char() {
            Ok(chr) => {
                //print!("{}", chr); // tmp, displays what has been parsed
                chr
            },
            Err(IoError {kind: EndOfFile, ..}) => -1 as char
//            Err(why)=> fail!("lexer.get_char() failed: {}", why)
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut string = String::new();
        let number = 0u;
        let mut lastChar = ' ';

        // Skip over all whitespace
        while is_whitespace(lastChar) {
            lastChar = self.get_char(); // ToDo: tokenize indent
        }

        // Identifier token [a-zA-Z][a-zA-Z0-9]*

        if is_alphabetic(lastChar) {
            string = String::from_char(1, lastChar);

            loop {
                lastChar = self.get_char();

                if is_alphanumeric(lastChar) == false {
                    break;
                }

                string.push_char(lastChar);
            }

            // ToDo: add more keywords
            // if Keywords.from_str(string.as_slice()) {
            //     return Keywords.from_str(string.slice());
            // }

            return tokens::Identifier(string);

            // return match string.as_slice() {
            //     "var"   => Keyword(keywords::Var),
            //     "print" => Keyword(keywords::Print),
            //     _       => Identifier(string)
            // }
        }

        // ToDo: find numeric values

        // Comment token

        if lastChar == '>' {
            lastChar = self.get_char();
            if lastChar == '>' {
                // ToDo: case of third '>' -> Multiline comments

                while lastChar != -1 as char && lastChar != '\n' && lastChar != '\r' {
                    lastChar = self.get_char();
                }

                if lastChar != -1 as char {
                    return self.get_tok();
                }
            }
        }

        // Found EOF

        if lastChar != -1 as char {
            return tokens::EOF;
        }

        // ToDo: return unknown token with the char

        return tokens::EOF;
    }
}

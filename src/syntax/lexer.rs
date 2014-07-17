#![allow(dead_code)] //temp
#![allow(unused_variable)] //temp
#![allow(unused_imports)] //temp
#![allow(unused_mut)] //temp

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
    buffer: B
}


impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(buffer: B) -> Lexer<B> {
        Lexer {
            line_number: 1,
            column_number: 1,
            buffer: buffer
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let number = 0u;
        let mut state = tokens::Start;
        let mut string = String::new();
        let mut lastChar;


        loop {
            lastChar = match self.buffer.read_char() {
                Ok(chr) => chr,
                Err(err) => break
            };

            print!("{}", lastChar); // tmp

            match lastChar {
                '('  => return tokens::Punctuation(punctuation::ParenOpen),
                ')'  => return tokens::Punctuation(punctuation::ParenClose),
                '['  => return tokens::Punctuation(punctuation::SBracketOpen),
                ']'  => return tokens::Punctuation(punctuation::SBracketClose),
                '{'  => return tokens::Punctuation(punctuation::CBracketOpen),
                '}'  => return tokens::Punctuation(punctuation::CBracketClose),
                '.'  => return tokens::Punctuation(punctuation::Period),
                ','  => return tokens::Punctuation(punctuation::Comma),
                ':'  => return tokens::Punctuation(punctuation::Colon),
                ';'  => return tokens::Punctuation(punctuation::SemiColon),
                '~'  => return tokens::Punctuation(punctuation::Negate),
                '='  => return tokens::Punctuation(punctuation::Assign),
                // Start 2 char puncuators:
                '>'  => return tokens::Punctuation(punctuation::GreaterThan),
                '<'  => return tokens::Punctuation(punctuation::LessThan),
                '+'  => return tokens::Punctuation(punctuation::Plus),
                '-'  => return tokens::Punctuation(punctuation::Minus),
                '*'  => return tokens::Punctuation(punctuation::Multiply),
                '/'  => return tokens::Punctuation(punctuation::Divide),
                '%'  => return tokens::Punctuation(punctuation::Modulus),
                // ">=" => return tokens::Punctuation(punctuation::GreaterThanEqual),
                // "<=" => return tokens::Punctuation(punctuation::LessThanEqual),
                // "++" => return tokens::Punctuation(punctuation::Increment),
                // "--" => return tokens::Punctuation(punctuation::Decrement),
                // "+=" => return tokens::Punctuation(punctuation::AddAssign),
                // "-=" => return tokens::Punctuation(punctuation::MinusAssign),
                // "*=" => return tokens::Punctuation(punctuation::MultiplyAssign),
                // "/=" => return tokens::Punctuation(punctuation::DivideAssign),
                // "%=" => return tokens::Punctuation(punctuation::ModulusAssign),
                _ => break
            };




            string.push_char(lastChar);


            print!("{}", lastChar); // tmp
        }

        return tokens::EOF
    }
}

/* Old:
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
*/

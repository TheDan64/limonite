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
    buffer: B,
    newCharFlag: bool
}

impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(buffer: B) -> Lexer<B> {
        Lexer {
            line_number: 1,
            column_number: 1,
            buffer: buffer,

	    // This flag is for when a character needs to be reparsed	    
	    newCharFlag: false
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut state = tokens::LineBegin;
        let mut string = String::new();
        let mut lastChar;


        loop {
            lastChar = match self.buffer.read_char() {
                Ok(chr) => chr,
                Err(err) => break
            };

/*            match lastChar {
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
*/
        }

        return tokens::EOF
    }
}
use std::char::{is_whitespace, is_alphabetic, is_alphanumeric};
use std::from_str::FromStr;
use std::io::{IoError, IoResult};
use std::string::String;

use syntax::core::keywords::Keywords;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::punctuation;

// A Lexer that keeps track of the current line and column position
// as well as the position in the char input stream.
pub struct Lexer<B> {
    pub line_number: uint,
    pub column_number: uint,
    pos: uint,
    input: String
}

impl<B:Buffer> Lexer<B> {
    // Create a new lexer instance
    pub fn new(mut FileReader: B) -> Lexer<B> {
        Lexer {
            line_number: 1,
            column_number: 1,
            pos: 0,
            input: FileReader.read_to_string().unwrap()
        }
    }

    fn slice_from_pos(&mut self) -> &str {
        self.input.as_slice().slice_from(self.pos)
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> char {
        let ch = self.next_char();

        self.pos += 1;
        self.line_number += 1;

        if ch == '\n' {
            self.line_number = 1;
            self.column_number += 1;
        }

        return ch;
    }

    // Gets the next char
    fn next_char(&self) -> char {
        return self.input.as_slice().char_at(self.pos);
    }

    // Determine if we hit End of File
    fn eof(&self) -> bool {
        return self.pos >= self.input.len();
    }

    // Thanks to mbrubeck of Mozilla for this consume_while fn as
    // well eof() and next_char() examples :)
    fn consume_while(&mut self, test: |char| -> bool) -> String {
        let mut result = String::new();

        while !self.eof() && test(self.next_char()) {
            result.push_char(self.consume_char());
        }

        return result;
    }
    
    // Consume non newline and tab whitespace
    fn consume_whitespace(&mut self) {
        self.consume_while(|ch| match ch {
            '\n' | '\t' => false,
            w if w.is_whitespace() => true,
            _ => false
        });
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    fn consume_identifier(&mut self) -> Option<String> {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        Some(self.consume_while(|ch| match ch {
            a if a.is_alphanumeric() => true,
            '_' => true,
            _ => false
        }))
    }

    // Determines what type of number it is and call the appropriate fn
    fn consume_numeric(&mut self) -> Option<String> {
        if self.slice_from_pos().starts_with("0x") {
            self.consume_hex()

        } else if self.slice_from_pos().starts_with("0b") {
            self.consume_bin()

        } else {
            self.consume_num()

        }
    }

    // Int: [0-9]+(s|u), Float: [0-9]+.[0-9]+(f32|f64) I think
    fn consume_num(&self) -> Option<String> {
        let string = String::new();

        Some(string) // tmp
    }

    // Hexidecimal: 0x[0-9a-fA-F_]+(u|s)
    fn consume_hex(&mut self) -> Option<String> {
        let mut string = String::from_str("0x");

        self.pos += 2;

        // Dumb: can't use += for strings but + is fine...
        string = string + self.consume_while(|ch| match ch {
            '0'..'9' | 'a'..'f' | 'A'..'F' | '_' => true,
             _                                   => false
        });
        
        if !self.eof() {
            match self.next_char() {
                's' | 'u' => string.push_char(self.consume_char()),
                 _        => ()
            };
        }

        match string.as_slice() {
            "0x" => None,
             _   => Some(string)
        }
    }

    // Binary: 0b[01_]*
    fn consume_bin(&mut self) -> Option<String> {
        let mut string = String::from_str("0b");

        self.pos += 2;

        string = string + self.consume_while(|ch| match ch {
            '0' | '1' | '_' => true,
             _              => false
        });

        if !self.eof() {
            match self.next_char() {
                's' | 'u' => string.push_char(self.consume_char()),
                 _        => ()
            }
        }
        
        match string.as_slice() {
            "0b" => None,
             _   => Some(string)
        }
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut currChar = '\n';
        let mut indentLevel = 0u;
        let mut string = String::new();

        // Next step: finish above support fns and finish this section.

        return tokens::EOF;
    }
}

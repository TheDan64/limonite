use std::char::{is_whitespace, is_alphanumeric};
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

    fn current_slice(&mut self) -> &str {
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

        ch
    }

    // Gets the next char
    fn next_char(&self) -> char {
        return self.input.as_slice().char_at(self.pos);
    }

    // Determine if we hit the inevitable End of File
    fn eof(&self) -> bool {
        return self.pos >= self.input.len();
    }

    // Thanks to mbrubeck of Mozilla for this consume_while fn as
    // well eof() and next_char() examples :)
    fn consume_while(&mut self, test: |&Lexer<B>, char| -> bool) -> String {
        let mut result = String::new();

        while !self.eof() && test(self, self.next_char()) {
            result.push_char(self.consume_char());
        }

        result
    }
    
    // Consume non newline and tab whitespace
    fn consume_whitespace(&mut self) {
        self.consume_while(|self_, ch| match ch {
            '\n' | '\t'            => false,
            w if w.is_whitespace() => true,
            _                      => false
        });
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    fn consume_identifier(&mut self) -> Option<String> {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        Some(self.consume_while(|self_, ch| match ch {
            a if a.is_alphanumeric() => true,
            '_'                      => true,
             _                       => false
        }))
    }

    // Determines what type of number it is and call the appropriate fn
    fn consume_numeric(&mut self) -> Option<String> {
        let start = self.pos;
        let mut result = String::new();

        if self.current_slice().starts_with("0x") {
            // Found hexadecimal: 0x[0-9a-fA-F_]+

            self.pos += 2;
            result.push_str("0x");

            // Cant do += for String, and push_str looks better than
            // result = result + self.consume...
            result.push_str(self.consume_while(|self_, ch| match ch {
                '0'..'9' | 'a'..'f' | 'A'..'F' => true,
                 _                             => false
            }).as_slice());

        } else if self.current_slice().starts_with("0b") {
            // Found binary: 0b[01_]+

            self.pos += 2;
            result.push_str("0b");

            // Formatting the same as the hex case above.
            result.push_str(self.consume_while(|self_, ch| match ch {
                '0' | '1' | '_' => true,
                 _              => false
            }).as_slice());

        } else {
            // Need to finish this

        }

        // ToDo: catch signed or unsigned signature at the end (u|i)?

        // ToDo: Make this error msg better. Add line column and number, etc.
        match result.as_slice() {
            "0x" | "0b" => fail!("Invalid hex or binary value! GG!"),
             _          => Some(result)
        }
    }

    fn consume_comment(&mut self) -> Option<String> {
        let mut result = String::new();

        Some(result)
    }

    fn consume_tabs(&mut self) -> uint {
        let mut count = 0;

        self.consume_while(ref |self_, ch| match ch {
            '\t' => {
                count += 1;
                true
            },
             _   => false
        });

        count
    }

    // Parse the file where it left off and return the next token
    pub fn get_tok(&mut self) -> Token {
        let mut currChar = '\n';
        let mut indentLevel = 0u;
        let mut string = String::new();

        // ToDo: Lex!

        return tokens::EOF;
    }
}

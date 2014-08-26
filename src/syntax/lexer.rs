use syntax::core::keywords::Keywords;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::punctuation;

pub trait Tokenizer {
    fn get_tok(&mut self) -> Token;
}

// A lexer
pub struct Lexer {
    pub line_number: uint,
    pub column_number: uint,
    pos: uint,
    line_start: uint,
    input: String
}

impl Tokenizer for Lexer {
    // Parse the file where it left off and return the next token
    fn get_tok(&mut self) -> Token {
        let mut indentLevel = 0u;

        // ToDo: More lexing!
        loop {
            if self.eof() {
                return tokens::EOF;
            }

            self.consume_whitespace();

            match self.consume_char() {
                // Find Keywords and Identifiers
                a if a.is_alphabetic() || a == '_' => {
                    let ident = self.consume_identifier();

                    // Keywords are found in /src/syntax/core/keywords.rs
                    return match from_str::<Keywords>(ident.as_slice()) {
                        Some(key) => tokens::Keyword(key),
                        None      => tokens::Identifier(ident)
                    };
                },

                // Find ints, floats, hex, and bin numeric values
                n if n.is_digit() => {
                    return match self.consume_numeric() {
                        Ok(num)  => tokens::Numeric(num),
                        Err(err) => tokens::Error(err)
                    }
                },

                // ToDo: match punctuation
                _ => ()
            };
        }
    }
}

impl Lexer {
    // Create a new lexer instance
    pub fn new<B: Buffer>(mut fileReader: B) -> Lexer {
        Lexer {
            line_number: 1,
            column_number: 1,
            pos: 0,
            line_start: 0,
            input: fileReader.read_to_string().unwrap()
        }
    }

    // For displaying error messages from the parser
    pub fn current_line(&mut self) -> String {
        let tmp = self.pos;
        let mut result = String::new();

        self.pos = self.line_start;

        result.push_str(self.consume_while(|ch| match ch {
            '\n' => false,
            _    => true
        }).as_slice());

        self.pos = tmp;

        result
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
            self.line_start = self.pos;
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
    fn consume_while(&mut self, test: |char| -> bool) -> String {
        let mut result = String::new();

        while !self.eof() && test(self.next_char()) {
            result.push_char(self.consume_char());
        }

        result
    }

    // Consume non newline and tab whitespace
    fn consume_whitespace(&mut self) {
        self.consume_while(|ch| match ch {
            '\n' | '\t'            => false,
            w if w.is_whitespace() => true,
            _                      => false
        });
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    fn consume_identifier(&mut self) -> String {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        self.consume_while(|ch| match ch {
            a if a.is_alphanumeric() => true,
            '_'                      => true,
             _                       => false
        })
    }

    // Determines what type of number it is and call the appropriate fn
    fn consume_numeric(&mut self) -> Result<String, String> {
        let mut result = String::new();

        if self.current_slice().starts_with("0x") {
            // Found hexadecimal: 0x[0-9a-fA-F_]+

            self.pos += 2;
            result.push_str("0x");

            // Cant do += for String, and push_str looks better than
            // result = result + self.consume...
            result.push_str(self.consume_while(|ch| match ch {
                '0'..'9' | 'a'..'f' | 'A'..'F' => true,
                 _                             => false
            }).as_slice());

            // ToDo: Get valid suffix for hex. Same as int?

        } else if self.current_slice().starts_with("0b") {
            // Found binary: 0b[01_]+

            self.pos += 2;
            result.push_str("0b");

            // Formatting the same as the hex case above.
            result.push_str(self.consume_while(|ch| match ch {
                '0' | '1' | '_' => true,
                 _              => false
            }).as_slice());

            // ToDo: Get valid suffix for bin. Same as int?

        } else {
            // Found int: [0-9]+ or float: [0-9]+.[0-9]+

            result.push_str(self.consume_while(|ch| match ch {
                '0'..'9' => true,
                 _       => false
            }).as_slice());

            match self.next_char() {
                // Float decimal point:
                '.' => {
                    result.push_char(self.consume_char());

                    let fractional = self.consume_while(|ch| match ch {
                        '0'..'9' => true,
                         _       => false
                    });

                    if fractional.as_slice() == "" {
                        return Err("Invalid floating point number.".to_string());

                    } else {
                        result.push_str(fractional.as_slice());
                    }

                    // Float suffixes:
                    // ToDo: find f32, f64?
                },

                // Int suffixes:
                // ToDo: will we need other int sizes?
                'i' | 'u' => {
                    result.push_char(self.consume_char());

                    // Ensure there are no additional alpha chars
                    if self.next_char().is_alphabetic() {
                        return Err("Invalid suffix.".to_string());
                    }

                },

                // Ensure there are no additional alpha chars
                c if c.is_alphabetic() => {
                    return Err("Invalid suffix".to_string());
                },

                // Presumably any other remaining char is valid, ie punctuation {,[ etc
                _ => ()
            };
        }

        match result.as_slice() {
            "0x" => Err("Invalid hex value.".to_string()),
            "0b" => Err("Invalid binary value.".to_string()),
             _   => Ok(result)
        }
    }

    fn consume_comment(&mut self) -> Result<String, String> {
        let mut result = String::from_str(">>");

        if self.next_char() == '>' {
            // Multiline comments must end in <<< else error
            let mut sequence = 0u;

            result.push_char('>');

            result.push_str(self.consume_while(ref |ch| match ch {
                '<' => {
                    sequence += 1;

                    if sequence == 3 {
                        return false;
                    }
                    true
                },
                 _  => {
                     sequence = 0;
                     true
                 }
            }).as_slice());

            // ToDo: eof check/no <<< found -> error
        }
        else {
            // Single line comments eat up anything until newline or eof
            result.push_str(self.consume_while(|ch| match ch {
                '\n' => false,
                _ => true
            }).as_slice());

        }

        // ToDo: Finish this

        Ok(result)
    }

    fn consume_tabs(&mut self) -> uint {
        let mut count = 0;

        self.consume_while(ref |ch| match ch {
            '\t' => {
                count += 1;
                true
            },
             _   => false
        });

        count
    }
}

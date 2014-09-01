use syntax::core::keywords::Keywords;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::punctuation;
use syntax::core::punctuation::Punctuations;

pub trait Tokenizer {
    fn get_tok(&mut self) -> Token;
}

// A Lexer that keeps track of the current line and column position
// as well as the position in the char input stream.
pub struct Lexer {
    /*****

    pub status of line and column number deprecated, please use get_tok_line(), get_tok_column()

    ******/
    pub line_number: uint,
    pub column_number: uint,
    buffer_pos: uint,
    line_start: uint,
    input: String
}

impl Tokenizer for Lexer {
    // Parse the file where it left off and return the next token
    fn get_tok(&mut self) -> Token {
        // ToDo: More lexing!
        loop {
            if self.eof() {
                return tokens::EOF;
            }

            self.consume_whitespace();

            return match self.next_char() {
                // Find Keywords and Identifiers
                a if a.is_alphabetic() || a == '_' => self.consume_identifier(),

                e if self.eof() => tokens::EOF,

                // Find ints, floats, hex, and bin numeric values
                n if n.is_digit() => self.consume_numeric(),

                // Count tabs: \n\t*
                '\n' => self.consume_tabs(),

                // Error: Found tabs without preceeding newline
                '\t' => {
                    self.consume_char();

                    tokens::Error("Found an out of place tab.".to_string())
                },

                // Find single char punctuations
                '(' | ')' | '[' | ']' | '{' | '}' |
                '.' | ',' | ':' | '<' | '~' | '=' => {
                    // Dumb: cant do self.single_punc_token(self.consume_char()) due to borrowing
                    // But the following works.
                    let ch = self.consume_char();

                    self.single_punc_token(ch)
                },

                // Find multi-char(+=, -=, ..) punctuations or not
                '+' | '-' | '*' | '/' | '%' => {
                    let ch = self.consume_char();

                    match self.next_char() {
                        '=' => {
                            let ch2 = self.consume_char();

                            self.multi_punc_token([ch, ch2])
                        },
                         _  => self.single_punc_token(ch)
                    }
                },

                '>' => {
                    self.consume_char();

                    match self.next_char() {
                        '>' => self.consume_comment(),
                         _  => self.single_punc_token('>')
                    }
                },

                ch => tokens::Error(format!("Unknown character '{}'.", ch).to_string())
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
            buffer_pos: 0,
            line_start: 0,
            input: fileReader.read_to_string().unwrap()
        }
    }

    // ToDo: figure out a way to change this to take an index
    pub fn current_line(&mut self) -> String {
        let tmp = self.buffer_pos;
        let mut result = String::new();

        self.buffer_pos = self.line_start;

        result.push_str(self.consume_while(|ch| match ch {
            '\n' => false,
            _    => true
        }).as_slice());

        self.buffer_pos = tmp;

        result
    }

    fn current_slice(&mut self) -> &str {
        self.input.as_slice().slice_from(self.buffer_pos)
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> char {
        let ch = self.next_char();

        self.buffer_pos += 1;
        self.column_number += 1;

        if ch == '\n' {
            self.line_start = self.buffer_pos;
            self.line_number += 1;
            self.column_number = 1;
        }

        ch
    }

    // Gets the next char
    fn next_char(&self) -> char {
        return self.input.as_slice().char_at(self.buffer_pos);
    }

    // Determine if we hit the inevitable End of File
    fn eof(&self) -> bool {
        return self.buffer_pos >= self.input.len();
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

    // Single char puncuations: (, [, ., ...
    fn single_punc_token(&mut self, ch: char) -> Token {
        tokens::Punctuation(match ch {
            '(' => punctuation::ParenOpen,
            ')' => punctuation::ParenClose,
            '[' => punctuation::SBracketOpen,
            ']' => punctuation::SBracketClose,
            '{' => punctuation::CBracketOpen,
            '}' => punctuation::CBracketClose,
            '.' => punctuation::Period,
            ',' => punctuation::Comma,
            ':' => punctuation::Colon,
            '>' => punctuation::GreaterThan,
            '<' => punctuation::LessThan,
            '+' => punctuation::Plus,
            '-' => punctuation::Minus,
            '*' => punctuation::Asterisk,
            '/' => punctuation::Slash,
            '%' => punctuation::Percent,
            '~' => punctuation::Tilde,
            '=' => punctuation::Equals,

            // Should never get here:
              _ => punctuation::Period
        })
    }

    // Multi char punctuations: +=, -=, ...
    fn multi_punc_token(&mut self, vec: &[char]) -> Token {
        let string = String::from_chars(vec);
        let punct = from_str::<Punctuations>(string.as_slice()).unwrap();
        
        tokens::Punctuation(punct)
    }

    // Consume non newline whitespace
    fn consume_whitespace(&mut self) {
        self.consume_while(|ch| match ch {
            '\n'                   => false,
            '\t'                   => false,
            w if w.is_whitespace() => true,
            _                      => false
        });
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    fn consume_identifier(&mut self) -> Token {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        let ident = self.consume_while(|ch| match ch {
            a if a.is_alphanumeric() => true,
            '_'                      => true,
             _                       => false
        });

        match from_str::<Keywords>(ident.as_slice()) {
            Some(key) => tokens::Keyword(key),
            None      => tokens::Identifier(ident)
        }
    }

    // Determines what type of number it is and call the appropriate fn
    fn consume_numeric(&mut self) -> Token {
        let mut result = String::new();

        if self.current_slice().starts_with("0x") {
            // Found hexadecimal: 0x[0-9a-fA-F_]+

            self.buffer_pos += 2;
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

            self.buffer_pos += 2;
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
                        return tokens::Error("Invalid floating point number.".to_string());

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
                        return tokens::Error("Invalid suffix.".to_string());
                    }

                },

                // Ensure there are no additional alpha chars
                c if c.is_alphabetic() => {
                    return tokens::Error("Invalid suffix".to_string());
                },

                // Presumably any other remaining char is valid, ie punctuation {,[ etc
                _ => ()
            };
        }

        match result.as_slice() {
            "0x" => tokens::Error("Invalid hex value.".to_string()), 
            "0b" => tokens::Error("Invalid binary value.".to_string()),
             _   => tokens::Numeric(result)
        }
    }

    fn consume_comment(&mut self) -> Token {
        let mut result = String::new();

        // Consume 2nd '>'
        self.consume_char();

        if self.next_char() == '>' {
            // Multiline comments must end in <<< else error
            let mut sequence = 0u;

            // Consume 3rd '>'
            self.consume_char();

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

        tokens::Comment(result)
    }

    fn consume_tabs(&mut self) -> Token {
        let mut count = 0;
        
        // Consume the newline token, count tabs
        self.consume_char();
        self.consume_while(ref |ch| match ch {
            '\t' => {
                count += 1;
                true
            },
             _   => false
        });

        tokens::Indent(count)
    }
}

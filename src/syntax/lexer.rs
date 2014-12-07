use syntax::core::keywords::Keywords;
use syntax::core::types;
use syntax::core::types::Types;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::punctuation;
use syntax::core::punctuation::Punctuations;

pub trait Tokenizer {
    fn get_tok(&mut self) -> Token;
}

// A Lexer that keeps track of the current line and column position
// as well as the position in the char input stream.
pub struct Lexer<'a> {
    line_number: uint,
    column_number: uint,
    buffer_pos: uint,
    line_start: uint,
    input: &'a str,
    lines: Vec<&'a str>
}

impl<'a> Tokenizer for Lexer<'a> {
    // Parse the file where it left off and return the next token
    fn get_tok(&mut self) -> Token {
        if self.eof() {
            return tokens::EOF;
        }

        self.consume_whitespace();
        
        return match self.next_char() {
            // Find Keywords and Identifiers
            Some(a) if a.is_alphabetic() || a == '_' => self.consume_identifier(),

            // Find ints, floats, hex, and bin numeric values
            Some(n) if n.is_digit() => self.consume_numeric(),

            // Count tabs: \n\t*
            Some('\n') => self.consume_tabs(),

            // Error: Found tabs without preceeding newline
            Some('\t') => {
                self.consume_char().unwrap();
                
                tokens::Error("Found an out of place tab.".to_string())
            },
            
            // Find single char punctuations
            Some('(') | Some(')') |
            Some('[') | Some(']') |
            Some('{') | Some('}') |
            Some('.') |
            Some(',') |
            Some(':') |
            Some('<') |
            Some('~') |
            Some('=') => {
                // Dumb: cant do self.single_punc_token(self.consume_char().unwrap()) due to borrowing
                // But the following works.
                let ch = self.consume_char().unwrap();

                self.single_punc_token(ch)
            },

            // Find multi-char(+=, -=, ..) or the single-char version
            Some('+') |
            Some('*') |
            Some('/') |
            Some('%') => {
                let ch = self.consume_char().unwrap();
                
                match self.next_char() {
                    Some('=') => {
                        self.consume_char();
                        
                        self.multi_punc_token([ch, '='])
                    },
                    _ => self.single_punc_token(ch)
                }
            },

            // Find -> punctuation or -
            Some('-') => {
                self.consume_char();

                match self.next_char() {
                    Some('>') => {
                        self.consume_char();

                        self.multi_punc_token(['-', '>'])
                    },
                    _ => self.single_punc_token('-')
                }
            },

            // Find >> and >>> comments, otherwise '>' punctuation
            Some('>') => {
                self.consume_char();
                
                match self.next_char() {
                    Some('>') => self.consume_comment(),
                    _         => self.single_punc_token('>')
                }
            },

            // Find character literals, 'c', including ascii escape chars
            Some('\'') => self.consume_char_literal(),

            // Find string literals, "String"
            Some('\"') => self.consume_string_literal(),
            
            Some(ch) => tokens::Error(format!("Unknown character '{}'.", ch).to_string()),
            
            None => tokens::EOF
        };
    }
}

impl<'a> Lexer<'a> {
    // Create a new lexer instance
    pub fn new(slice: &'a str) -> Lexer<'a> {
        Lexer {
            line_number: 1,
            column_number: 1,
            buffer_pos: 0,
            line_start: 0,
            input: slice,
            lines: slice.lines().collect()
        }
    }

    // get_line takes a line number(1..self.lines.len()), not an index (0..self.lines.len()-1)
    pub fn get_line(&mut self, line: uint) -> Option<&str> {
        match line {
            // Out of bounds
            l if l < 1 => None,
            l if l > self.lines.len() => None,

            // Return the line
            _ => Some(self.lines[line - 1])
        }
    }

    fn current_slice(&mut self) -> &str {
        self.input.slice_from(self.buffer_pos)
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> Option<char> {
        match self.next_char() {
            Some(ch) => {
                self.buffer_pos += 1;
                self.column_number += 1;
                
                if ch == '\n' {
                    self.line_start = self.buffer_pos;
                    self.line_number += 1;
                    self.column_number = 1;
                }

                Some(ch)
            },

            None => None
        }
    }

    // Gets the next char
    fn next_char(&self) -> Option<char> {
        match self.eof() {
            false => Some(self.input.char_at(self.buffer_pos)),
            true  => None
        }
    }

    // Determine if we hit the inevitable End of File
    fn eof(&self) -> bool {
        return self.buffer_pos >= self.input.len();
    }

    // Thanks to mbrubeck of Mozilla for the base of this consume_while
    // fn as well eof() and next_char() examples :)
    fn consume_while(&mut self, test: |char| -> bool, escape: bool) -> &str {
        let mut result: &'a str = "";

        // Always unwrapping as the loop checks eof.
        while !self.eof() && test(self.next_char().unwrap()) {
            match self.next_char() {
                // Ignore any carriage returns
                Some('\r') => {
                    self.consume_char();

                    continue;
                },

                // Handle escape chars if escape flag is set
                Some('\\') if escape => {
                    self.consume_char();

                    match self.consume_char() {
                        Some(ch) => match Lexer::escape_char(ch) {
                            Ok(chr) => result = concat!(result, chr),

                            // Currently no way to handle this error
                            // So I'm returning the char as is
                            Err(_)  => result = concat!(result, ch)
                        },

                        // EOF
                        None => result = concat!(result, '\\')
                    }

                    continue;
                },
                
                _  => result += self.consume_char().unwrap()
            };
        }

        result.as_slice()
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
             _  => panic!(format!("Lexer error: hit what should be an unreachable single punctuation type {}", ch))
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
            '\n' | '\t'            => false,
            w if w.is_whitespace() => true,
            _                      => false
        }, false);
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    // Keywords and Types are subsets of identifiers.
    fn consume_identifier(&mut self) -> Token {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        let ident = self.consume_while(|ch| match ch {
            a if a.is_alphanumeric() => true,
            '_'                      => true,
             _                       => false
        }, false);

        // ToDo: Consolodate the following somehow?
        // Idea: one big from_str fn for ident/keyword/type/boollits?

        match from_str::<Keywords>(ident) {
            Some(key) => return tokens::Keyword(key),
            None      => ()
        };

        match ident.as_slice() {
            "True"  => return tokens::BoolLiteral(true),
            "False" => return tokens::BoolLiteral(false),
            _       => ()
        }

        match from_str::<Types>(ident) {
            Some(type_) => tokens::Type(type_),
            _           => tokens::Identifier(ident.to_string())
        }
    }

    // Find a sequence of 32 or 64
    fn consume_32_64(&mut self, type_: char) -> Result<String, String> {
        // ch is the starting character, ie i, u, f
        let mut string = String::from_char(1, type_);

        match self.next_char() {
            Some('3') => {
                string.push(self.consume_char().unwrap());
                
                match self.next_char() {
                    Some('2')  => {
                        string.push(self.consume_char().unwrap());

                        Ok(string)
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {}32?", string, type_)),
                    Some(chr)  => Err(format!("Invalid suffix {}{}. Did you mean {0}2?", string, self.consume_char().unwrap()).to_string()),
                    None       => Err(format!("Hit EOF when looking for suffix {}32.", type_).to_string())
                }
            },
            Some('6') => {
                string.push(self.consume_char().unwrap());
                
                match self.next_char() {
                    Some('4')  => {
                        string.push(self.consume_char().unwrap());
                        
                        Ok(string)
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {}64?", string, type_)),
                    Some(chr)  => Err(format!("Invalid suffix {}{}. Did you mean {0}4?", string, self.consume_char().unwrap()).to_string()),
                    None       => Err(format!("Hit EOF when looking for suffix {}64.", type_).to_string())
                }
            },
            Some('\n') | // NL & CR have pesky visual effects.
            Some('\r') => Err(format!("Invalid suffix {}. Did you mean {1}32 or {1}64?", string, type_)),
            Some(chr)  => Err(format!("Invalid suffix {}{}. Did you mean {0}32 or {0}64?", string, self.consume_char().unwrap()).to_string()),
            None       => Err(format!("Hit EOF when looking for a suffix {0}32 or {0}64.", type_).to_string())
        }
    }

    // Determines what type of number it is and consume it
    fn consume_numeric(&mut self) -> Token {
        let mut number = String::new();
        let mut suffix = None;

        if self.current_slice().starts_with("0x") {
            // Found hexadecimal: 0x[0-9a-fA-F_]+

            self.buffer_pos += 2;
            number.push_str("0x");

            // Cant do += for String, and push_str looks better than
            // number = number + self.consume...
            number.push_str(self.consume_while(|ch| match ch {
                '0'...'9' |
                'a'...'f' |
                'A'...'F' |
                '_' => true,
                 _  => false
            }, false));

            if number.as_slice() == "0x" {
                return tokens::Error("No hexadecimal value was found.".to_string());
            }

            // Attempt to find a suffix if one exists
            match self.next_char() {
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();
                    let mut string = String::new();
                    
                    match self.consume_32_64(ch) {
                        Ok(s)    => string.push_str(s.as_slice()),
                        Err(err) => return tokens::Error(err)
                    };

                    suffix = from_str::<Types>(string.as_slice());
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphanumeric() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return tokens::Error(err.to_string());
                },
                
                // If eof or other just return the numeric token without a suffix
                _ => ()
            };

        } else if self.current_slice().starts_with("0b") {
            // Found binary: 0b[01_]+

            self.buffer_pos += 2;
            number.push_str("0b");

            // Formatting the same as the hex case above.
            number.push_str(self.consume_while(|ch| match ch {
                '0' |
                '1' |
                '_' => true,
                 _  => false
            }, false));

            if number.as_slice() == "0b" {
                return tokens::Error("No binary value was found.".to_string());
            }

            // Attempt to find a suffix if one exists
            match self.next_char() {
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();
                    let mut string = String::new();
                    
                    match self.consume_32_64(ch) {
                        Ok(s)    => string.push_str(s.as_slice()),
                        Err(err) => return tokens::Error(err)
                    };

                    suffix = from_str::<Types>(string.as_slice());
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphabetic() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return tokens::Error(err.to_string());
                },
                
                // If eof or other just return the numeric token without a suffix
                _ => ()
            };
            
        } else {
            // Found int: [0-9]+ or float: [0-9]+.[0-9]+

            number.push_str(self.consume_while(|ch| match ch {
                '0'...'9' |
                '_' => true,
                 _  => false
            }, false));

            match self.next_char() {
                // Float decimal point:
                Some('.') => {
                    number.push(self.consume_char().unwrap());

                    let fractional = self.consume_while(|ch| match ch {
                        '0'...'9' |
                        '_' => true,
                         _  => false
                    }, false);

                    // Check if no decimal values were found
                    match fractional {
                        "" => return tokens::Error("Invalid floating point number.".to_string()),
                        _  => number.push_str(fractional)
                    }

                    // Find float suffixes
                    match self.next_char() {
                        Some('f') => {
                            let ch = self.consume_char().unwrap();
                            let mut string = String::new();

                            match self.consume_32_64(ch) {
                                Ok(s)    => string.push_str(s.as_slice()),
                                Err(err) => return tokens::Error(err)
                            };
                            
                            suffix = from_str::<Types>(string.as_slice());
                        },

                        // Found some other suffix, ie 0x42o
                        Some(c) if c.is_alphabetic() => {
                            let ch = self.consume_char().unwrap();
                            let err = format!("Invalid suffix {}. Did you mean f32, f64?", ch);

                            return tokens::Error(err.to_string());
                        },
                        
                        // No suffix found, can hit punctuation or other
                        _ => ()
                    }
                },

                // Int suffixes:
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();
                    let mut string = String::new();
                    
                    match self.consume_32_64(ch) {
                        Ok(s)    => string.push_str(s.as_slice()),
                        Err(err) => return tokens::Error(err)
                    };

                    suffix = from_str::<Types>(string.as_slice());
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphabetic() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return tokens::Error(err.to_string());
                },

                // Presumably any other remaining char is valid, ie punctuation {,[ etc
                _ => ()
            };
        }

        tokens::Numeric(number, suffix)
     }

    fn consume_comment(&mut self) -> Token {
        let mut result = String::new();

        // Consume 2nd '>'
        self.consume_char();

        match self.next_char() {
            // Multiline comments must end in <<< else error
            Some('>') => {
                let mut sequence = 0u;

                // Consume 3rd '>'
                self.consume_char();

                result.push_str(self.consume_while(|ch| match ch {
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
                }, false));

                // Should be able to consume the last <
                match self.consume_char() {
                    Some('<') => (),
                    _         => return tokens::Error("Hit eof before end of multi-line comment.".to_string())
                }

                // Remove << from end of the comment
                result.pop();
                result.pop();
            },

            // Single line comments eat up anything until newline or eof
            Some(_) => {
                result.push_str(self.consume_while(|ch| match ch {
                    '\n' => false,
                    _ => true
                }, false));
            },

            // Single line comment w/ EOF at start should be valid:
            None => ()
        }
        
        tokens::Comment(result)
    }

    fn consume_tabs(&mut self) -> Token {
        let mut count = 0;

        // Consume the newline token, count tabs
        self.consume_char();
        self.consume_while(|ch| match ch {
            '\t' => {
                count += 1;
                true
            },
            _    => false
        }, false);

        tokens::Indent(count)
    }

    fn escape_char(ch: char) -> Result<char, String> {
        // Does not include unicode escapes

        match ch {
            '\''=> Ok('\''),
            '\"'=> Ok('\"'),
            '\\'=> Ok('\\'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
//            '\n'=> Ok(''), // Escape newline?
            _   => Err(format!("Unknown character escape: {}", ch).to_string())
        }
    }

    fn consume_char_literal(&mut self) -> Token {
        let ch: char;

        // Consume first '
        self.consume_char();

        // Get the character or two if escaped
        match self.consume_char() {
            Some('\\') => {
                match self.consume_char() {
                    Some(ch2) => {
                        match Lexer::escape_char(ch2) {
                            Ok(esc)  => ch = esc,
                            Err(msg) => return tokens::Error(msg)
                        }
                    },
                    None => return tokens::Error("Hit eof before end of character literal.".to_string())
                };
            },
            Some('\'') => return tokens::Error("Empty character literal is invalid.".to_string()),
            Some(c)    => ch = c,
            None       => return tokens::Error("Hit eof before end of character literal.".to_string())
        };

        // Get the final '
        match self.consume_char() {
            Some('\'')     => tokens::CharLiteral(ch),
            Some(_) | None => tokens::Error("Char literal was not closed with a '".to_string())
        }
    }

    fn consume_string_literal(&mut self) -> Token {
        let mut result = String::new();

        // Consume first "
        self.consume_char();

        if self.eof() {
            return tokens::Error("Hit EOF before end of string literal.".to_string());
        }

        result.push_str(self.consume_while(|ch| match ch {
            '\"' => false,
            _ => true
        }, true));

        match self.consume_char() {
            Some(ch) => tokens::StrLiteral(result),
            None     => tokens::Error("Hit eof before end of string literal.".to_string())
        }
    }    
}

use syntax::core::tokens::Token;
use syntax::core::tokens::Token::*;
use syntax::core::types::Types;
use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;

pub trait Tokenizer {
    fn get_tok(&mut self) -> Token;
    fn get_error_pos(&self) -> (usize, usize, usize, usize);
}

// A Lexer that keeps track of the current line and column position
// as well as the position in the char input stream.
pub struct Lexer<'a> {
    line_number: usize,
    column_number: usize,
    error_start: (usize, usize),
    error_end: (usize, usize),
    buffer_pos: usize,
    input: &'a str,
    lines: Vec<&'a str>
}

impl<'a> Tokenizer for Lexer<'a> {
    // Parse the file where it left off and return the next token
    fn get_tok(&mut self) -> Token {
        if self.eof() {
            return EOF;
        }

        self.consume_whitespace();

        self.error_start = (self.line_number, self.column_number);
        
        let tok = match self.next_char() {
            // Find Keywords and Identifiers
            Some(a) if a.is_alphabetic() || a == '_' => self.consume_identifier(),

            // Find ints, floats, hex, and bin numeric values
            Some(n) if n.is_digit(10) => self.consume_numeric(),

            // Count tabs: \n\t*
            Some('\n') => self.consume_tabs(),

            // Error: Found tabs without preceeding newline
            Some('\t') => {
                self.consume_char().unwrap();
                
                Error("Found an out of place tab.".to_string())
            },
            
            // Find single-char punctuations
            Some('(') | Some(')') |
            Some('[') | Some(']') |
            Some('{') | Some('}') |
            Some('.') |
            Some(',') |
            Some(':') |
            Some('^') |
            Some('~') |
            Some('=') => {
                let punc = self.consume_char().unwrap().to_string();

                self.punctuation_token(&punc)
            },

            // Find multi-char(+=, -=, ..) or the single-char version
            Some('+') |
            Some('*') |
            Some('/') |
            Some('%') => {
                let mut punc = self.consume_char().unwrap().to_string();

                if let Some('=') = self.next_char() {
                    punc.push(self.consume_char().unwrap());
                }   

                self.punctuation_token(&punc)
            },

            // Find -, -=, -> punctuation
            Some('-') => {
                let mut punc = self.consume_char().unwrap().to_string();

                // = and > are adjacent chars, provides a nice if let:
                if let Some('='...'>') = self.next_char() {
                    punc.push(self.consume_char().unwrap())
                }

                self.punctuation_token(&punc)
            },

            // Find >> and >>> comments, otherwise > or >= punctuation
            Some('>') => {
                self.consume_char();
                
                match self.next_char() {
                    Some('>') => self.consume_comment(),
                    Some('=') => {
                        self.consume_char();

                        self.punctuation_token(">=")
                    },
                    _ => self.punctuation_token(">")
                }
            },

            // Find < and <= punctuation
            Some('<') => {
                self.consume_char();

                match self.next_char() {
                    Some('=') => {
                        self.consume_char();

                        self.punctuation_token("<=")
                    },
                    _ => self.punctuation_token("<")
                }
            },

            // Find character literals, 'c', including ascii escape chars
            Some('\'') => self.consume_char_literal(),

            // Find string literals, "String"
            Some('\"') => self.consume_string_literal(),
            
            Some(ch) => Error(format!("Unknown character '{}'.", ch)),
            
            None => EOF
        };

        self.error_end = (self.line_number, self.column_number);

        tok
    }

    fn get_error_pos(&self) -> (usize, usize, usize, usize) {
        let (start_line, start_column) = self.error_start;
        let (end_line, end_column) = self.error_end;

        (start_line, start_column, end_line, end_column)
    }
}

impl<'a> Lexer<'a> {
    // Create a new lexer instance
    pub fn new(slice: &'a str) -> Lexer<'a> {
        Lexer {
            line_number: 1,
            column_number: 1,
            error_start: (0, 0),
            error_end: (0, 0),
            buffer_pos: 0,
            input: slice,
            lines: slice.lines().collect()
        }
    }

    // get_line takes a line number(1...self.lines.len()), not an index (0...self.lines.len()-1)
    pub fn get_line(&mut self, line: usize) -> Option<&str> {
        match line {
            // Out of bounds
            l if l == 0 => None,
            l if l > self.lines.len() => None,

            // Return the line
            _ => Some(self.lines[line - 1])
        }
    }

    fn current_slice(&self) -> &str {
        &self.input[self.buffer_pos..]
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> Option<char> {
        match self.next_char() {
            Some(ch) => {
                self.buffer_pos += 1;
                self.column_number += 1;
                
                if ch == '\n' {
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

    fn consume_while<F: FnMut(char) -> bool>(&mut self, test: &mut F) -> String {
        let mut result = String::new();

        // Always unwrapping as the loop checks eof.
        while !self.eof() && test(self.next_char().unwrap()) {
            match self.consume_char().unwrap() {
                // Ignore any carriage returns
                '\r' => continue,
                ch   => result.push(ch)
            };
        };

        result
    }

    // Single and multi char punctuations: *, -, +=, -=, ...
    fn punctuation_token(&self, punc: &str) -> Token {
        Punctuation(match punc.parse::<Punctuations>() {
            Ok(p)  => p,
            Err(e) => panic!(e)
        })
    }

    // Consume non newline whitespace
    fn consume_whitespace(&mut self) {
        self.consume_while(&mut |ch| match ch {
            '\n' | '\t'            => false,
            w if w.is_whitespace() => true,
            _                      => false
        });
    }

    // Identifiers: [a-zA-Z_][a-zA-z0-9_]*
    // Keywords and Types are subsets of identifiers.
    fn consume_identifier(&mut self) -> Token {
        // Lexer will only let you start with alpha or undescore,
        // so there is no need to check for numeric start
        let ident = self.consume_while(&mut |ch| match ch {
            a if a.is_alphanumeric() => true,
            '_'                      => true,
             _                       => false
        });

        match &ident[..] {
            "True"  => return BoolLiteral(true),
            "False" => return BoolLiteral(false),
            _       => ()
        };

        if let Ok(key) = ident.parse::<Keywords>() {
            return Keyword(key);
        }

        if let Ok(t) = ident.parse::<Types>() {
            return Type(t);
        }

        Identifier(ident)
    }

    // Find a sequence of 32 or 64
    fn consume_32_64(&mut self, prefix: char) -> Result<String, String> {
        // prefix is the starting character, ie i, u, f
        let mut string = String::new();
        string.push(prefix);

        match self.next_char() {
            Some('3') => {
                string.push(self.consume_char().unwrap());
                
                match self.next_char() {
                    Some('2')  => {
                        string.push(self.consume_char().unwrap());

                        Ok(string)
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {}32?", string, prefix)),
                    Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}2?", string, self.consume_char().unwrap())),
                    None       => Err(format!("Hit EOF when looking for suffix {}32.", prefix))
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
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {}64?", string, prefix)),
                    Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}4?", string, self.consume_char().unwrap())),
                    None       => Err(format!("Hit EOF when looking for suffix {}64.", prefix))
                }
            },
            Some('\n') | // NL & CR have pesky visual effects.
            Some('\r') => Err(format!("Invalid suffix {}. Did you mean {1}32 or {1}64?", string, prefix)),
            Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}32 or {0}64?", string, self.consume_char().unwrap())),
            None       => Err(format!("Hit EOF when looking for a suffix {0}32 or {0}64.", prefix))
        }
    }

    // Determines what type of number it is and consume it
    fn consume_numeric(&mut self) -> Token {
        let mut number = String::new();
        let mut suffix = String::new();

        if self.current_slice().starts_with("0x") {
            // Found hexadecimal: 0x[0-9a-fA-F_]+

            self.buffer_pos += 2;
            number.push_str("0x");

            // Cant do += for String, and push_str looks better than
            // number = number + self.consume...
            number.push_str(&self.consume_while(&mut |ch| match ch {
                '0'...'9' |
                'a'...'f' |
                'A'...'F' |
                '_' => true,
                 _  => false
            }));

            if &number[..] == "0x" {
                return Error("No hexadecimal value was found.".to_string());
            }

            // Attempt to find a suffix if one exists
            match self.next_char() {
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();
                    
                    match self.consume_32_64(ch) {
                        Ok(s)    => suffix.push_str(&s),
                        Err(err) => return Error(err)
                    };
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphanumeric() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return Error(err.to_string());
                },
                
                // If eof or other just return the numeric token without a suffix
                _ => ()
            };

        } else if self.current_slice().starts_with("0b") {
            // Found binary: 0b[01_]+

            self.buffer_pos += 2;
            number.push_str("0b");

            // Formatting the same as the hex case above.
            number.push_str(&self.consume_while(&mut |ch| match ch {
                '0' |
                '1' |
                '_' => true,
                 _  => false
            }));

            if &number[..] == "0b" {
                return Error("No binary value was found.".to_string());
            }

            // Attempt to find a suffix if one exists
            match self.next_char() {
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();

                    match self.consume_32_64(ch) {
                        Ok(s)    => suffix.push_str(&s),
                        Err(err) => return Error(err)
                    };
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphabetic() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return Error(err.to_string());
                },
                
                // If eof or other just return the numeric token without a suffix
                _ => ()
            };
            
        } else {
            // Found int: [0-9]+ or float: [0-9]+.[0-9]+

            number.push_str(&self.consume_while(&mut |ch| match ch {
                '0'...'9' |
                '_' => true,
                 _  => false
            }));

            match self.next_char() {
                // Float decimal point:
                Some('.') => {
                    number.push(self.consume_char().unwrap());

                    let fractional = self.consume_while(&mut |ch| match ch {
                        '0'...'9' |
                        '_' => true,
                         _  => false
                    });

                    // Check if no decimal values were found
                    match &fractional[..] {
                        "" => return Error("Invalid floating point number.".to_string()),
                        _  => number.push_str(&fractional)
                    }

                    // Find float suffixes
                    match self.next_char() {
                        Some('f') => {
                            let ch = self.consume_char().unwrap();

                            match self.consume_32_64(ch) {
                                Ok(s)    => suffix.push_str(&s),
                                Err(err) => return Error(err)
                            };
                        },

                        // Found some other suffix, ie 0x42o
                        Some(c) if c.is_alphabetic() => {
                            let ch = self.consume_char().unwrap();
                            let err = format!("Invalid suffix {}. Did you mean f32, f64?", ch);

                            return Error(err.to_string());
                        },
                        
                        // No suffix found, can hit punctuation or other
                        _ => ()
                    }
                },

                // Int suffixes:
                Some('u') |
                Some('i') => {
                    let ch = self.consume_char().unwrap();
                    
                    match self.consume_32_64(ch) {
                        Ok(s)    => suffix.push_str(&s),
                        Err(err) => return Error(err)
                    };
                },

                // Found some other suffix, ie 0x42o
                Some(c) if c.is_alphabetic() => {
                    let ch = self.consume_char().unwrap();
                    let err = format!("Invalid suffix {}. Did you mean u32, u64, i32, or i64?", ch);

                    return Error(err.to_string());
                },

                // Presumably any other remaining char is valid, ie punctuation {,[ etc
                _ => ()
            };
        }

        Numeric(number, suffix.parse::<Types>().ok())
     }

    fn consume_comment(&mut self) -> Token {
        let mut result = String::new();

        // Consume 2nd '>'
        self.consume_char();

        match self.next_char() {
            // Multiline comments must end in <<< else error
            Some('>') => {
                let mut sequence = 0usize;

                // Consume 3rd '>'
                self.consume_char();

                result.push_str(&self.consume_while(&mut |ch| match ch {
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
                }));

                // Should be able to consume the last <
                match self.consume_char() {
                    Some('<') => (),
                    _         => return Error("Hit eof before end of multi-line comment.".to_string())
                }

                // Remove << from end of the comment string
                result.pop();
                result.pop();
            },

            // Single line comments eat up anything until newline or eof
            Some(_) => {
                result.push_str(&self.consume_while(&mut |ch| match ch {
                    '\n' => false,
                    _ => true
                }));
            },

            // Single line comment w/ EOF at start should be valid:
            None => ()
        }
        
        Comment(result)
    }

    fn consume_tabs(&mut self) -> Token {
        let mut count = 0usize;

        // Consume the newline token, count tabs
        self.consume_char();
        self.consume_while(&mut |ch| match ch {
            '\t' => {
                count += 1;
                true
            },
            _    => false
        });

        Indent(count)
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
            _   => Err(format!("Unknown character escape: \\{}", ch))
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
                            Err(msg) => return Error(msg)
                        };
                    },
                    None => return Error("Hit eof before end of character literal.".to_string())
                };
            },
            Some('\'') => return Error("Empty character literal is invalid.".to_string()),
            Some(c)    => ch = c,
            None       => return Error("Hit eof before end of character literal.".to_string())
        };

        // Get the final '
        match self.consume_char() {
            Some('\'') => CharLiteral(ch),
            _          => Error("Char literal was not closed with a '".to_string())
        }
    }

    // This is currently set up to accept multi line strings
    fn consume_string_literal(&mut self) -> Token {
        let mut result = String::new();

        // Consume first "
        self.consume_char();

        // Consume until closing "
        loop {
            match self.consume_char() {
                // Ignore any carriage returns
                Some('\r') => continue,

                // Handle Escape chars
                Some('\\') => {
                    if let Some(ch) = self.consume_char() {
                        match Lexer::escape_char(ch) {
                            Ok(ch) => result.push(ch),
                            Err(e) => return Error(e)
                        }
                    }
                },

                // End at a closing "
                Some('\"') => return StrLiteral(result),
                Some(ch)   => result.push(ch),
                None       => break
            };
        };

        return Error("Hit EOF before end of string literal.".to_string());
    }    
}

use std::iter;
use std::str;

use lexical::tokens::Tokens;
use lexical::tokens::Tokens::*;
use lexical::types::Types;
use lexical::keywords::Keywords;
use lexical::symbols::Symbols;

// A Lexer that keeps track of the current line and column position
// as well as the position in the char input stream.
pub struct Lexer<'a> {
    line_number: usize,
    column_number: usize,
    input: iter::Peekable<str::CharIndices<'a>>,
}


impl<'a> Lexer<'a> {
    // Create a new lexer instance
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            line_number: 1,
            column_number: 1,
            input: input.char_indices().peekable(),
        }
    }

    // Gets the next char and sets the position forward in the buffer
    fn consume_char(&mut self) -> Option<char> {
        if let Some(result) = self.input.next() {
            let (_, chr) = result;
            self.column_number += 1;

            if chr == '\n' {
                self.line_number += 1;
                self.column_number = 1;
            }

            return Some(chr);
        }

        None
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(result) = self.input.peek() {
            let &(_, chr) = result;
            return Some(chr);
        }

        None
    }

    fn consume_while<F: FnMut(char) -> bool>(&mut self, test: &mut F) -> String {
        let mut result = String::new();

        // Always unwrapping as the loop checks eof.
        while self.next_char().is_some() && test(self.next_char().unwrap()) {
            match self.consume_char().unwrap() {
                // Ignore any carriage returns
                '\r' => continue,
                ch   => result.push(ch)
            };
        };

        result
    }

    // Single and multi char symbols: *, -, +=, -=, ...
    fn symbols_token(&self, punc: &str) -> Tokens {
        Symbol(match punc.parse::<Symbols>() {
            Ok(p)  => p,
            Err(e) => unreachable!(e)
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
    // Keywords are subsets of identifiers.
    fn consume_identifier(&mut self) -> Tokens {
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

        Identifier(ident)
    }

    // Determines what type of number it is and consume it
    fn consume_numeric(&mut self) -> Tokens { // TODO: Change Error token to Result
        let mut number = String::new();
        let mut suffix = None;

        match self.next_char() {
            Some('0') => {
                self.consume_char();
                match self.next_char() {
                    Some('x') => {
                        // Found hexadecimal: 0x[0-9a-fA-F_]+
                        self.consume_char();

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

                        match self.consume_numeric_suffix() {
                            Ok(Some(t)) => suffix = Some(t),
                            Ok(None) => (),
                            Err(e) => return Error(e)
                        };
                    },
                    Some('b') => {
                        // Found binary: 0b[01_]+
                        self.consume_char();

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

                        match self.consume_numeric_suffix() {
                            Ok(Some(t)) => suffix = Some(t),
                            Ok(None) => (),
                            Err(e) => return Error(e)
                        };
                    },
                    _ => {
                        // REVIEW: Better way to do this than two allocations?
                        // Maybe a consume_while_append which adds to an existing string
                        // instead of returning one? Happens elsewhere too
                        number.push('0');
                        number.push_str(&self.consume_while(&mut |ch| match ch {
                            '0'...'9' |
                            '_' => true,
                             _  => false
                        }));

                        // Can be either float or int
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
                                    "" => return Error("No numbers found after the decimal point.".to_string()),
                                    _  => number.push_str(&fractional)
                                }

                                match self.consume_numeric_suffix() {
                                    Ok(Some(t)) => suffix = Some(t),
                                    Ok(None) => (),
                                    Err(e) => return Error(e)
                                };
                            },
                            _ => match self.consume_numeric_suffix() {
                                Ok(Some(t)) => suffix = Some(t),
                                Ok(None) => (),
                                Err(e) => return Error(e)
                            }
                        };
                    }
                }
            },
            _ => {
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
                            "" => return Error("No numbers found after the decimal point.".to_string()),
                            _  => number.push_str(&fractional)
                        }

                        match self.consume_numeric_suffix() {
                            Ok(Some(t)) => suffix = Some(t),
                            Ok(None) => (),
                            Err(e) => return Error(e)
                        };
                    },
                    _ => match self.consume_numeric_suffix() {
                        Ok(Some(t)) => suffix = Some(t),
                        Ok(None) => (),
                        Err(e) => return Error(e)
                    }
                };
            },
        }

        Numeric(number, suffix)
     }

    fn consume_numeric_suffix(&mut self) -> Result<Option<Types>, String> {
        let mut suffix = String::with_capacity(4);

        match self.next_char() {
            Some('u') |
            Some('i') => {
                suffix.push(self.consume_char().unwrap());
                self.consume_numeric_suffix_end(&mut suffix, true)?;

                Ok(suffix.parse::<Types>().ok())
            },
            Some('f') => {
                suffix.push(self.consume_char().unwrap());
                self.consume_numeric_suffix_end(&mut suffix, false)?;

                Ok(suffix.parse::<Types>().ok())
            },
            Some(c) if c.is_alphabetic() => {
                let ch = self.consume_char().unwrap();
                let err = format!("Invalid suffix {}. Did you mean u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, or f128?", ch);

                Err(err)
            },
            Some(_) => Ok(None),
            None => Ok(None)
        }
    }

    // Find a sequence of 8, 16 (if allowed), 32, 64, or 128
    fn consume_numeric_suffix_end(&mut self, buffer: &mut String, allow_8_16: bool) -> Result<(), String> {
        // buffer should contain the starting character, ie i, u, f
        match self.next_char() {
            Some('8') if allow_8_16 => {
                buffer.push(self.consume_char().unwrap());

                Ok(())
            },
            Some('1') => {
                buffer.push(self.consume_char().unwrap());

                match self.next_char() {
                    Some('6') if allow_8_16 => {
                        buffer.push(self.consume_char().unwrap());

                        Ok(())
                    },
                    Some('2') => {
                        buffer.push(self.consume_char().unwrap());

                        match self.next_char() {
                            Some('8') => {
                                buffer.push(self.consume_char().unwrap());

                                Ok(())
                            },
                            Some('\n') | // NL & CR have pesky visual effects.
                            Some('\r') => Err(format!("Invalid suffix {}. Did you mean {0}8?", buffer)),
                            Some(_) => Err(format!("Invalid suffix {}{}. Did you mean {0}8?", buffer, self.consume_char().unwrap())),
                            None => Err(format!("Hit EOF when looking for suffix {}8", buffer)),
                        }
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {0}6 or {0}28?", buffer)),
                    Some(_) => if allow_8_16 {
                        Err(format!("Invalid suffix {}{}. Did you mean {0}6 or {0}28?", buffer, self.consume_char().unwrap()))
                    } else {
                        Err(format!("Invalid suffix {}{}. Did you mean {0}28?", buffer, self.consume_char().unwrap()))
                    },
                    None => if allow_8_16 {
                        Err(format!("Hit EOF when looking for suffix {}16 or {0}128.", buffer))
                    } else {
                        Err(format!("Hit EOF when looking for suffix {}128", buffer))
                    }
                }
            },
            Some('3') => {
                buffer.push(self.consume_char().unwrap());

                match self.next_char() {
                    Some('2')  => {
                        buffer.push(self.consume_char().unwrap());

                        Ok(())
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {0}2?", buffer)),
                    Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}2?", buffer, self.consume_char().unwrap())),
                    None       => Err(format!("Hit EOF when looking for suffix {}32.", buffer))
                }
            },
            Some('6') => {
                buffer.push(self.consume_char().unwrap());

                match self.next_char() {
                    Some('4')  => {
                        buffer.push(self.consume_char().unwrap());

                        Ok(())
                    },
                    Some('\n') | // NL & CR have pesky visual effects.
                    Some('\r') => Err(format!("Invalid suffix {}. Did you mean {0}4?", buffer)),
                    Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}4?", buffer, self.consume_char().unwrap())),
                    None       => Err(format!("Hit EOF when looking for suffix {}64.", buffer))
                }
            },
            Some('\n') | // NL & CR have pesky visual effects.
            Some('\r') => Err(format!("Invalid suffix {}. Did you mean {0}32, {0}64, or {0}128?", buffer)),
            Some(_)    => Err(format!("Invalid suffix {}{}. Did you mean {0}32, {0}64, or {0}128?", buffer, self.consume_char().unwrap())),
            None       => Err(format!("Hit EOF when looking for a suffix {0}32, {0}64, or {0}128.", buffer))
        }
    }

    fn consume_comment(&mut self) -> Tokens {
        let mut result = String::new();

        // Consume 2nd '>'
        self.consume_char();

        match self.next_char() {
            // Multiline comments must end in <<< else error
            Some('>') => {
                let mut sequence = 0u8;

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
                if self.consume_char() != Some('<') {
                    return Error("Hit eof before end of multi-line comment.".to_string())
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

    fn consume_tabs(&mut self) -> Tokens {
        let mut count = 0u64;

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
            '\n'=> Ok(' '), // Escape newline?
            _   => Err(format!("Unknown character escape: \\{}", ch))
        }
    }

    fn consume_char_literal(&mut self) -> Tokens {
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
    fn consume_string_literal(&mut self) -> Tokens {
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

        Error("Hit EOF before end of string literal.".to_string())
    }
}


impl<'a> Iterator for Lexer<'a> {
    type Item = Tokens;

    // Parse the file where it left off and return the next token
    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

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

            // Find single-char symbols
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

                self.symbols_token(&punc)
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

                self.symbols_token(&punc)
            },

            // Find -, -=, -> symbols
            Some('-') => {
                let mut punc = self.consume_char().unwrap().to_string();

                // = and > are adjacent chars, provides a nice if let:
                if let Some('='...'>') = self.next_char() {
                    punc.push(self.consume_char().unwrap())
                }

                self.symbols_token(&punc)
            },

            // Find >> and >>> comments, otherwise > or >= symbols
            Some('>') => {
                self.consume_char();

                match self.next_char() {
                    Some('>') => self.consume_comment(),
                    Some('=') => {
                        self.consume_char();

                        self.symbols_token(">=")
                    },
                    _ => self.symbols_token(">")
                }
            },

            // Find < and <= symbols
            Some('<') => {
                self.consume_char();

                match self.next_char() {
                    Some('=') => {
                        self.consume_char();

                        self.symbols_token("<=")
                    },
                    _ => self.symbols_token("<")
                }
            },

            // Find character literals, 'c', including ascii escape chars
            Some('\'') => self.consume_char_literal(),

            // Find string literals, "String"
            Some('\"') => self.consume_string_literal(),

            Some(ch) => Error(format!("Unknown character ({}).", ch)),

            None => return None,
        };

        Some(tok)
    }
}

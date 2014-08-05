use std::io::BufferedReader;
use std::io::File;
use std::string::String;

use syntax::lexer::Lexer;
use syntax::core::tokens;
use syntax::core::tokens::Token;
use syntax::core::keywords;
use syntax::core::keywords::Keywords;

pub struct Error {
    pub line: u64,
    pub column: u64,
    pub messgage: String,
}

enum Expr {
    Integer(IntegerAST),
    UInteger(UIntegerAST),
    Float(FloatAST),
}

impl Expr {
    fn gen_code(&self) -> () {
        match *self {
            Integer(node) => (),
            UInteger(node) => (),
            Float(node) => (),
        }
    }
}

struct IntegerAST {
    val: i64,
}

struct UIntegerAST {
    val: u64,
}


struct FloatAST {
    val: f64,
}

pub struct Parser {
    lexer: Lexer<BufferedReader<File>>,
    indentation: u64,
}

impl Parser {
    pub fn new(filename: &str) -> Parser {
        let path = Path::new(filename);
        let file = match File::open(&path) {
            Ok(f)  => f,
            Err(e) => fail!("Failed to open file. File error: {}", e),
        };
        Parser {
            lexer: Lexer::new(BufferedReader::new(file)),
            indentation: 0,
        }
    }

    // Get the next token from the lexer
    fn get_next_token(&mut self) -> Token {
        self.lexer.get_tok()
    }

    // Create an error from the current lexer's
    // state, and a message
    fn throw_error(&self, msg: String) -> Error {
        Error {
            line: self.lexer.line_number as u64,
            column: self.lexer.column_number as u64,
            messgage: msg,
        }
    }

    // Perform any necessary on-start actions
    fn start(&self) {
        // Start code gen startup

        // Implicit imports

        // Generate code for implicit top level function definition
    }

    // Ensures that the indentation matches the current level of indentation
    fn check_indentation(&self, depth: u64) -> Result<Option<Expr>, Error> {
        if self.indentation == depth {
            return Ok(None);
        }
        Err(self.throw_error(format!("Incorrect indentation level. Expected {}, but was {}.",
                                     self.indentation, depth)))
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&self, keyword: Keywords) -> Result<Option<Expr>, Error> {
        Ok(None)
    }

    fn parse_expression(&self, name: String) -> Result<Option<Expr>, Error> {
        Ok(None)
    }

    // Parse numbers into their correct representation
    fn parse_number(&mut self, num: &str) -> Result<Option<Expr>, Error> {
        println!("{}", num);
        let value = 0;
        // This would normally check the ending on the thing slash information
        // about the number to pick int, uint, or float
        Ok(Some(Integer(IntegerAST { val: value })))
    }

    // Parse the file
    pub fn parse(&mut self) {
        loop {
            // Parse the token into the next node of the AST
            let result: Result<Option<Expr>, Error>;

            let cur_token = self.get_next_token();

            result = match cur_token {
                tokens::Start => {
                    self.start();
                    Ok(None)
                }
                tokens::Keyword(word) => {
                    self.handle_keywords(word)
                }
                tokens::Identifier(repr) => {
                    self.parse_expression(repr)
                }
                tokens::EOF => {
                    break
                }
                _ => {
                    fail!("Invalid token at the top level {}", cur_token);
                }
            };

            // `result` can either raise an error or not return an AST,
            // deal with its potential values
            match result {
                Ok(value) => {
                    match value {
                        Some(node) => {
                            node.gen_code();
                        }
                        None => {
                        }
                    }
                }
                Err(e) => {
                    fail!("Error on line: {} col: {}, {}", e.line, e.column, e.messgage);
                }
            }
        }
    }
}

use std::string::String;
use syntax::lexer::Tokenizer;
use syntax::core::tokens::Token;
use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;

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
            Expr::Integer(node) => (),
            Expr::UInteger(node) => (),
            Expr::Float(node) => (),
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

pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    indentation: u64,
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            indentation: 0,
        }
    }

    // Get the next token from the lexer
    fn next_token(&mut self) -> Token {
        self.lexer.get_tok()
    }

    // Create an error from the current lexer's
    // state, and a message
    fn write_error(&self, msg: &str) -> Error {
        Error {
            line: 1,
            column: 1,
            messgage: msg.to_string(),
        }
    }

    fn expect_error(&self, reason: &str, expect: &str, got: &str) -> Error {
        self.write_error(format!("{}. Expected {}, but got {}", reason, expect, got).as_slice())
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
        Err(self.write_error("Incorrect indentation level"))
    }

    // Parses a variable or constant declaration
    fn parse_declaration(&mut self) -> Result<Option<Expr>, Error> {
        let token = self.next_token();
        match token {
            Token::Identifier(name) => {
                let token1 = self.next_token();
                match token1 {
                    // Token::Punctuation(punc) => {
                    //     match punc {
                    //         punctuation::Assign => {
                    //             self.parse_expression()
                    //         }
                    //         _ => Err(self.write_error("Invalid Punctuation here"))
                    //     }
                    // }
                    Token::Punctuation(Punctuations::Equals) => {
                        self.parse_expression()
                    }
                    _ => Err(self.write_error("Invalid sequence"))
                }
            },
            Token::Error(msg) => Err(self.write_error(msg.as_slice())),
            _ => Err(self.expect_error("", "a variable name", "something else"))
        }
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&mut self, keyword: Keywords) -> Result<Option<Expr>, Error> {
        match keyword {
            Keywords::Def => {
                self.parse_declaration()
            }
            _ => Err(self.write_error(format!("Unsupported keyword {}.", keyword).as_slice()))
        }
    }

    fn parse_expression(&self) -> Result<Option<Expr>, Error> {
        Ok(None)
    }

    // Parse numbers into their correct representation
    fn parse_number(&mut self, num: &str) -> Result<Option<Expr>, Error> {
        println!("{}", num);
        let value = 0;
        // This would normally check the ending on the thing slash information
        // about the number to pick int, uint, or float
        Ok(Some(Expr::Integer(IntegerAST { val: value })))
    }

    // Parse the file
    pub fn parse(&mut self) {
        self.start();
        let cur_token = self.next_token();
        loop {
            // Parse the token into the next node of the AST
            let result: Result<Option<Expr>, Error>;

            result = match cur_token {
                Token::Keyword(ref keyword) => {
                    self.handle_keywords(*keyword)
                }
                Token::Identifier(ref repr) => {
                    self.parse_expression()
                }
                Token::EOF => {
                    break
                }
                _ => {
                    panic!("Invalid token at the top level {}", cur_token);
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
                    panic!("Error on line: {} col: {}, {}", e.line, e.column, e.messgage);
                }
            }
        }
    }
}

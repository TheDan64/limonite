use std::string::String;
use syntax::lexer::Tokenizer;
use syntax::core::tokens::Token;
use syntax::core::tokens::Token::*;
use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;
use syntax::ast::expr::Expr;

pub struct ErrorMsg {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

#[allow(dead_code)]
enum OldExpr {
    Integer(IntegerAST),
    UInteger(UIntegerAST),
    Float(FloatAST),
}

impl OldExpr {
    #[allow(unused_variables)]
    fn gen_code(&self) -> () {
        match *self {
            OldExpr::Integer(ref node) => (),
            OldExpr::UInteger(ref node) => (),
            OldExpr::Float(ref node) => (),
        }
    }
}

#[allow(dead_code)]
struct IntegerAST {
    val: i64,
}

#[allow(dead_code)]
struct UIntegerAST {
    val: u64,
}

#[allow(dead_code)]
struct FloatAST {
    val: f64,
}

#[allow(dead_code)]
pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    indentation: u64,
    run_codegen: bool
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            indentation: 0,
            run_codegen: true
        }
    }

    // Get the next token from the lexer
    fn next_token(&mut self) -> Token {
        self.lexer.get_tok()
    }

    // Create an error from the current lexer's
    // state, and a message
    fn write_error(&self, msg: &str) -> ErrorMsg {
        let (start_line, start_column, _, _) = self.lexer.get_error_pos();

        ErrorMsg {
            line: start_line,
            column: start_column,
            message: msg.to_string(),
        }
    }

    fn expect_error(&self, reason: &str, expect: &str, got: &str) -> ErrorMsg {
        self.write_error(&format!("{}. Expected {}, but got {}", reason, expect, got))
    }

    // Perform any necessary on-start actions
    fn start(&self) {
        // Start code gen startup

        // Implicit imports

        // Generate code for implicit top level function definition
    }

    // Ensures that the indentation matches the current level of indentation
    #[allow(dead_code)]
    fn check_indentation(&self, depth: u64) -> Result<Option<OldExpr>, ErrorMsg> {
        if self.indentation == depth {
            return Ok(None);
        }
        Err(self.write_error("Incorrect indentation level"))
    }

    // Parses a variable or constant declaration
    #[allow(unused_variables)]
    fn parse_declaration(&mut self) -> Result<Option<OldExpr>, ErrorMsg> {
        let token = self.next_token();
        match token {
            Identifier(name) => {
                let token1 = self.next_token();
                match token1 {
                    // Punctuation(punc) => {
                    //     match punc {
                    //         punctuation::Assign => {
                    //             self.parse_expression()
                    //         }
                    //         _ => Err(self.write_error("Invalid Punctuation here"))
                    //     }
                    // }
                    Punctuation(Punctuations::Equals) => {
                        self.parse_expression()
                    },
                    _ => Err(self.write_error("Invalid sequence"))
                }
            },
            Error(msg) => Err(self.write_error(&msg)),
            _ => Err(self.expect_error("", "a variable name", "something else"))
        }
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&mut self, keyword: Keywords) -> Result<Option<OldExpr>, ErrorMsg> {
        match keyword {
            Keywords::Def => {
                self.parse_declaration()
            },
            _ => Err(self.write_error(&format!("Unsupported keyword {:?}.", keyword)))
        }
    }

    fn parse_expression(&self) -> Result<Option<OldExpr>, ErrorMsg> {
        Ok(None)
    }

    // Parse numbers into their correct representation
    #[allow(dead_code)]
    fn parse_number(&mut self, num: &str) -> Result<Option<OldExpr>, ErrorMsg> {
        println!("{}", num);
        let value = 0;
        // This would normally check the ending on the thing slash information
        // about the number to pick int, uint, or float
        Ok(Some(OldExpr::Integer(IntegerAST { val: value })))
    }

    // Parse the file
    #[allow(unused_variables)]
    pub fn parse(&mut self) {
        self.start();

        loop {
            // Parse the token into the next node of the AST
            let result: Result<Option<OldExpr>, ErrorMsg>;
            let token = self.next_token();

            // Debug:
            println!("{:?}", token);

            result = match token {
                Numeric(string, type_) => {
                    panic!("Unimplemented top level token 'Numeric'");
                },
                Identifier(repr) => {
                    self.parse_expression()
                },
                Indent(depth) => {
                    // ToDo: Keep track of indentation level when preceeding a statement
                    Ok(None)
                },
                BoolLiteral(lit) => {
                    panic!("Unimplemented top level token 'BoolLiteral'");
                },
                CharLiteral(lit) => {
                    panic!("Unimplemented top level token 'CharLiteral'");
                },
                StrLiteral(lit) => {
                    panic!("Unimplemented top level token 'StrLiteral'");
                },
                Keyword(keyword) => {
                    self.handle_keywords(keyword)
                },
                Punctuation(punc) => {
                    panic!("Unimplemented top level token 'Punctuation'");
                },
                Comment(text) => {
                    // ToDo: Docstring, else ignore
                    Ok(None)
                },
                Error(err) => {
                    Err(self.write_error(&err))
                },
                Type(type_) => {
                    panic!("Unimplemented top level token 'Type'");
                },
                EOF => {
                    Ok(None)
                }
            };

            // `result` can either raise an error or not return an AST,
            // deal with its potential values
            match result {
                Ok(value) => {
                    match value {
                        Some(node) => {
//                            node.gen_code();
                        },
                        None => {
                        }
                    }
                },
                Err(e) => {
                    panic!("Error on line: {} col: {}, {}", e.line, e.column, e.message);
                }
            };
        }
    }
}

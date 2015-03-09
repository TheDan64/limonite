use syntax::lexer::Tokenizer;
use syntax::core::tokens::Token;
use syntax::core::tokens::Token::*;
use syntax::core::keywords::Keywords;
use syntax::core::punctuation::Punctuations;
use syntax::ast::expr::*;

#[allow(dead_code)]
enum OldExpr {
    Integer(IntegerAST),
    UInteger(UIntegerAST),
    Float(FloatAST),
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

pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    indentation: u64,
    run_codegen: bool,
    ast_root: ExprWrapper
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        let expr = Box::new(Expr::BlockExpr(Vec::new()));
        let wrapper = ExprWrapper::new(expr, 0, 0, 0, 0);

        Parser {
            lexer: tokenizer,
            indentation: 0,
            run_codegen: true,
            ast_root: wrapper
        }
    }

    // Get the next token from the lexer
    fn next_token(&mut self) -> Token {
        self.lexer.get_tok()
    }

    // Create an error from the current lexer's
    // state, and a message
    fn write_error(&mut self, msg: &str) {
        let (start_line, start_column, _, _) = self.lexer.get_error_pos();
        
        self.run_codegen = false;

        println!("Error on line: {} col: {}, {}", start_line, start_column, msg);

        // Skip to the end of the line (at Indent token) and allow parsing to continue
        loop {
            // Note: This consumes the Indent token, which probably isnt ideal.
            match self.next_token() {
                Indent(_) => break,
                EOF => break,
                _ => ()
            }
        }
    }

    fn expect_error(&mut self, reason: &str, expect: &str, got: &str) {
        self.write_error(&format!("{}. Expected {}, but got {}", reason, expect, got));
    }

    // Perform any necessary on-start actions
    fn start(&self) {
        // Start code gen startup

        // Implicit imports

        // Generate code for implicit top level function definition
    }

    // Ensures that the indentation matches the current level of indentation
    #[allow(dead_code)]
    fn check_indentation(&mut self, depth: u64) -> Option<OldExpr> {
        if self.indentation == depth {
            return None;
        }
        
        self.write_error("Incorrect indentation level");

        None
    }

    // Parses a variable or constant declaration
    #[allow(unused_variables)]
    fn parse_declaration(&mut self) -> Option<OldExpr> {
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
                        return self.parse_expression();
                    },
                    _ => self.write_error("Invalid sequence")
                }
            },
            Error(msg) => self.write_error(&msg),
            _ => self.expect_error("", "a variable name", "something else")
        }

        None
    }

    // Parses a print function/statement
    fn parse_print_fn(&mut self) -> Option<ExprWrapper> {


        None
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&mut self, keyword: Keywords) -> Option<ExprWrapper> {
        match keyword {
//            Keywords::Def => {
//                self.parse_declaration()
//            },
            Keywords::Print => {
                self.parse_print_fn()
            },
            _ => {
                self.write_error(&format!("Unsupported keyword {:?}.", keyword));
                None
            }
        }
    }

    fn parse_expression(&self) -> Option<OldExpr> {
        None
    }

    // Parse numbers into their correct representation
    #[allow(dead_code)]
    fn parse_number(&mut self, num: &str) -> Option<OldExpr> {
        println!("{}", num);
        let value = 0;
        // This would normally check the ending on the thing slash information
        // about the number to pick int, uint, or float
        Some(OldExpr::Integer(IntegerAST { val: value }))
    }

    // Parse the file
    #[allow(unused_variables)]
    pub fn parse(&mut self) {
        self.start();

        // Generate AST
        loop {
            // Parse the token into the next node of the AST
            let token = self.next_token();

            // Debug:
            println!("{:?}", token);

            match token {
                Numeric(string, type_) => {
                    // If we had an intepreter mode, just print the value.
                    panic!("Unimplemented top level token 'Numeric'");
                },
                Identifier(repr) => {
                    // Interpreter: print the value or representation
//                    self.parse_expression()
                    panic!("Unimplemented top level token 'Identifier'");
                },
                Indent(depth) => {
                    // ToDo: Keep track of indentation level when preceeding a statement
                    // Might need look ahead to see if next token is Comment or Indent
                    // which means you didn't actually dedent.
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
                    if let Some(exprwrapper) = self.handle_keywords(keyword) {
                        if let &mut Expr::BlockExpr(ref mut vec) = self.ast_root.get_expr() {
                            vec.push(exprwrapper)
                        }
                    }
                },
                Punctuation(punc) => {
                    panic!("Unimplemented top level token 'Punctuation'");
                },
                Comment(text) => {
                    // ToDo: Docstring, else ignore
                },
                Error(err) => {
                    self.write_error(&err);
                },
                Type(type_) => {
                    panic!("Unimplemented top level token 'Type'");
                },
                EOF => break
            };
        }

        // Do semantic analysis on AST


        // Run codegen when no errors have been found
        if self.run_codegen {
            self.ast_root.gen_code();
        }
    }
}

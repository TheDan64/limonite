#![allow(dead_code)]

use syntax::lexer::Tokenizer;
use syntax::core::tokens::Token;
use syntax::core::tokens::Token::*;
use syntax::core::keywords::Keywords;
use syntax::core::symbols::Symbols;
use syntax::ast::expr::*;
use syntax::ast::consts::*;

pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    indentation: usize,
    run_codegen: bool,
    ast_root: ExprWrapper,
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            indentation: 0,
            run_codegen: true,
            ast_root: ExprWrapper::default(Box::new(Expr::NoOp)),
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
    fn check_indentation(&mut self, depth: usize) -> Option<ExprWrapper> {
        if self.indentation == depth {
            return None;
        }
        
        self.write_error("Incorrect indentation level");

        None
    }

    // Parses a variable or constant declaration
    #[allow(unused_variables)]
    #[allow(dead_code)]
    fn parse_declaration(&mut self) -> Option<ExprWrapper> {
        let token = self.next_token();
        match token {
            Identifier(name) => {
                let token1 = self.next_token();

                match token1 {
                    // Symbol(punc) => {
                    //     match punc {
                    //         symbols::Assign => {
                    //             self.parse_expression()
                    //         }
                    //         _ => Err(self.write_error("Invalid Symbol here"))
                    //     }
                    // }
                    Symbol(Symbols::Equals) => {
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

    fn expect_token(&self, token: &Token, next_token: Token) -> bool {
        println!("{:?}, {:?}, {:?}", token, *token, next_token);
        *token == next_token
    }

    fn collect_args(&mut self) -> Option<Vec<ExprWrapper>> {
        let mut args = Vec::new();
        let mut tok = self.next_token();
        let mut first_arg = true;

        loop {
            println!("Start Loop - first_arg: {}", first_arg); 
            if !first_arg {
                println!("Got token");
                tok = self.next_token();
            }

            if !first_arg && self.expect_token(&tok, Symbol(Symbols::Comma)) {
                println!("hit the comma!");
                tok = self.next_token(); 
            }

            if self.expect_token(&tok, Symbol(Symbols::ParenClose)) {
                println!("hit the close paren {:?}", tok);
                return Some(args);
            }

            let name = match tok {
                Token::Identifier(ref name) => Some(name),
                _ => {
                    self.write_error(&format!("Unsupported token {:?}.", tok));
                    None
                }
            };

            if let Some(arg) = name {
                println!("hit the ident: {}", arg);
                args.push(ExprWrapper::default(
                    Box::new(Expr::Const(Const::UTF8String(String::from_str(arg))))));
                first_arg = false;
                continue;
            }
            break;
        }
        self.write_error(&format!("Invalid syntax."));
        return None;
    }

    // Parses a print function/statement
    fn parse_print_fn(&mut self) -> Option<ExprWrapper> {
        let tok = self.next_token();
        if !self.expect_token(&tok, Symbol(Symbols::ParenOpen)) {
            return None;
        }

        println!("Before Collect");
        let name = ExprWrapper::default(
            Box::new(
                Expr::Const(
                    Const::UTF8String(
                        String::from_str("print")
                    )
                )
            )
        );
        if let Some(args) = self.collect_args() {
            return Some(ExprWrapper::default(Box::new(Expr::FnCall(name, args))));
        }
        return None;
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

    #[allow(dead_code)]
    fn parse_expression(&self) -> Option<ExprWrapper> {
        None
    }

    // Parse numbers into their correct representation
    #[allow(unused_variables)]
    #[allow(dead_code)]
    fn parse_number(&mut self, num: &str) -> Option<ExprWrapper> {
        println!("{}", num);
        let value = 0;
        // This would normally check the ending on the thing slash information
        // about the number to pick int, uint, or float
        None
    }

    // Parse the file
    #[allow(unused_variables)]
    fn parse_top_level_blocks(&mut self) -> ExprWrapper {
        self.start();

        let mut expr = Vec::new();

        // Generate AST
        loop {
            // Parse the token into the next node of the AST
            let token = self.next_token();

            println!("{:?}", token);

            match token {
                Numeric(string, type_) => {
                    panic!("Unimplemented top level token 'Numeric'");
                },
                Identifier(repr) => {
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
                        expr.push(exprwrapper);
                    }
                },
                Symbol(punc) => {
                    panic!("Unimplemented top level token 'Symbol'");
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
        ExprWrapper::new(Box::new(Expr::Block(expr)), 0, 0, 0, 0)
    }

    pub fn parse(&mut self) {
        self.ast_root = self.parse_top_level_blocks();
    }

    pub fn get_ast(&self) -> &ExprWrapper {
        &self.ast_root
    }

    fn run_codegen(&self) {
        self.ast_root.gen_code();
    }
}

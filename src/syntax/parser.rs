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
    indent_level: usize,
    run_codegen: bool,
    ast_root: ExprWrapper,
    preview_token: Option<Token>
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            indent_level: 0,
            run_codegen: true,
            ast_root: ExprWrapper::default(Box::new(Expr::NoOp)),
            preview_token: None
        }
    }

    // Consume the next token from the lexer
    fn next_token(&mut self) -> Token {
        match self.preview_token.take() {
            Some(tok) => tok,
            None => self.lexer.get_tok()
        }
    }

    // Preview the next token without consuming it
    fn update_preview_token(&mut self) {
        self.preview_token = Some(self.next_token());
    }

    // Create an error from the current lexer's
    // state, and a message
    fn write_error(&mut self, msg: &str) {
        let (start_line, start_column, _, _) = self.lexer.get_error_pos();
        
        self.run_codegen = false;

        println!("Error on line: {} col: {}, {}", start_line, start_column, msg);

        // Skip to the end of the line (at Indent token) and allow parsing to continue
        loop {
            self.update_preview_token();

            // Note: This consumes the Indent token, which probably isnt ideal.
            match self.next_token() {
                Indent(depth) => {
                    self.check_indentation(depth);
                    break;
                },
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
    fn check_indentation(&mut self, depth: usize) -> isize {
        let difference = depth as isize - self.indent_level as isize;
        println!("{:?}", difference);
        if difference <= 0 {
            self.indent_level = depth;
        } else {
            self.write_error("Increased indentation level in a non standard way.");
        }

        return difference;
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

    // Parse function definitions: fn ident(args) -> type
    #[allow(unused_variables)]
    fn parse_fn(&mut self) -> Option<ExprWrapper> {
        // Get the function name
        let fn_name = match self.next_token() {
            Identifier(string) => string,
            _ => {
                self.expect_error("", "an identifier", "something else");

                return None;
            }
        };

        // Get a left paren (
        let mut tok = self.next_token();

        if !self.expect_token(&tok, Symbol(Symbols::ParenOpen)) {
            self.expect_error("", "an opening paren '('", "something else");

            return None;
        }

        // Get all args (ie a: u64)
        // ToDo: optional args (ie a = "foo": str)
        let mut args = Vec::new();

        tok = self.next_token();

        if tok != Symbol(Symbols::ParenClose) {
            loop {
                // Find sequence: ((Identifier : (Type | Identifier))(, (Identifier : (Type | Identifier)))*)?
                let arg_name = match tok {
                    Identifier(ident) => ident,
                    _ => {
                        self.expect_error("", "a function name", "something else");

                        return None;
                    }
                };

                tok = self.next_token();

                if !self.expect_token(&tok, Symbol(Symbols::Colon)) {
                    self.expect_error("", "a colon ':'", "something else");

                    return None;
                }

                match self.next_token() {
                    Type(t) => args.push((arg_name, Type(t))),
                    Identifier(ident) => args.push((arg_name, Identifier(ident))),
                    _ => {
                        self.expect_error("", "a return type", "something else");

                        return None;
                    }
                };

                match self.next_token() {
                    // Hit a closing paren, no more args
                    Symbol(Symbols::ParenClose) => break,

                    // Hit a comma, expecting more args
                    Symbol(Symbols::Comma) => (),

                    // Found something else, error
                    _ => {
                        self.expect_error("", "a closing paren ')' or comma ','", "something else");

                        return None;
                    }
                };

                tok = self.next_token();
            }
        }

        // Get right arrow ->
        tok = self.next_token();

        if !self.expect_token(&tok, Symbol(Symbols::RightThinArrow)) {
            self.expect_error("", "a thin right arrow '->'", "something else");

            return None;
        }

        // Get a return type or identifier
        tok = self.next_token();

        let return_type = match tok {
            Type(t) => {
                Type(t)
            },
            Identifier(ident) => {
                Identifier(ident)
            },
            _ => {
                self.expect_error("", "a return type", "something else");

                return None;
            }
        };

        tok = self.next_token();

        self.indent_level += 1;

        match tok {
            Indent(depth) => self.check_indentation(depth),
            _ => {
                self.expect_error("", "a new line", "something else");

                return None;
            }
        };

        // Combine the rest of the function definiton with the fn info
        let definition = self.parse_top_level_blocks();

        let expr = Box::new(Expr::FnDecl(fn_name, args, return_type, definition));

        return Some(ExprWrapper::default(expr));
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&mut self, keyword: Keywords) -> Option<ExprWrapper> {
        match keyword {
//            Keywords::Def => {
//                self.parse_declaration()
//            },
            Keywords::Fn => self.parse_fn(),
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

                    self.update_preview_token();

                    let difference = match self.preview_token {
//                        Some(Comment(_)) => continue,
//                        Some(Indent(_)) => continue,
                        _ => self.check_indentation(depth)
                    };

                    if difference < 0 {
                        break;
                    } else {
                        continue;
                    }

                    // If indentation decreases -=1 then should break out of this loop
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

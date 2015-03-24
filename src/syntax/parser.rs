#![allow(dead_code)]
use syntax::lexer::Tokenizer;
use syntax::core::tokens::Token;
use syntax::core::tokens::Token::*;
use syntax::core::keywords::Keywords;
use syntax::core::symbols::Symbols;
use syntax::ast::expr::*;
use syntax::ast::consts::*;
use syntax::ast::op::*;
use syntax::core::types::*;

pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    indent_level: usize,
    valid_syntax: bool,
    ast_root: ExprWrapper,
    preview_token: Option<Token>
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            indent_level: 0,
            valid_syntax: true,
            ast_root: ExprWrapper::default(Expr::NoOp),
            preview_token: None
        }
    }

    fn expect_token(&self, token: &Token, next_token: Token) -> bool {
        *token == next_token
    }

    // Consume the next token from the lexer
    fn next_token(&mut self) -> Token {
        match self.preview_token.take() {
            Some(tok) => tok,
            None => self.lexer.get_tok()
        }
    }

    fn peek(&mut self) -> Token {
        let tok = self.next_token();
        self.preview_token = Some(tok.clone());
        tok
    }

    // Create an error from the current lexer's
    // state, and a message
    fn write_error(&mut self, msg: &str) {
        let (start_line, start_column, _, _) = self.lexer.get_error_pos();
        
        self.valid_syntax = false;

        println!("filename:{}:{} {}", start_line, start_column, msg);

        // Skip to the end of the line (at Indent token) and allow parsing to continue
        loop {
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

        if difference <= 0 {
            self.indent_level = depth;
        } else {
            match self.peek() {
                Indent(_) | Comment(_) => (),
                _ => self.write_error("Increased indentation level in a non standard way.")
            };
        }

        return difference;
    }

    fn collect_args(&mut self) -> Option<Vec<ExprWrapper>> {
        let mut args = Vec::new();
        let mut tok = self.next_token();
        let mut first_arg = true;

        loop {
            if !first_arg {
                tok = self.next_token();
            }

            if !first_arg && self.expect_token(&tok, Symbol(Symbols::Comma)) {
                tok = self.next_token(); 
            }

            if self.expect_token(&tok, Symbol(Symbols::ParenClose)) {
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
                args.push(ExprWrapper::default(Expr::Const(Const::UTF8String(arg.to_string()))));
                first_arg = false;
                continue;
            }
            break;
        }
        self.write_error(&format!("Invalid syntax."));
        return None;
    }

    fn collect_sequence<F, G>
        (&mut self, mut collect_arg: F, sequence_end: G) -> Vec<ExprWrapper>
        where F: FnMut(&mut Parser<TokType>, Token) -> Option<ExprWrapper>,
              G: Fn(&Parser<TokType>, &Token) -> bool {
        let mut args = Vec::new();
        loop {
            if let Some(new_arg) = collect_arg(self, Symbol(Symbols::Comma)) {
                args.push(new_arg);
            }
            let tok = self.peek();
            if sequence_end(self, &tok) {
                break;
            }

            // Skips the comma
            self.next_token();
        }
        args
    }

    fn parse_fn_call(&mut self, ident: String) -> Option<ExprWrapper> {
        let token = self.next_token();
        if !self.expect_token(&token, Symbol(Symbols::ParenOpen)) {
            self.write_error("Expected an open parenthesis here.");
        }

        let parse_args = |this: &mut Parser<TokType>, seperator: Token| {
            this.parse_expression(0)
        };

        let sequence_end = |this: &Parser<TokType>, current_token: &Token| {
            this.expect_token(current_token, Symbol(Symbols::ParenClose))
        };

        let ident = ExprWrapper::default(Expr::Ident(ident.to_string()));
        let args = self.collect_sequence(parse_args, sequence_end);
        self.next_token();

        Some(ExprWrapper::default(Expr::FnCall(ident, args)))
    }

    fn parse_idents(&mut self, ident: String) -> Option<ExprWrapper> {
        let tok = self.peek();
        match tok {
            Symbol(Symbols::ParenOpen) => {
                self.parse_fn_call(ident)
            }
            _ => {
                None
            }
        }
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
        let definition = self.parse();

        let expr = Expr::FnDecl(fn_name, args, return_type, definition);

        return Some(ExprWrapper::default(expr));
    }

    // Handles top-level keywords to start parsing them
    fn handle_keywords(&mut self, keyword: Keywords) -> Option<ExprWrapper> {
        match keyword {
            Keywords::Function => self.parse_fn(),
            Keywords::If => self.parse_if(),
            _ => {
                self.write_error(&format!("Unsupported keyword {:?}.", keyword));
                None
            }
        }
    }

    fn parse_if(&mut self) -> Option<ExprWrapper> {
        let condition = match self.parse_expression(0) {
            Some(exprwrapper) => exprwrapper,
            None => return None
        };

        let tok = self.next_token();

        if !self.expect_token(&tok, Symbol(Symbols::Comma)) {
            self.expect_error("", "a comma ','", "something else");

            return None;
        }

        self.indent_level += 1;

        match self.next_token() {
            Indent(depth) => self.check_indentation(depth),
            _ => {
                self.expect_error("", "a new line", "something else");

                return None;
            }
        };

        let block = self.parse();

        let expr = Expr::If(condition, block, None);

        Some(ExprWrapper::default(expr))
    }

    fn is_infix_op(&self, token: &Token) -> bool {
        match *token {
            Symbol(Symbols::Plus) => true,
            Symbol(Symbols::Minus) => true,
            Symbol(Symbols::Asterisk) => true,
            Symbol(Symbols::Slash) => true,
            Symbol(Symbols::Percent) => true,
            Symbol(Symbols::Caret) => true,
            Keyword(Keywords::Equals) => true,
            _ => false
        }
    }

    fn get_precedence(&self, token: &Token) -> u8 {
        match *token {
            Symbol(Symbols::Plus) => InfixOp::Add.get_precedence(),
            Symbol(Symbols::Minus) => InfixOp::Sub.get_precedence(),
            Symbol(Symbols::Asterisk) => InfixOp::Mul.get_precedence(),
            Symbol(Symbols::Slash) => InfixOp::Div.get_precedence(),
            Symbol(Symbols::Percent) => InfixOp::Mod.get_precedence(),
            Symbol(Symbols::Caret) => InfixOp::Pow.get_precedence(),
            Keyword(Keywords::Equals) => InfixOp::Equ.get_precedence(),
            _ => 0
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ExprWrapper> {
        // E -> (E) | [E] | E * E | E + E | E - E | E / E | E % E | E ^ E |
        // E equals E | E and E | E or E | not E | -E | Terminal
        // Terminal -> identifier | const

        let subroutine = self.parse_expression_subroutine();
        if subroutine == None {
            return None
        }
        let mut lhs = subroutine.unwrap();

        let mut token = self.peek();
        while self.is_infix_op(&token) && self.get_precedence(&token) >= precedence {
            token = self.next_token();
            let new_precedence = self.get_precedence(&token) + match token {
                // Right associative ops don't get the +1
                Symbol(Symbols::Caret) => 0,
                _ => 1,
            };

            if let Some(rhs) = self.parse_expression(new_precedence) {
                let infix = match token {
                    Symbol(Symbols::Plus) => InfixOp::Add,
                    Symbol(Symbols::Minus) => InfixOp::Sub,
                    Symbol(Symbols::Asterisk) => InfixOp::Mul,
                    Symbol(Symbols::Slash) => InfixOp::Div,
                    Symbol(Symbols::Percent) => InfixOp::Mod,
                    Symbol(Symbols::Caret) => InfixOp::Pow,
                    Keyword(Keywords::Equals) => InfixOp::Equ,
                    _ => panic!("This shouldn't happen!")
                };

                lhs = ExprWrapper::default(Expr::InfixOp(infix, lhs, rhs));
            } else {
                return None;
            }
            token = self.peek();
        }
        return Some(lhs);
    }

    fn parse_expression_subroutine(&mut self) -> Option<ExprWrapper> {
        match self.next_token() {
            // Terminals
            StrLiteral(string) => {
                Some(ExprWrapper::default(Expr::Const(Const::UTF8String(string))))
            },
            CharLiteral(chr) => {
                Some(ExprWrapper::default(Expr::Const(Const::UTF8Char(chr))))
            },
            Identifier(ident) => {
                if let Symbol(Symbols::ParenOpen) = self.peek() {
                    return self.parse_fn_call(ident);
                }

                Some(ExprWrapper::default(Expr::Ident(ident)))
            },
            Numeric(string, _type) => Some(self.parse_number(string, _type)),

            // Parens
            Symbol(Symbols::ParenOpen) => {
                let exprwrapper = self.parse_expression(0);

                let tok = self.next_token();

                if !self.expect_token(&tok, Symbol(Symbols::ParenClose)) {
                    self.expect_error("", "a closing paren ')'", "something else");

                    return None;
                }

                exprwrapper
            },

            // Unary ops, precedence hard coded to a (high) 8
            Symbol(Symbols::Minus) => {
                return match self.parse_expression(8) {
                    Some(exprwrapper) => Some(ExprWrapper::default(Expr::UnaryOp(UnaryOp::Negate, exprwrapper))),
                    None => None
                }
            },
            Keyword(Keywords::Not) => {
                return match self.parse_expression(8) {
                    Some(exprwrapper) => Some(ExprWrapper::default(Expr::UnaryOp(UnaryOp::Not, exprwrapper))),
                    None => None
                }
            },

            // Else error
            _ => {
                self.write_error("Not sure how you got here.");

                None
            }
        }
    }

    // Parse numbers into their correct representation
    #[allow(unused_variables)]
    #[allow(dead_code)]
    fn parse_number(&mut self, num: String, type_: Option<Types>) -> ExprWrapper {
        let has_decimal_point = num.contains('.');

        let base:u32 = match &num[..2] {
            "0x" => 16,
            "0b" => 2,
            _    => 10
        };

        let bytes = num.into_bytes().into_iter().filter(|byte| match *byte as char {
            '_' | 'x' | 'b' => false,
            _ => true
        });

        let mut int32 = 0i32;
        let mut int64 = 0i64;
        let mut uint32 = 0u32;
        let mut uint64 = 0u64;
        let mut float32 = 0f32;
        let mut float64 = 0f64;

        let mut before_decimal_point = true;
        let mut decimal_iteration = 1f32;

        // Does not account for overflow
        for byte in bytes {
            match byte as char {
                b if b.is_digit(base) => {
                    match type_ {
                        Some(Types::Int32Bit) => {
                            int32 *= base as i32;
                            int32 += b.to_digit(base).unwrap() as i32;
                        },
                        Some(Types::Int64Bit) => {
                            int64 *= base as i64;
                            int64 += b.to_digit(base).unwrap() as i64;
                        },
                        Some(Types::UInt32Bit) => {
                            uint32 *= base;
                            uint32 += b.to_digit(base).unwrap();
                        },
                        Some(Types::UInt64Bit) => {
                            uint64 *= base as u64;
                            uint64 += b.to_digit(base).unwrap() as u64;
                        },
                        Some(Types::Float32Bit) => {
                            if before_decimal_point {
                                float32 *= base as f32;
                                float32 += b.to_digit(base).unwrap() as f32;
                            } else {
                                float32 += b.to_digit(base).unwrap() as f32 / (base as f32 * decimal_iteration);
                                decimal_iteration += 1f32;
                            }
                        },
                        Some(Types::Float64Bit) => {
                            if before_decimal_point {
                                float64 *= base as f64;
                                float64 += b.to_digit(base).unwrap() as f64;
                            } else {
                                float64 += b.to_digit(base).unwrap() as f64 / (base as f64 * decimal_iteration as f64);
                                decimal_iteration += 1f32;
                            }
                        },
                        _ => {
                            // No given type suffix. Default to i32 or f32 when a decimal point present
                            if has_decimal_point {
                                if before_decimal_point {
                                    float32 *= base as f32;
                                    float32 += b.to_digit(base).unwrap() as f32;
                                } else {
                                    float32 += b.to_digit(base).unwrap() as f32 / (base as f32 * decimal_iteration);
                                    decimal_iteration += 1f32;
                                }
                            } else {
                                int32 *= base as i32;
                                int32 += b.to_digit(base).unwrap() as i32;
                            }

                        }
                    }
                },
                '.' => before_decimal_point = false,
                _ => panic!("Numeric parse failure: This shouldn't happen!")
            }
        }

        ExprWrapper::default(Expr::Const(match type_ {
            Some(Types::Int32Bit)   => Const::I32Num(int32),
            Some(Types::Int64Bit)   => Const::I64Num(int64),
            Some(Types::UInt32Bit)  => Const::U32Num(uint32),
            Some(Types::UInt64Bit)  => Const::U64Num(uint64),
            Some(Types::Float32Bit) => Const::F32Num(float32),
            Some(Types::Float64Bit) => Const::F64Num(float64),
            _ => {
                // No given type suffix. Default to i32 or f32 when a decimal point present
                if has_decimal_point {
                    Const::F32Num(float32)
                } else {
                    Const::I32Num(int32)
                }
            }
        }))
    }

    // Parse the file
    #[allow(unused_variables)]
    pub fn parse(&mut self) -> ExprWrapper {
        self.start();

        let mut expr = Vec::new();

        // Generate AST
        loop {
            // Parse the token into the next node of the AST
            let token = self.next_token();

            match token {
                Numeric(string, type_) => {
                    panic!("Unimplemented top level token 'Numeric'");
                },
                Identifier(ident) => {
                    if let Some(exprwrapper) = self.parse_idents(ident) {
                        expr.push(exprwrapper);
                    }
                },
                Indent(depth) => {
                    // Look ahead. If you find another Indent token it's a blank line
                    // and didn't actually dedent. Same is true for single line comments
                    // however this isn't guarenteed for multi line comments (because
                    // you could have code after the end of the comment).
                    // ToDo: Account to multiline comments
                    let difference = match self.peek() {
                        Comment(_) => continue,
                        Indent(_) => continue,
                        _ => self.check_indentation(depth)
                    };

                    // Break if you dedent (cannot happen at top level) else continue
                    if difference < 0 {
                        break;
                    }
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
                Type(type_) => {
                    panic!("Unimplemented top level token 'Type'");
                },
                Error(err) => {
                    self.write_error(&err);
                },
                EOF => break,
            };
        }

        ExprWrapper::new(Expr::Block(expr), 0, 0, 0, 0)
    }

    pub fn generated_valid_syntax(&self) -> bool {
        self.valid_syntax
    }
}

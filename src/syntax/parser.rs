#![allow(dead_code)]
use syntax::lexer::Tokenizer;
use syntax::core::tokens::Tokens;
use syntax::core::tokens::Tokens::*;
use syntax::core::keywords::Keywords;
use syntax::core::symbols::Symbols;
use syntax::ast::expr::*;
use syntax::ast::literals::*;
use syntax::ast::op::*;
use syntax::core::types::*;

pub struct Parser<TokType: Tokenizer> {
    lexer: TokType,
    ast_root: ExprWrapper,
    indent_level: u64,
    preview_token: Option<Tokens>,
    valid_ast: bool,
    block_status: BlockStatus,
}

enum BlockStatus {
    Out,
    Starting,
    In,
}

impl<TokType: Tokenizer> Parser<TokType> {
    pub fn new(tokenizer: TokType) -> Parser<TokType> {
        Parser {
            lexer: tokenizer,
            ast_root: ExprWrapper::default(Expr::NoOp),
            indent_level: 0,
            valid_ast: true,
            preview_token: None,
            block_status: BlockStatus::Out,
        }
    }

    /// Consume the next `Token` from the lexer
    /// - Ignores `Comment`s entirely
    /// - Smartly handlers `Indent`s by:
    ///    - When in blocks ignores them
    ///    - Ensures correct indentation size, then gets the next token
    fn next_token(&mut self) -> Tokens {
        loop {
            let result = match self.preview_token.take() {
                Some(tok) => tok,
                None => self.lexer.get_tok(),
            };
            match result {
                Indent(depth) => {
                    match self.block_status {
                        BlockStatus::Out => {
                            // If the new depth is smaller than the old depth, we've dedented
                            if depth - self.indent_level <= 0 {
                                self.indent_level = depth;
                            }
                            return result;
                        },
                        BlockStatus::Starting => {
                            if self.indent_level == depth {
                                self.block_status = BlockStatus::In;
                            } else {
                                return self.write_error("Invalid level of indentation");
                            }
                        },
                        BlockStatus::In => {
                            self.block_status = BlockStatus::Out;
                        },
                    }
                },
                Comment(_) => (),
                _ => return result,
            }
        }
    }

    /// Returns a peek at the next `Token` without consuming it
    /// The next call to `next_token` will return the same `Token` returned
    /// by the last call to `peek`
    fn peek(&mut self) -> Tokens {
        let tok = self.next_token();
        self.preview_token = Some(tok.clone());
        tok
    }

    /// Create an error from the current `Lexer`s state, with a message
    fn write_error(&mut self, msg: &str) -> Tokens {
        let (start_line, start_column, _, _) = self.lexer.get_error_pos();
        
        self.valid_ast = false;

        Tokens::Error(format!("filename:{}:{} {}", start_line, start_column, msg))
    }

    fn write_expect_error(&mut self, reason: &str, expect: &str, got: &str) {
        self.write_error(&format!("{}. Expected {}, but got {}", reason, expect, got));
    }

    /// Perform any necessary on-start actions
    fn start(&self) {
    }

    fn incr_indentation(&mut self) {
        self.block_status = BlockStatus::Starting;
        self.indent_level += 1;
    }

    fn collect_args(&mut self) -> Option<Vec<ExprWrapper>> {
        let mut args = Vec::new();
        let mut tok = self.next_token();
        let mut first_arg = true;

        loop {
            if !first_arg {
                tok = self.next_token();
            }

            if !first_arg && tok.expect(Symbol(Symbols::Comma)) {
                tok = self.next_token(); 
            }

            if tok.expect(Symbol(Symbols::ParenClose)) {
                return Some(args);
            }

            let name = match tok {
                Tokens::Identifier(ref name) => Some(name),
                _ => {
                    self.write_error(&format!("Unsupported token {:?}.", tok));
                    None
                }
            };

            if let Some(arg) = name {
                args.push(ExprWrapper::default(Expr::Literal(Literal::UTF8String(arg.to_string()))));
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
        where F: FnMut(&mut Parser<TokType>, Tokens) -> Option<ExprWrapper>,
              G: Fn(&Parser<TokType>, Tokens) -> bool {
        let mut args = Vec::new();
        loop {
            if let Some(new_arg) = collect_arg(self, Symbol(Symbols::Comma)) {
                args.push(new_arg);
            }
            let tok = self.peek();
            if sequence_end(self, tok) {
                break;
            }

            // Skips the comma
            self.next_token();
        }
        args
    }

    fn parse_fn_call(&mut self, ident: String) -> Option<ExprWrapper> {
        let token = self.next_token();
        if !token.expect(Symbol(Symbols::ParenOpen)) {
            self.write_error("Expected an open parenthesis here.");
            return None;
        }

        let tok = self.peek();

        // Check to see if there are no args
        if tok.expect(Symbol(Symbols::ParenClose)) {
            self.next_token();
            return Some(ExprWrapper::default(Expr::FnCall(ident.to_string(), Vec::new())));
        }

        let parse_args = |this: &mut Parser<TokType>, seperator: Tokens| {
            if !seperator.expect(Symbol(Symbols::Comma)) {
                this.write_error("Missing a comma between arguments.");
            }
            this.parse_expression(0)
        };

        let sequence_end = |this: &Parser<TokType>, current_token: Tokens| {
            current_token.expect(Symbol(Symbols::ParenClose))
        };

        let args = self.collect_sequence(parse_args, sequence_end);
        self.next_token();

        Some(ExprWrapper::default(Expr::FnCall(ident.to_string(), args)))
    }

    fn parse_assignment(&mut self, ident: String) -> Option<ExprWrapper> {
        // Clear the equals sign
        self.next_token();

        if let Some(rvalue) = self.parse_expression(0) {
            let ident = ExprWrapper::default(Expr::Ident(ident));
            return Some(ExprWrapper::default(Expr::Assign(ident, rvalue)));
        } else {
            self.write_expect_error("", "An expression", "None");
        }
        None
    }

    fn parse_idents(&mut self, ident: String) -> Option<ExprWrapper> {
        self.next_token();

        let tok = self.peek();
        match tok {
            Symbol(Symbols::ParenOpen) => self.parse_fn_call(ident),
            Symbol(Symbols::Equals) => self.parse_assignment(ident),
            _ => None,
        }
    }

    // Parse function definitions: fn ident(args) -> type
    #[allow(unused_variables)]
    fn parse_fn(&mut self) -> Option<ExprWrapper> {
        self.next_token();

        // Get the function name
        let fn_name = match self.next_token() {
            Identifier(string) => string,
            _ => {
                self.write_expect_error("", "an identifier", "something else");

                return None;
            }
        };

        // Get a left paren (
        let mut tok = self.next_token();

        if !tok.expect(Symbol(Symbols::ParenOpen)) {
            self.write_expect_error("", "an opening paren '('", "something else");

            return None;
        }

        // Get all args (ie a: u64)
        // ToDo: optional args (ie a = "foo": str)
        let mut args = Vec::new();

        tok = self.next_token();

        if tok != Symbol(Symbols::ParenClose) {
            loop {
                // Find sequence: ((Identifier : Identifier)(, (Identifier : Identifier))*)?
                let arg_name = match tok {
                    Identifier(ident) => ident,
                    _ => {
                        self.write_expect_error("", "a function name", "something else");

                        return None;
                    }
                };

                tok = self.next_token();

                if !tok.expect(Symbol(Symbols::Colon)) {
                    self.write_expect_error("", "a colon ':'", "something else");

                    return None;
                }

                match self.next_token() {
                    Identifier(ident) => args.push((arg_name, Identifier(ident))),
                    _ => {
                        self.write_expect_error("", "a return type", "something else");

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
                        self.write_expect_error("", "a closing paren ')' or comma ','", "something else");

                        return None;
                    }
                };

                tok = self.next_token();
            }
        }

        // Get right arrow ->
        tok = self.next_token();

        if tok.expect(Symbol(Symbols::RightThinArrow)) {
            self.write_expect_error("", "a thin right arrow '->'", "something else");

            return None;
        }

        // Get a return type or identifier
        tok = self.next_token();

        let return_type = match tok {
            Identifier(ident) => {
                Identifier(ident)
            },
            _ => {
                self.write_expect_error("", "a return type", "something else");

                return None;
            }
        };

        self.incr_indentation();

        // Combine the rest of the function definiton with the fn info
        let definition = self.sub_parse();

        let expr = Expr::FnDecl(fn_name, args, return_type, definition);

        return Some(ExprWrapper::default(expr));
    }

    fn parse_declaration(&mut self) -> Option<ExprWrapper> {
        let keyword = self.next_token();
        let def_decl = keyword.expect(Keyword(Keywords::Def));

        let token = self.next_token();
        if let Identifier(ident) = token {
            let token = self.next_token();
            if !token.expect(Symbol(Symbols::Colon)) {
                self.write_expect_error("", "a colon", &format!("{:?}", token));
                return None
            }

            let token = self.next_token();
            if let Identifier(typ) = token {
                let token = self.next_token();
                if !token.expect(Symbol(Symbols::Equals)) {
                    self.write_expect_error("", "an Equal", &format!("{:?}", token));
                    return None
                }

                let expr = self.parse_expression(0);
                if let Some(value) = expr {
                    return Some(ExprWrapper::default(
                        Expr::VarDecl(def_decl, ident, typ, value)));
                } else {
                    self.write_expect_error("No value", "an expression",
                                            &format!("{:?}", token));
                }
            } else {
                self.write_expect_error("No identifier", "an identifier",
                                        &format!("{:?}", token));
            }
        } else {
            self.write_expect_error("No identifier", "an identifier",
                                    &format!("{:?}", token));
        }
        None
    }

    /// Parse a while block
    fn parse_while(&mut self) -> Option<ExprWrapper> {
        self.next_token();
        if let Some(expr) = self.parse_expression(0) {
            let token = self.next_token();
            if !token.expect(Symbol(Symbols::Comma)) {
                self.write_expect_error("Incomplete while expression",
                                        &format!("{:?}", Symbols::Comma),
                                        &format!("{:?}", token));
                return None
            }
            self.incr_indentation();

            let block = self.sub_parse();
            let result = Expr::WhileLoop(expr, block);
            return Some(ExprWrapper::default(result));
        } else {
            self.write_expect_error("While should have an expression",
                                    &format!("{:?}", "An expression"),
                                    &format!("{:?}", "Nothing"));
            return None;
        }
    }

    /// Handles top-level keywords to start parsing them
    fn parse_keywords(&mut self, keyword: Keywords) -> Option<ExprWrapper> {
        match keyword {
            Keywords::Var | Keywords::Def => self.parse_declaration(),
            Keywords::Function => self.parse_fn(),
            Keywords::While => self.parse_while(),
            Keywords::If => self.parse_if(),
            _ => {
                self.write_error(&format!("Unsupported keyword {:?}.", keyword));
                None
            }
        }
    }

    fn parse_if(&mut self) -> Option<ExprWrapper> {
        self.next_token();

        let condition = match self.parse_expression(0) {
            Some(exprwrapper) => exprwrapper,
            None => return None
        };

        let tok = self.next_token();

        if !tok.expect(Symbol(Symbols::Comma)) {
            self.write_expect_error("", "a comma ','", "something else");

            return None;
        }

        self.incr_indentation();

        let block = self.sub_parse();

        let expr = Expr::If(condition, block, None);

        Some(ExprWrapper::default(expr))
    }

    fn is_infix_op(&self, token: &Tokens) -> bool {
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

    fn get_precedence(&self, token: &Tokens) -> u8 {
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
        // Terminal -> identifier | literal

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
        // TODO: Add support for Bools

        match self.next_token() {
            // Terminals
            StrLiteral(string) => {
                Some(ExprWrapper::default(Expr::Literal(Literal::UTF8String(string))))
            },
            CharLiteral(chr) => {
                Some(ExprWrapper::default(Expr::Literal(Literal::UTF8Char(chr))))
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

                if !tok.expect(Symbol(Symbols::ParenClose)) {
                    self.write_expect_error("", "a closing paren ')'", "something else");

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

        let chars = num.chars().filter(|chr| match *chr {
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
        for chr in chars {
            match chr {
                c if c.is_digit(base) => {
                    match type_ {
                        Some(Types::Int32Bit) => {
                            int32 *= base as i32;
                            int32 += c.to_digit(base).unwrap() as i32;
                        },
                        Some(Types::Int64Bit) => {
                            int64 *= base as i64;
                            int64 += c.to_digit(base).unwrap() as i64;
                        },
                        Some(Types::UInt32Bit) => {
                            uint32 *= base;
                            uint32 += c.to_digit(base).unwrap();
                        },
                        Some(Types::UInt64Bit) => {
                            uint64 *= base as u64;
                            uint64 += c.to_digit(base).unwrap() as u64;
                        },
                        Some(Types::Float32Bit) => {
                            if before_decimal_point {
                                float32 *= base as f32;
                                float32 += c.to_digit(base).unwrap() as f32;
                            } else {
                                float32 += c.to_digit(base).unwrap() as f32 / (base as f32 * decimal_iteration);
                                decimal_iteration += 1f32;
                            }
                        },
                        Some(Types::Float64Bit) => {
                            if before_decimal_point {
                                float64 *= base as f64;
                                float64 += c.to_digit(base).unwrap() as f64;
                            } else {
                                float64 += c.to_digit(base).unwrap() as f64 / (base as f64 * decimal_iteration as f64);
                                decimal_iteration += 1f32;
                            }
                        },
                        _ => {
                            // No given type suffix. Default to i32 or f32 when a decimal point present
                            if has_decimal_point {
                                if before_decimal_point {
                                    float32 *= base as f32;
                                    float32 += c.to_digit(base).unwrap() as f32;
                                } else {
                                    float32 += c.to_digit(base).unwrap() as f32 / (base as f32 * decimal_iteration);
                                    decimal_iteration += 1f32;
                                }
                            } else {
                                int32 *= base as i32;
                                int32 += c.to_digit(base).unwrap() as i32;
                            }

                        }
                    }
                },
                '.' => before_decimal_point = false,
                _ => panic!("Numeric parse failure: Invalid characters found!")
            }
        }

        ExprWrapper::default(Expr::Literal(match type_ {
            Some(Types::Int32Bit)   => Literal::I32Num(int32),
            Some(Types::Int64Bit)   => Literal::I64Num(int64),
            Some(Types::UInt32Bit)  => Literal::U32Num(uint32),
            Some(Types::UInt64Bit)  => Literal::U64Num(uint64),
            Some(Types::Float32Bit) => Literal::F32Num(float32),
            Some(Types::Float64Bit) => Literal::F64Num(float64),
            _ => {
                // No given type suffix. Default to i32 or f32 when a decimal point present
                if has_decimal_point {
                    Literal::F32Num(float32)
                } else {
                    Literal::I32Num(int32)
                }
            }
        }))
    }

    /// Returns an `ExprWrapper` to the root of the current AST branch
    fn sub_parse(&mut self) -> ExprWrapper {
        let mut expr = Vec::new();
        loop {
            match self.peek() {
                Identifier(ident) => {
                    if let Some(exprwrapper) = self.parse_idents(ident) {
                        expr.push(exprwrapper);
                    }
                },
                Keyword(keyword) => {
                    if let Some(exprwrapper) = self.parse_keywords(keyword) {
                        expr.push(exprwrapper);
                    }
                },
                Error(err) => {
                    self.write_error(&err);
                    break
                },
                EOF => break,

                // These tokens are all illegal in top level expressions
                Symbol(_) | StrLiteral(_) | CharLiteral(_) | BoolLiteral(_) |
                Numeric(_, _) | Comment(_) | Indent(_) => {
                    panic!("Unimplemented top level token '{:?}'", self.peek());
                },
            };
        }

        ExprWrapper::new(Expr::Block(expr), 0, 0, 0, 0)
    }
    
    pub fn parse(&mut self) -> Option<ExprWrapper>{
        let ast_root = self.sub_parse();

        if self.valid_ast {
            Some(ast_root)
        } else {
            None
        }
    }
}

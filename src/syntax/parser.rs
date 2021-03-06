use crate::interner::StrId;
use crate::lexical::{Keyword::*, LexerError, Symbol::{self, *}, Token, TokenKind, TokenResult};
use crate::span::{Span, Spanned};
use crate::syntax::{Block, Expr, ExprKind, InfixOp, Item, ItemKind, Local, Literal::*, Stmt, StmtKind, Type, TypeKind, UnaryOp};
use crate::syntax::items::FnSig;

use std::convert::TryFrom;
use std::iter::{Iterator, Peekable};

pub struct Parser<'s, I: Iterator> {
    errors: Vec<ParserError<'s>>,
    token_stream: Peekable<I>,
    indent: u32,
}

impl<'s, I: Iterator<Item=TokenResult<'s>>> Parser<'s, I> {
    pub fn new(token_stream: I) -> Self {
        Parser {
            errors: Vec::new(),
            token_stream: token_stream.peekable(),
            indent: 0,
        }
    }

    pub fn run(mut self) -> Result<Block<'s>, Vec<ParserError<'s>>> {
        let ast_root = self.parse_block();

        if self.errors.is_empty() {
            Ok(ast_root)
        } else {
            Err(self.errors)
        }
    }

    fn opt_next_token(&mut self) -> Result<Option<Token<'s>>, ParserError<'s>> {
        match self.token_stream.peek() {
            Some(Ok(t)) => Ok(Some(*t)),
            Some(Err(e)) => (*e).into(),
            None => Ok(None),
        }
    }

    fn next_token(&mut self) -> Result<Token<'s>, ParserError<'s>> {
        match self.opt_next_token()? {
            Some(t) => Ok(t),
            None => unimplemented!("eof error"),
        }
    }

    fn opt_consume_token(&mut self) -> Result<Option<Token<'s>>, ParserError<'s>> {
        match self.token_stream.next() {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(e)) => e.into(),
            None => Ok(None),
        }
    }

    fn skip_til_indent(&mut self) {
        while !matches!(self.opt_next_token().map(|i| i.map(|i| i.node())), Ok(Some(TokenKind::Indent(_)))) {
            self.consume_token().unwrap();
        }
    }

    fn consume_token(&mut self) -> Result<Token<'s>, ParserError<'s>> {
        match self.opt_consume_token()? {
            Some(t) => Ok(t),
            None => unimplemented!("eof error"),
        }
    }

    fn parse_fn_call(&mut self, ident: Spanned<&'s str>) -> Result<Expr<'s>, ParserError<'s>> {
        // Open Paren
        self.consume_token().unwrap();

        let token = self.next_token()?;

        // call()
        if token.node() == TokenKind::Symbol(ParenClose) {
            self.consume_token().unwrap();

            let span = Span::new(ident, ident, token);

            return Ok(Spanned::boxed(ExprKind::FnCall(ident, Vec::new()), span));
        }

        let exprs = self.parse_deliminated(|p| p.parse_expr(0), Comma, true)?;
        let end_token = self.next_token()?;

        // Can be close token
        if end_token.node() != TokenKind::Symbol(ParenClose) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(end_token),
            });
        }

        self.consume_token().unwrap();

        let span = Span::new(token, ident, end_token);

        Ok(Spanned::boxed(ExprKind::FnCall(ident, exprs), span))
    }

    fn parse_numeric(&mut self, num: Spanned<&'s str>, opt_suffix: Option<Spanned<&'s str>>) -> Result<Expr<'s>, ParserError<'s>> {
        macro_rules! parse_int {
            ($ty:ty) => {{
                let num = num.node();

                if num.starts_with("0b") {
                    <$ty>::from_str_radix(&num[2..], 2).expect("fixme")
                } else if num.starts_with("0x") {
                    <$ty>::from_str_radix(&num[2..], 16).expect("fixme")
                } else {
                    <$ty>::from_str_radix(&num, 10).expect("fixme")
                }
            }}
        }

        let num_lit = match opt_suffix.map(|sp| sp.node()) {
            Some("i8") => I8Num(parse_int!(i8)),
            Some("i16") => I16Num(parse_int!(i16)),
            Some("i32") => I32Num(parse_int!(i32)),
            Some("i64") => I64Num(parse_int!(i64)),
            Some("i128") => I128Num(parse_int!(i128)),
            Some("u8") => U8Num(parse_int!(u8)),
            Some("u16") => U16Num(parse_int!(u16)),
            Some("u32") => U32Num(parse_int!(u32)),
            Some("u64") => U64Num(parse_int!(u64)),
            Some("u128") => U128Num(parse_int!(u128)),
            Some("f32") => F32Num(num.node().parse().expect("fixme")),
            Some("f64") => F64Num(num.node().parse().expect("fixme")),
            Some(_unknown) => todo!("parser error"),
            None => {
                if num.node().contains('.') {
                    F32Num(num.node().parse().expect("fixme"))
                } else {
                    I32Num(parse_int!(i32))
                }
            },
        };

        let end_idx = opt_suffix.map(|s| s.end_idx()).unwrap_or(num.end_idx());
        let span = Span::new(num, num, end_idx);

        Ok(Spanned::boxed(ExprKind::Literal(num_lit), span))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr<'s>, ParserError<'s>> {
        // E -> (E) | [E] | E * E | E + E | E - E | E / E | E % E | E ^ E |
        // E equals E | E and E | E or E | not E | -E | Terminal
        // Terminal -> identifier | literal

        let lhs_tok = self.next_token()?;
        let mut lhs = match lhs_tok.node() {
            TokenKind::Symbol(ParenOpen) => {
                self.consume_token().unwrap();

                let lhs = self.parse_expr(0)?;
                let sp_tok = self.next_token()?;

                if sp_tok.node() != TokenKind::Symbol(ParenClose) {
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken(sp_tok)
                    });
                }

                self.consume_token().unwrap();

                lhs
            },
            // Symbol(SBracketOpen) => {
            //     let lhs = self.parse_expr(0);

            //     // FIXME: Check if SBracketClose
            //     self.consume_token();

            //     lhs
            // },
            // TODO: or Keyword::Not
            TokenKind::Symbol(Minus) => {
                self.consume_token().unwrap();

                let ((), r_bp) = UnaryOp::Negate.binding_power();
                let rhs = self.parse_expr(r_bp)?;
                let span = Span::new(lhs_tok, lhs_tok, rhs.span());

                Spanned::boxed(ExprKind::UnaryOp(lhs_tok.replace(UnaryOp::Negate), rhs), span)
            },
            TokenKind::StrLiteral(s) => {
                self.consume_token().unwrap();

                lhs_tok.replace(Box::new(ExprKind::Literal(UTF8String(s))))
            },
            TokenKind::Numeric(num, opt_suffix) => {
                self.consume_token().unwrap();
                self.parse_numeric(num, opt_suffix)?
            },
            TokenKind::Identifier(i) => {
                self.consume_token().unwrap();

                lhs_tok.replace(Box::new(ExprKind::Var(i)))
            },
            TokenKind::Symbol(_) => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken(lhs_tok),
                });
            },
            t => panic!("Unsupported expr lhs: {:?}", t),
        };

        loop {
            let tok = match self.opt_next_token()? {
                Some(tok) => tok,
                None => break,
            };

            if let TokenKind::Symbol(Symbol::ParenOpen) = tok.node() {
                let ident = match lhs_tok.node() {
                    TokenKind::Identifier(s) => lhs_tok.replace(s),
                    _ => unreachable!(), // Maybe not?
                };
                lhs = self.parse_fn_call(ident)?;

                continue;
            }

            let (op, (l_bp, r_bp)) = match InfixOp::try_from(tok.node()) {
                Ok(op) => (tok.replace(op), op.binding_power()),
                // REVIEW: Should we always break?
                Err(()) => break,
            };

            if l_bp < min_bp {
                break;
            }

            self.consume_token().unwrap();

            let rhs = self.parse_expr(r_bp)?;
            let span = Span::new(lhs_tok, lhs_tok, rhs.span());

            // We want to desugar an assignment infix op into an assignment expr.
            // Maybe ExprKind::InfixOp is a poor name since it doesn't include all infix ops?
            let kind = match op.node() {
                InfixOp::AddEq => {
                    let rhs_span = rhs.span();
                    let op_expr = ExprKind::InfixOp(op.replace(InfixOp::Add), lhs.clone(), rhs);
                    let desugared_rhs = Spanned::boxed(op_expr, rhs_span);

                    ExprKind::Assign(lhs, desugared_rhs)
                },
                InfixOp::DivEq => {
                    let rhs_span = rhs.span();
                    let op_expr = ExprKind::InfixOp(op.replace(InfixOp::Div), lhs.clone(), rhs);
                    let desugared_rhs = Spanned::boxed(op_expr, rhs_span);

                    ExprKind::Assign(lhs, desugared_rhs)
                },
                InfixOp::ModEq => {
                    let rhs_span = rhs.span();
                    let op_expr = ExprKind::InfixOp(op.replace(InfixOp::Mod), lhs.clone(), rhs);
                    let desugared_rhs = Spanned::boxed(op_expr, rhs_span);

                    ExprKind::Assign(lhs, desugared_rhs)
                },
                InfixOp::MulEq => {
                    let rhs_span = rhs.span();
                    let op_expr = ExprKind::InfixOp(op.replace(InfixOp::Mul), lhs.clone(), rhs);
                    let desugared_rhs = Spanned::boxed(op_expr, rhs_span);

                    ExprKind::Assign(lhs, desugared_rhs)
                },
                InfixOp::SubEq => {
                    let rhs_span = rhs.span();
                    let op_expr = ExprKind::InfixOp(op.replace(InfixOp::Sub), lhs.clone(), rhs);
                    let desugared_rhs = Spanned::boxed(op_expr, rhs_span);

                    ExprKind::Assign(lhs, desugared_rhs)
                },
                _ => ExprKind::InfixOp(op, lhs, rhs),
            };

            lhs = Spanned::boxed(kind, span);

            continue;
        }

        Ok(lhs)
    }

    fn handle_sub_parse<F, O>(&mut self, stmts: &mut Vec<Stmt<'s>>, f: F)
    where
        F: Fn(&mut Self) -> Result<O, ParserError<'s>>,
        O: Into<StmtKind<'s>>,
    {
        match f(self) {
            Ok(o) => stmts.push(Stmt::new(o)),
            Err(err) => {
                self.errors.push(err);
                self.skip_til_indent();
            },
        }
    }

    fn parse_block(&mut self) -> Block<'s> {
        let mut stmts = Vec::new();
        let indent_level = self.indent;

        loop {
            let next_token = match self.opt_next_token() {
                Ok(Some(t)) => t,
                Err(e) => {
                    self.consume_token().unwrap_err();
                    self.errors.push(e);
                    continue;
                },
                Ok(None) => break,
            };

            match next_token.node() {
                TokenKind::Comment(..) => { self.consume_token().unwrap(); },
                TokenKind::Indent(level) => {
                    if level == indent_level {
                        self.consume_token().unwrap();
                        continue;
                    } else if level > indent_level {
                        todo!("indent error: {} > {}", level, indent_level);
                    } else {
                        self.consume_token().unwrap();

                        let next_tok = self.opt_next_token().map(|opt_sp_tok| opt_sp_tok.map(|t| t.node()));
                        let is_indent_or_err = match next_tok {
                            Ok(Some(TokenKind::Indent(_))) | Err(_) => true,
                            // Hit EOF; end block
                            Ok(None) => break,
                            _ => false,
                        };

                        // TODO: 1) Might have to do this in a loop. 2) Should check that the last ident isnt the same level

                        // If it's not just multiple indents or an error, dedent & exit block
                        if !is_indent_or_err {
                            self.indent = level;
                            break;
                        }

                        continue;
                    }
                },
                TokenKind::Identifier(_) => self.handle_sub_parse(&mut stmts, |p| p.parse_expr(0)),
                TokenKind::Numeric(_, _) => self.handle_sub_parse(&mut stmts, |p| p.parse_expr(0)),
                TokenKind::Keyword(Var) => self.handle_sub_parse(&mut stmts, Self::parse_var_decl),
                TokenKind::Keyword(If) => self.handle_sub_parse(&mut stmts, Self::parse_if),
                TokenKind::Keyword(While) => self.handle_sub_parse(&mut stmts, Self::parse_while),
                TokenKind::Keyword(Function) => self.handle_sub_parse(&mut stmts, Self::parse_fn_def),
                TokenKind::Keyword(Return) => self.handle_sub_parse(&mut stmts, Self::parse_return),
                TokenKind::Keyword(Use) => self.handle_sub_parse(&mut stmts, Self::parse_use),
                e => unimplemented!("{:?}", e),
            }
        }

        // REVIEW: What if multi ident dedent? next_token == indent?
        self.dedent();

        Block::new(indent_level, stmts)
    }

    fn parse_use(&mut self) -> Result<Item<'s>, ParserError<'s>> {
        let use_keywd = self.consume_token().unwrap();
        let idents = self.parse_deliminated(Parser::parse_ident, Symbol::DoubleColon, false)?;
        let end_idx = if idents.is_empty() {
            use_keywd.span()
        } else {
            idents[idents.len() - 1].span()
        };

        let span = Span::new(use_keywd, use_keywd, end_idx);

        Ok(Spanned::new(ItemKind::Use(use_keywd.replace(()), idents), span))
    }

    fn parse_return(&mut self) -> Result<Expr<'s>, ParserError<'s>> {
        let return_keywd = self.consume_token().unwrap();
        let opt_expr;
        let end_idx;

        if matches!(self.next_token()?.node(), TokenKind::Indent(_)) {
            opt_expr = None;
            end_idx = return_keywd.span();
        } else {
            let expr = self.parse_expr(0)?;

            end_idx = expr.span();
            opt_expr = Some(expr);
        };

        let span = Span::new(return_keywd, return_keywd, end_idx);

        Ok(Spanned::boxed(ExprKind::Return(opt_expr), span))
    }

    fn parse_deliminated<F, T>(&mut self, f: F, sym: Symbol, _can_trail: bool) -> Result<Vec<T>, ParserError<'s>>
    where
        F: Fn(&mut Parser<'s, I>) -> Result<T, ParserError<'s>>,
    {
        let mut parsed = Vec::new();

        parsed.push(f(self)?);

        let mut next_tok = self.next_token()?;

        while next_tok.node() == TokenKind::Symbol(sym) {
            self.consume_token().unwrap();

            parsed.push(f(self)?);

            next_tok = self.next_token()?;
        }

        Ok(parsed)
    }

    fn parse_if(&mut self) -> Result<Expr<'s>, ParserError<'s>> {
        let if_keywd = self.consume_token().unwrap();
        let cond = self.parse_expr(0)?;
        let sp_comma = self.next_token()?;

        if sp_comma.node() != TokenKind::Symbol(Symbol::Comma) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_comma)
            });
        }

        self.consume_token().unwrap();
        self.indent();

        let block = self.parse_block();
        let span = Span::new(if_keywd, if_keywd, sp_comma);

        // TODO: else/elseif
        Ok(Spanned::boxed(ExprKind::If(cond, block, None), span))
    }

    fn parse_while(&mut self) -> Result<Expr<'s>, ParserError<'s>> {
        let while_keywd = self.consume_token().unwrap();
        let cond = self.parse_expr(0)?;
        let sp_comma = self.next_token()?;

        if sp_comma.node() != TokenKind::Symbol(Symbol::Comma) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_comma)
            });
        }

        self.consume_token().unwrap();
        self.indent();

        let block = self.parse_block();
        let span = Span::new(while_keywd, while_keywd, sp_comma);

        Ok(Spanned::boxed(ExprKind::WhileLoop(cond, block), span))
    }

    fn parse_ident(&mut self) -> Result<Spanned<&'s str>, ParserError<'s>> {
        let sp_ident = self.next_token()?;
        let ident = match sp_ident.node() {
            TokenKind::Identifier(ident) => sp_ident.replace(ident),
            _ => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_ident),
            }),
        };

        self.consume_token().unwrap();

        Ok(ident)
    }

    fn parse_ident_ty_pair(&mut self) -> Result<(Spanned<&'s str>, Type<'s>), ParserError<'s>> {
        let ident = self.parse_ident()?;
        let sp_colon = self.next_token()?;

        if sp_colon.node() != TokenKind::Symbol(Symbol::Colon) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_colon)
            });
        }

        let sp_ident_ty = self.next_token()?;
        let ident_ty = match sp_ident_ty.node() {
            TokenKind::Identifier(ident) => ident,
            _ => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_ident_ty),
            }),
        };

        Ok((ident, Spanned::boxed(TypeKind::Path(ident_ty), sp_ident_ty.span())))
    }

    fn parse_fn_def(&mut self) -> Result<Item<'s>, ParserError<'s>> {
        let fn_keywd = self.consume_token().unwrap();
        let sp_ident = self.next_token()?;
        let ident = match sp_ident.node() {
            TokenKind::Identifier(ident) => sp_ident.replace(ident),
            _ => return Err(ParserError {
                kind: ParserErrorKind::FnDeclNameMissing(sp_ident),
            }),
        };

        self.consume_token().unwrap();

        let sp_paren = self.next_token()?;

        if sp_paren.node() != TokenKind::Symbol(Symbol::ParenOpen) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_paren)
            });
        }

        self.consume_token().unwrap();

        let param_pairs = if self.next_token()?.node() == TokenKind::Symbol(Symbol::ParenClose) {
            Vec::new()
        } else {
            self.parse_deliminated(Parser::parse_ident_ty_pair, Comma, true)?
        };

        let sp_paren = self.next_token()?;

        if sp_paren.node() != TokenKind::Symbol(Symbol::ParenClose) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(sp_paren)
            });
        }

        // TODO: return type
        let end_idx = if true {
            sp_paren
        } else {
            todo!("return type")
        };

        let fn_sig = FnSig::new(param_pairs, None);

        self.consume_token().unwrap();
        self.indent();

        let block = self.parse_block();

        let span = Span::new(fn_keywd, fn_keywd, end_idx);

        Ok(Spanned::new(ItemKind::FnDef(ident, Spanned::new(fn_sig, span), block), span)) // FIXME: outer span should encompase block?
    }

    #[inline]
    fn indent(&mut self) {
        self.indent += 1;
    }

    #[inline]
    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    fn parse_var_decl(&mut self) -> Result<Local<'s>, ParserError<'s>> {
        let _var_kwd = self.consume_token().unwrap();
        let tok = self.next_token()?;
        let ident = match tok.node() {
            TokenKind::Identifier(i) => tok.replace(i),
            _ => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(tok),
            }),
        };

        self.consume_token().unwrap();

        let tok = self.next_token()?;

        if tok.node() == TokenKind::Symbol(Symbol::Equals) {
            self.consume_token().unwrap();
        } else {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken(tok),
            });
        }

        let init = self.parse_expr(0)?;
        // let span = Span::new(var_kwd, var_kwd, init.span());

        Ok(Local::new(false, ident, None, init))
    }
}

#[derive(Debug)]
pub enum ParserErrorKind<'s> {
    FnDeclNameMissing(Token<'s>),
    LexerError(LexerError<'s>),
    /// This is the most generic error we can produce. It is only intended
    /// to be a placeholder to be replaced by a more detailed variant.
    UnexpectedToken(Token<'s>),
}

#[derive(Debug)]
pub struct ParserError<'s> {
    pub(crate) kind: ParserErrorKind<'s>,
}

impl ParserError<'_> {
    pub fn file_id(&self) -> StrId {
        match self.kind {
            ParserErrorKind::LexerError(le) => le.file_id(),
            ParserErrorKind::FnDeclNameMissing(t)
            | ParserErrorKind::UnexpectedToken(t) => t.span().file_id,
        }
    }
}

impl<'s, T> From<LexerError<'s>> for Result<T, ParserError<'s>> {
    fn from(le: LexerError<'s>) -> Self {
        Err(ParserError {
            kind: ParserErrorKind::LexerError(le),
        })
    }
}

#[test]
fn test_comment_hello_world() {
    use crate::interner::StrId;
    use crate::lexical::Lexer;
    use crate::syntax::StmtKind;

    let s = ">> This is a comment
print(\"Hello, world!\")\n";
    let lexer = Lexer::new(s, StrId::DUMMY);
    let parser = Parser::new(lexer);
    let ast_block = parser.run().unwrap();
    let stmts = ast_block.stmts();

    assert_eq!(stmts.len(), 1);

    let stmt = &stmts[0];

    if let StmtKind::Expr(e) = stmt.kind() {
        assert_eq!(&s[e.span()], "print(\"Hello, world!\")");

        if let ExprKind::FnCall(ident, args) = &**e.get_node() {
            assert_eq!(ident.node(), "print");
            assert_eq!(&s[ident.span()], "print");
            assert_eq!(args.len(), 1);

            let arg_expr = &args[0];

            assert_eq!(&s[arg_expr.span()], "\"Hello, world!\"");
            assert!(matches!(**arg_expr.get_node(), ExprKind::Literal(UTF8String("\"Hello, world!\""))))
        } else { unreachable!(); }
    } else { unreachable!(); }
}

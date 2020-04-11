use std::iter::{Iterator, Peekable};

use crate::lexical::{LexerError, Symbol::{self, *}, Token, TokenKind::{self, *}, TokenResult};
use crate::lexical::Keyword::Equals;
use crate::span::{Span, Spanned};
use crate::syntax::{Block, Expr, ExprKind, InfixOp, Literal::*, Stmt};

pub struct Parser<'s, I: Iterator> {
    token_stream: Peekable<I>,
    errors: Vec<ParserError<'s>>,
}

impl<'s, I: Iterator<Item=TokenResult<'s>>> Parser<'s, I> {
    pub fn new(token_stream: I) -> Self {
        Parser {
            errors: Vec::new(),
            token_stream: token_stream.peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Block<'s>, Vec<ParserError<'s>>> {
        let ast_root = self.sub_parse();

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

    fn consume_token(&mut self) -> Option<TokenResult<'s>> {
        self.token_stream.next()
    }

    fn parse_ident(&mut self, ident: Spanned<&'s str>) -> Result<Expr<'s>, ParserError<'s>> {
        let ident_token = self.consume_token().unwrap().unwrap();
        let next_token = self.next_token()?;

        match next_token.node() {
            TokenKind::Symbol(ParenOpen) => self.parse_fn_call(ident),
            // TokenKind::Symbol(ParenOpen) => self.parse_assignment(ident),
            // TokenKind::Symbol(ParenOpen) => self.parse_add_assignment(ident),
            // TokenKind::Symbol(ParenOpen) => self.parse_sub_assignment(ident),
            _ => unimplemented!(), // TODO: *=, /=, .. and Errors
        }
    }

    fn parse_fn_call(&mut self, ident: Spanned<&'s str>) -> Result<Expr<'s>, ParserError<'s>> {
        // Open Paren
        self.consume_token();

        let token = self.next_token()?;

        // call()
        if token.node() == Symbol(ParenClose) {
            let close_paren_span = self.consume_token().unwrap().unwrap().span();
            let span = Span::new(close_paren_span.file_id, ident.span().start_idx, close_paren_span.end_idx);

            return Ok(Spanned::new(Box::new(ExprKind::FnCall(ident, Vec::new())), span));
        }

        // re: (e[, e]*)
        let regex = And(
            |p: &mut Parser<'s, I>| Self::parse_expr(p, 0),
            ZeroOrMore(And(
                Comma,
                |p: &mut Parser<'s, I>| Self::parse_expr(p, 0),
            ))
        );

        let mut exprs = Vec::new();

        regex.parse(self, &mut exprs)?;

        let end_token = self.next_token()?;

        // Can be close token
        if end_token.node() != Symbol(ParenClose) {
            unimplemented!("err");
        }

        self.consume_token();

        let span = Span::new(token.span().file_id, ident.span().start_idx, end_token.span().end_idx);

        Ok(Spanned::new(Box::new(ExprKind::FnCall(ident, exprs)), span))
    }

    fn get_precedence(&self, token: Token<'s>) -> u8 {
        match token.node() {
            Symbol(Plus) => InfixOp::Add.get_precedence(),
            Symbol(Minus) => InfixOp::Sub.get_precedence(),
            Symbol(Asterisk) => InfixOp::Mul.get_precedence(),
            Symbol(Slash) => InfixOp::Div.get_precedence(),
            Symbol(Percent) => InfixOp::Mod.get_precedence(),
            Symbol(Caret) => InfixOp::Pow.get_precedence(),
            Keyword(Equals) => InfixOp::Equ.get_precedence(),
            _ => 0
        }
    }

    fn is_infix_op(&self, token: Token<'s>) -> bool {
        match token.node() {
            Symbol(Plus) => true,
            Symbol(Minus) => true,
            Symbol(Asterisk) => true,
            Symbol(Slash) => true,
            Symbol(Percent) => true,
            Symbol(Caret) => true,
            Symbol(LessThan) => true,
            Symbol(LessThanEqual) => true,
            Symbol(GreaterThan) => true,
            Symbol(GreaterThanEqual) => true,
            Keyword(Equals) => true,
            _ => false
        }
    }

    fn parse_expr(&mut self, precedence: u8) -> Result<Expr<'s>, ParserError<'s>> {
        // E -> (E) | [E] | E * E | E + E | E - E | E / E | E % E | E ^ E |
        // E equals E | E and E | E or E | not E | -E | Terminal
        // Terminal -> identifier | literal

        let mut lhs = self.sub_parse_expr()?;

        dbg!(&lhs);

        let mut token = self.next_token()?;

        while self.is_infix_op(token) && self.get_precedence(token) >= precedence {
            self.consume_token();

            let new_precedence = self.get_precedence(token) + match token.node() {
                // Right associative ops don't get the +1
                Symbol(Caret) => 0,
                _ => 1,
            };

            let rhs = self.parse_expr(new_precedence)?;
            let infix = match token.node() {
                Symbol(Plus) => InfixOp::Add,
                Symbol(Minus) => InfixOp::Sub,
                Symbol(Asterisk) => InfixOp::Mul,
                Symbol(Slash) => InfixOp::Div,
                Symbol(Percent) => InfixOp::Mod,
                Symbol(Caret) => InfixOp::Pow,
                Symbol(LessThan) => InfixOp::Lt,
                Symbol(LessThanEqual) => InfixOp::Lte,
                Symbol(GreaterThan) => InfixOp::Gt,
                Symbol(GreaterThanEqual) => InfixOp::Gte,
                Keyword(Equals) => InfixOp::Equ,
                _ => unreachable!("Expression parse")
            };

            let span = Span::new(lhs.span().file_id, lhs.span().start_idx, rhs.span().end_idx);

            lhs = Spanned::new(Box::new(ExprKind::InfixOp(infix, lhs, rhs)), span);

            token = self.next_token()?;
        }

        Ok(lhs)
    }

    fn sub_parse_expr(&mut self) -> Result<Expr<'s>, ParserError<'s>> {
        let next_token = self.next_token()?;

        match next_token.node() {
            TokenKind::StrLiteral(s) => {
                self.consume_token();

                Ok(next_token.replace(Box::new(ExprKind::Literal(UTF8String(s)))))
            },
            _ => unimplemented!(),
        }
    }

    fn sub_parse(&mut self) -> Block<'s> {
        let mut stmts = Vec::new();

        loop {
            let next_token = match self.opt_next_token() {
                Ok(Some(t)) => t,
                Err(e) => unimplemented!("Parser error: {:?}", e),
                Ok(None) => break,
            };

            match next_token.node() {
                TokenKind::Comment(..) => { self.consume_token(); },
                TokenKind::Indent(_level) => { self.consume_token(); }, // TODO
                TokenKind::Identifier(ident) => {
                    match self.parse_ident(next_token.replace(ident)) {
                        Ok(expr) => stmts.push(Stmt::new(expr)),
                        Err(err) => self.errors.push(err),
                    };
                    self.consume_token();
                },
                e => unimplemented!("{:?}", e),
            }
        }

        Block::new(stmts)
    }
}

struct And<P, P2>(P, P2);
struct ZeroOrMore<P>(P);

trait RegexParse<'s, I: Iterator<Item=TokenResult<'s>>> {
    fn parse(&self, parser: &mut Parser<'s, I>, exprs: &mut Vec<Expr<'s>>) -> Result<(), ParserError<'s>>;
}

impl<'s, I> RegexParse<'s, I> for Symbol
where
    I: Iterator<Item=TokenResult<'s>>,
{
    fn parse(&self, parser: &mut Parser<'s, I>, _exprs: &mut Vec<Expr<'s>>) -> Result<(), ParserError<'s>> {
        let tok = parser.next_token()?;

        if tok.node() == TokenKind::Symbol(*self) {
            parser.consume_token();

            Ok(())
        } else {
            Err(ParserError {
                kind: ParserErrorKind::UnexpectToken(tok),
            })
        }
    }
}

impl<'s, I, P, P2> RegexParse<'s, I> for And<P, P2>
where
    I: Iterator<Item=TokenResult<'s>>,
    P: RegexParse<'s, I>,
    P2: RegexParse<'s, I>,
{
    fn parse(&self, parser: &mut Parser<'s, I>, exprs: &mut Vec<Expr<'s>>) -> Result<(), ParserError<'s>> {
        self.0.parse(parser, exprs)?;
        self.1.parse(parser, exprs)
    }
}

impl<'s, I, P> RegexParse<'s, I> for ZeroOrMore<P>
where
    I: Iterator<Item=TokenResult<'s>>,
    P: RegexParse<'s, I>,
{
    fn parse(&self, parser: &mut Parser<'s, I>, exprs: &mut Vec<Expr<'s>>) -> Result<(), ParserError<'s>> {
        while let Ok(()) = self.0.parse(parser, exprs) {}

        Ok(())
    }
}

impl<'s, I, F> RegexParse<'s, I> for F
where
    F: Fn(&mut Parser<'s, I>) -> Result<Expr<'s>, ParserError<'s>>,
    I: Iterator<Item=TokenResult<'s>>,
{
    fn parse(&self, parser: &mut Parser<'s, I>, exprs: &mut Vec<Expr<'s>>) -> Result<(), ParserError<'s>> {
        exprs.push(self(parser)?);

        Ok(())
    }
}

trait RegexObject {

}

#[derive(Debug)]
pub enum ParserErrorKind<'s> {
    LexerError(LexerError<'s>),
    // Should be internal only?
    UnexpectToken(Token<'s>),
}

#[derive(Debug)]
pub struct ParserError<'s> {
    kind: ParserErrorKind<'s>,
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
    let ast_block = parser.parse().unwrap();
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

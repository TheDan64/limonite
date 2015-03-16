extern crate limonite;

use std::vec::IntoIter;

use limonite::syntax::lexer::Tokenizer;
use limonite::syntax::parser::Parser;
use limonite::syntax::core::tokens::Token;
use limonite::syntax::core::tokens::Token::*;
use limonite::syntax::core::types::Types;
use limonite::syntax::core::keywords::Keywords;
use limonite::syntax::core::symbols::Symbols;
use limonite::syntax::ast::expr::{Expr, ExprWrapper};

struct MockLexer {
    tokens: IntoIter<Token>
}

impl MockLexer {
    fn new(v: Vec<Token>) -> MockLexer {
        MockLexer {
            tokens: v.into_iter()
        }
    }
}

impl Tokenizer for MockLexer {
    fn get_tok(&mut self) -> Token {
        let next = self.tokens.next();
        match next {
            Some(tok) => tok,
            None => EOF,
        }
    }

    fn get_error_pos(&self) -> (usize, usize, usize, usize) {
        (1, 1, 1, 1)
    }
}

#[test]
fn test_print() {
    let lexer = MockLexer::new(vec![
        Keyword(Keywords::Print),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::Comma),
        Identifier("meow".to_string()),
        Symbol(Symbols::Comma),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
    ]);
    let mut parser = Parser::new(lexer);
    parser.parse();
    let ast = parser.get_ast().get_expr();
    println!("{:?}", ast);

    assert!(ast != ast);
}

#[test]
fn test_variable_int_declaration() {
    // let mLexer = MockLexer::new(vec![
        // Keyword(Keywords::Var),
        // Identifier("meow".to_string()),
        // Symbol(Symbols::Equals),
    // ]);
    // let mut parser = Parser::new(mLexer);

    // parser.parse();
}

#[test]
fn test_valid_fn_declaration() {
    // No args: foo() -> u64
    let lexer = MockLexer::new(vec![Keyword(Keywords::Fn),
                                        Identifier(String::from_str("foo")),
                                        Symbol(Symbols::ParenOpen),
                                        Symbol(Symbols::ParenClose),
                                        Symbol(Symbols::RightThinArrow),
                                        Type(Types::UInt64Bit),
                                        Indent(1),
                                        EOF]);

    let mut parser = Parser::new(lexer);
    parser.parse();

    let ast = parser.get_ast();

    let desired_ast = ExprWrapper::default(Box::new(Expr::Block(vec![ExprWrapper::default(
                      Box::new(Expr::FnDecl(String::from_str("foo"), Vec::new(), Type(Types::UInt64Bit),
                      ExprWrapper::default(Box::new(Expr::Block(Vec::new()))))))])));

    if *ast != desired_ast {
        panic!("No argument test failed");
    }

    // One arg: foo(bar: i32) -> str
    let args = vec![(String::from_str("bar"), Type(Types::Int32Bit))];

    let lexer = MockLexer::new(vec![Keyword(Keywords::Fn),
                                    Identifier(String::from_str("foo")),
                                    Symbol(Symbols::ParenOpen),
                                    Identifier(String::from_str("bar")),
                                    Symbol(Symbols::Colon),
                                    Type(Types::Int32Bit),
                                    Symbol(Symbols::ParenClose),
                                    Symbol(Symbols::RightThinArrow),
                                    Type(Types::Str),
                                    Indent(1),
                                    EOF]);

    let mut parser = Parser::new(lexer);
    parser.parse();

    let ast = parser.get_ast();

    let desired_ast = ExprWrapper::default(Box::new(Expr::Block(vec![ExprWrapper::default(
                      Box::new(Expr::FnDecl(String::from_str("foo"), args, Type(Types::Str),
                      ExprWrapper::default(Box::new(Expr::Block(Vec::new()))))))])));

    if *ast != desired_ast {
        panic!("One argument test failed");
    }

    // Multiple args: foo(bar: i32, left: Obj, right: Obj) -> None
    let args = vec![(String::from_str("bar"), Type(Types::Int32Bit)),
                    (String::from_str("left"), Identifier(String::from_str("Obj"))),
                    (String::from_str("right"), Identifier(String::from_str("Obj")))];

    let lexer = MockLexer::new(vec![Keyword(Keywords::Fn),
                                    Identifier(String::from_str("foo")),
                                    Symbol(Symbols::ParenOpen),
                                    Identifier(String::from_str("bar")),
                                    Symbol(Symbols::Colon),
                                    Type(Types::Int32Bit),
                                    Symbol(Symbols::Comma),
                                    Identifier(String::from_str("left")),
                                    Symbol(Symbols::Colon),
                                    Identifier(String::from_str("Obj")),
                                    Symbol(Symbols::Comma),
                                    Identifier(String::from_str("right")),
                                    Symbol(Symbols::Colon),
                                    Identifier(String::from_str("Obj")),
                                    Symbol(Symbols::ParenClose),
                                    Symbol(Symbols::RightThinArrow),
                                    Type(Types::NoneType),
                                    Indent(1),
                                    EOF]);

    let mut parser = Parser::new(lexer);
    parser.parse();

    let ast = parser.get_ast();

    let desired_ast = ExprWrapper::default(Box::new(Expr::Block(vec![ExprWrapper::default(
                      Box::new(Expr::FnDecl(String::from_str("foo"), args, Type(Types::NoneType),
                      ExprWrapper::default(Box::new(Expr::Block(Vec::new()))))))])));

    if *ast != desired_ast {
        panic!("Multiple argument test failed");
    }

}

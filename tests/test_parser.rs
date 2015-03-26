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
use limonite::syntax::ast::consts::*;
use limonite::syntax::ast::op::*;

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
        Identifier("print".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::Comma),
        StrLiteral("meow".to_string()),
        Symbol(Symbols::Comma),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
    ]);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let desired_ast = ExprWrapper::default(Expr::Block(
        vec![ExprWrapper::default(
            Expr::FnCall(
                "print".to_string(),
                vec![
                    ExprWrapper::default(Expr::Ident("meow".to_string())),
                    ExprWrapper::default(Expr::Const(Const::UTF8String("meow".to_string()))),
                    ExprWrapper::default(Expr::Ident("meow".to_string())),
                ],
            ))
        ]));

    assert!(ast == desired_ast, "\nActual AST: {:?}\nDesired AST: {:?}", ast, desired_ast);
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
    let lexer = MockLexer::new(vec![Keyword(Keywords::Function),
                                    Identifier("foo".to_string()),
                                    Symbol(Symbols::ParenOpen),
                                    Symbol(Symbols::ParenClose),
                                    Symbol(Symbols::RightThinArrow),
                                    Type(Types::UInt64Bit),
                                    Indent(1),
                                    EOF]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::FnDecl("foo".to_string(), Vec::new(), Type(Types::UInt64Bit),
                      ExprWrapper::default(Expr::Block(Vec::new()))))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "No argument test failed");

    // One arg: foo(bar: i32) -> str
    let args = vec![("bar".to_string(), Type(Types::Int32Bit))];

    let lexer = MockLexer::new(vec![Keyword(Keywords::Function),
                                    Identifier("foo".to_string()),
                                    Symbol(Symbols::ParenOpen),
                                    Identifier("bar".to_string()),
                                    Symbol(Symbols::Colon),
                                    Type(Types::Int32Bit),
                                    Symbol(Symbols::ParenClose),
                                    Symbol(Symbols::RightThinArrow),
                                    Type(Types::Str),
                                    Indent(1),
                                    EOF]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::FnDecl("foo".to_string(), args, Type(Types::Str),
                      ExprWrapper::default(Expr::Block(Vec::new()))))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "One argument test failed");

    // Multiple args: foo(bar: i32, left: Obj, right: Obj) -> None
    let args = vec![("bar".to_string(), Type(Types::Int32Bit)),
                    ("left".to_string(), Identifier("Obj".to_string())),
                    ("right".to_string(), Identifier("Obj".to_string()))];

    let lexer = MockLexer::new(vec![Keyword(Keywords::Function),
                                    Identifier("foo".to_string()),
                                    Symbol(Symbols::ParenOpen),
                                    Identifier("bar".to_string()),
                                    Symbol(Symbols::Colon),
                                    Type(Types::Int32Bit),
                                    Symbol(Symbols::Comma),
                                    Identifier("left".to_string()),
                                    Symbol(Symbols::Colon),
                                    Identifier("Obj".to_string()),
                                    Symbol(Symbols::Comma),
                                    Identifier("right".to_string()),
                                    Symbol(Symbols::Colon),
                                    Identifier("Obj".to_string()),
                                    Symbol(Symbols::ParenClose),
                                    Symbol(Symbols::RightThinArrow),
                                    Type(Types::NoneType),
                                    Indent(1),
                                    EOF]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::FnDecl("foo".to_string(), args, Type(Types::NoneType),
                      ExprWrapper::default(Expr::Block(Vec::new()))))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Multiple argument test failed");
}

#[test]
fn test_indentation_levels() {
    // Correct indentation level +1 after fn decl
    // let lexer = MockLexer::new(vec![Keyword(Keywords::Function),
    //                                 Identifier("foo".to_string()),
    //                                 Symbol(Symbols::ParenOpen),
    //                                 Symbol(Symbols::ParenClose),
    //                                 Symbol(Symbols::RightThinArrow),
    //                                 Type(Types::UInt64Bit),
    //                                 Indent(1)]);

    // let mut parser = Parser::new(lexer);
    // let ast = parser.parse();

    // ToDo: this test
}

#[test]
fn test_expression() {
    // if foo + bar equals "foobar",
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Identifier("foo".to_string()),
                                    Symbol(Symbols::Plus),
                                    Identifier("bar".to_string()),
                                    Keyword(Keywords::Equals),
                                    StrLiteral("foobar".to_string()),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Equ,
                    ExprWrapper::default(Expr::InfixOp(InfixOp::Add,
                    ExprWrapper::default(Expr::Ident("foo".to_string())),
                    ExprWrapper::default(Expr::Ident("bar".to_string())))),
                    ExprWrapper::default(Expr::Const(Const::UTF8String("foobar".to_string())))));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));

    assert!(ast == desired_ast, "{:?}, {:?}", ast, desired_ast);
}

#[test]
fn test_expression_precedence_add_mult() {
    // Make sure a + b * c + d generates a + (b * c) + d
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Identifier("a".to_string()),
                                    Symbol(Symbols::Plus),
                                    Identifier("b".to_string()),
                                    Symbol(Symbols::Asterisk),
                                    Identifier("c".to_string()),
                                    Symbol(Symbols::Plus),
                                    Identifier("d".to_string()),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let mult = ExprWrapper::default(Expr::InfixOp(InfixOp::Mul,
               ExprWrapper::default(Expr::Ident("b".to_string())),
               ExprWrapper::default(Expr::Ident("c".to_string()))));

    let left_add = ExprWrapper::default(Expr::InfixOp(InfixOp::Add,
                   ExprWrapper::default(Expr::Ident("a".to_string())), mult));

    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Add, left_add,
                    ExprWrapper::default(Expr::Ident("d".to_string()))));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));


    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Addition and multiplication precedence check failed");
}

#[test]
fn test_expression_precedence_pow() {
    // Make sure a ^ b ^ c generates a ^ (b ^ c) which is right associative
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Identifier("a".to_string()),
                                    Symbol(Symbols::Caret),
                                    Identifier("b".to_string()),
                                    Symbol(Symbols::Caret),
                                    Identifier("c".to_string()),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let right_pow = ExprWrapper::default(Expr::InfixOp(InfixOp::Pow,
                    ExprWrapper::default(Expr::Ident("b".to_string())),
                    ExprWrapper::default(Expr::Ident("c".to_string()))));

    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Pow,
                    ExprWrapper::default(Expr::Ident("a".to_string())), right_pow));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Power precedence check failed");
}

#[test]
fn test_numerics() {
    // Test default type assignment (42.0 should default to f32)
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Numeric("42.0".to_string(), None),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let condition = ExprWrapper::default(Expr::Const(Const::F32Num(42f32)));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Numeric default type test failed");

    // Test explicit type assignment via suffix (42u32 should be u32 as designated)
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Numeric("42".to_string(), Some(Types::UInt32Bit)),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let condition = ExprWrapper::default(Expr::Const(Const::U32Num(42u32)));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Numeric explicit type test failed");

    // Test underscores are removed (3_200 = 3200)
    let lexer = MockLexer::new(vec![Keyword(Keywords::If),
                                    Numeric("0xAF_F3".to_string(), Some(Types::UInt32Bit)),
                                    Symbol(Symbols::Comma),
                                    Indent(1)]);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let condition = ExprWrapper::default(Expr::Const(Const::U32Num(45043u32)));

    let desired_ast = ExprWrapper::default(Expr::Block(vec![ExprWrapper::default(
                      Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])),
                      None))]));

    println!("Desired ast: {:?}", desired_ast);
    println!("Actual ast: {:?}", ast);

    assert!(ast == desired_ast, "Numeric underscore removal test failed");
}

#[test]
fn test_function_call() {
    let lexer = MockLexer::new(vec![Identifier("fn_name".to_string()),
                                    Symbol(Symbols::ParenOpen),
                                    Numeric("42".to_string(), Some(Types::UInt32Bit)),
                                    Symbol(Symbols::Comma),
                                    StrLiteral("b".to_string()),
                                    Symbol(Symbols::Comma),
                                    Numeric("123".to_string(), Some(Types::UInt32Bit)),
                                    Symbol(Symbols::ParenClose),
    ]);

    let mut parser = Parser::new(lexer);
    let parse = parser.parse();
    let ast = parse.get_expr();

    let desired_ast = Expr::Block(
        vec![ExprWrapper::default(Expr::FnCall(
            "fn_name".to_string(),
            vec![
                ExprWrapper::default(Expr::Const(Const::U32Num(42))),
                ExprWrapper::default(Expr::Const(Const::UTF8String("b".to_string()))),
                ExprWrapper::default(Expr::Const(Const::U32Num(123))),
            ])
        )]);

    assert!(*ast == desired_ast, "Expected: {:?}, but found: {:?}", ast, desired_ast);
}

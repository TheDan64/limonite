extern crate limonite;
extern crate env_logger;

use std::vec::IntoIter;

use limonite::lexical::tokens::Tokens;
use limonite::lexical::tokens::Tokens::*;
use limonite::lexical::types::Types;
use limonite::lexical::keywords::Keywords;
use limonite::lexical::symbols::Symbols;
use limonite::syntax::parser::Parser;
use limonite::syntax::expr::{Expr, ExprWrapper};
use limonite::syntax::literals::*;
use limonite::syntax::op::*;

struct MockLexer {
    tokens: IntoIter<Tokens>
}

impl MockLexer {
    fn new(v: Vec<Tokens>) -> MockLexer {
        MockLexer {
            tokens: v.into_iter()
        }
    }
}

impl Iterator for MockLexer {
    type Item = Tokens;
    fn next(&mut self) -> Option<Tokens> {
        let next = self.tokens.next();
        match next {
            Some(tok) => Some(tok),
            None => None,
        }
    }
}

fn general_expect_test(tokens: Vec<Tokens>,
                       expected: Vec<ExprWrapper>,
                       should_match: bool) {
    let lexer = MockLexer::new(tokens);
    let mut parser = Parser::new(lexer);
    let ast_root = match parser.parse() {
        Some(ast) => ast,
        None => {
            if should_match {
                panic!("Expected an ast root");
            }
            ExprWrapper::default(Expr::NoOp)
        },
    };

    let ast = ast_root.get_expr();
    let expected = Expr::Block(expected);

    let verbiage = if should_match {
        "Expected"
    } else {
        "Did not expect"
    };
    assert!((expected == *ast) == should_match,
            "\n{}:\n    {:?}\n, but found:\n    {:?}",
            verbiage,
            expected,
            ast);
}

fn expect_test(tokens: Vec<Tokens>, expected: Vec<ExprWrapper>) {
    general_expect_test(tokens, expected, true);
}

fn unexpect_test(tokens: Vec<Tokens>, expected: Vec<ExprWrapper>) {
    general_expect_test(tokens, expected, false);
}

#[test]
fn main() {
    env_logger::init().unwrap();
}

#[test]
fn test_print() {
    let tokens = vec![
        Identifier("print".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::Comma),
        StrLiteral("meow".to_string()),
        Symbol(Symbols::Comma),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
    ];
    let desired_ast = vec![ExprWrapper::default(
            Expr::FnCall(
                "print".to_string(),
                vec![
                    ExprWrapper::default(Expr::Var("meow".to_string())),
                    ExprWrapper::default(Expr::Literal(Literals::UTF8String("meow".to_string()))),
                    ExprWrapper::default(Expr::Var("meow".to_string())),
                ],
            ))];

    expect_test(tokens, desired_ast);
}

#[test]
fn test_valid_fn_definition() {
    // No args: foo() -> u64
    let tokens = vec![
        Keyword(Keywords::Function),
        Identifier("foo".to_string()),
        Symbol(Symbols::ParenOpen),
        Symbol(Symbols::ParenClose),
        Symbol(Symbols::RightThinArrow),
        Identifier("int".to_string()),
        Indent(1),
    ];
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::FnDecl("foo".to_string(), Vec::new(), Some("int".to_string()),
            ExprWrapper::default(Expr::Block(Vec::new()))))
    ];
    expect_test(tokens, desired_ast);

    let tokens = vec![
        Keyword(Keywords::Function),
        Identifier("foo".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("bar".to_string()),
        Symbol(Symbols::Colon),
        Identifier("int".to_string()),
        Symbol(Symbols::ParenClose),
        Symbol(Symbols::RightThinArrow),
        Identifier("str".to_string()),
        Indent(1),
    ];

    // One arg: foo(bar: i32) -> str
    let args = vec![("bar".to_string(), "int".to_string())];
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::FnDecl("foo".to_string(), args, Some("str".to_string()),
            ExprWrapper::default(Expr::Block(Vec::new()))))
    ];
    expect_test(tokens, desired_ast);

    let tokens = vec![
        Keyword(Keywords::Function),
        Identifier("foo".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("bar".to_string()),
        Symbol(Symbols::Colon),
        Identifier("int".to_string()),
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
        Identifier("None".to_string()),
        Indent(1),
    ];
    //
    // Multiple args: foo(bar: i32, left: Obj, right: Obj) -> None
    let args = vec![("bar".to_string(), "int".to_string()),
                    ("left".to_string(), "Obj".to_string()),
                    ("right".to_string(), "Obj".to_string())];
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::FnDecl("foo".to_string(), args, Some("None".to_string()),
            ExprWrapper::default(Expr::Block(Vec::new()))))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_expression() {
    // if foo + bar equals "foobar",
    let tokens = vec![
        Keyword(Keywords::If),
        Identifier("foo".to_string()),
        Symbol(Symbols::Plus),
        Identifier("bar".to_string()),
        Keyword(Keywords::Equals),
        StrLiteral("foobar".to_string()),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Equ,
                    ExprWrapper::default(Expr::InfixOp(InfixOp::Add,
                    ExprWrapper::default(Expr::Var("foo".to_string())),
                    ExprWrapper::default(Expr::Var("bar".to_string())))),
                    ExprWrapper::default(Expr::Literal(Literals::UTF8String("foobar".to_string())))));
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_expression_precedence_add_mult() {
    // Make sure a + b * c + d generates a + (b * c) + d
    let tokens = vec![
        Keyword(Keywords::If),
        Identifier("a".to_string()),
        Symbol(Symbols::Plus),
        Identifier("b".to_string()),
        Symbol(Symbols::Asterisk),
        Identifier("c".to_string()),
        Symbol(Symbols::Plus),
        Identifier("d".to_string()),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let mult = ExprWrapper::default(Expr::InfixOp(InfixOp::Mul,
               ExprWrapper::default(Expr::Var("b".to_string())),
               ExprWrapper::default(Expr::Var("c".to_string()))));
    let left_add = ExprWrapper::default(Expr::InfixOp(InfixOp::Add,
                   ExprWrapper::default(Expr::Var("a".to_string())), mult));
    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Add, left_add,
                    ExprWrapper::default(Expr::Var("d".to_string()))));

    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_expression_precedence_pow() {
    // Make sure a ^ b ^ c generates a ^ (b ^ c) which is right associative
    let tokens = vec![
        Keyword(Keywords::If),
        Identifier("a".to_string()),
        Symbol(Symbols::Caret),
        Identifier("b".to_string()),
        Symbol(Symbols::Caret),
        Identifier("c".to_string()),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let right_pow = ExprWrapper::default(Expr::InfixOp(InfixOp::Pow,
                    ExprWrapper::default(Expr::Var("b".to_string())),
                    ExprWrapper::default(Expr::Var("c".to_string()))));
    let condition = ExprWrapper::default(Expr::InfixOp(InfixOp::Pow,
                    ExprWrapper::default(Expr::Var("a".to_string())), right_pow));

    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_numerics() {
    // Test default type assignment (42.0 should default to f32)
    let tokens = vec![
        Keyword(Keywords::If),
        Numeric("42.0".to_string(), None),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let condition = ExprWrapper::default(Expr::Literal(Literals::F32Num(42f32)));
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);

    // Test explicit type assignment via suffix (42u32 should be u32 as designated)
    let tokens = vec![
        Keyword(Keywords::If),
        Numeric("42".to_string(), Some(Types::UInt32Bit)),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let condition = ExprWrapper::default(Expr::Literal(Literals::U32Num(42u32)));
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);

    // Test underscores are removed (3_200 = 3200)
    let tokens = vec![
        Keyword(Keywords::If),
        Numeric("0xAF_F3".to_string(), Some(Types::UInt32Bit)),
        Symbol(Symbols::Comma),
        Indent(1)
    ];

    let condition = ExprWrapper::default(Expr::Literal(Literals::U32Num(45043u32)));
    let desired_ast = vec![
        ExprWrapper::default(
            Expr::If(condition, ExprWrapper::default(Expr::Block(vec![])), None))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_function_call() {
    let tokens = vec![
        Identifier("fn_name".to_string()),
        Symbol(Symbols::ParenOpen),
        Numeric("42".to_string(), Some(Types::UInt32Bit)),
        Symbol(Symbols::Comma),
        StrLiteral("b".to_string()),
        Symbol(Symbols::Comma),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
        Symbol(Symbols::ParenClose),
    ];

    let desired_ast = vec![
        ExprWrapper::default(Expr::FnCall(
            "fn_name".to_string(),
            vec![
                ExprWrapper::default(Expr::Literal(Literals::U32Num(42))),
                ExprWrapper::default(Expr::Literal(Literals::UTF8String("b".to_string()))),
                ExprWrapper::default(Expr::Literal(Literals::U32Num(123))),
            ])
        )];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_variable_declaration() {
    for typ in vec![Keywords::Var, Keywords::Def] {
        let tokens = vec![
            Keyword(typ),
            Identifier("name".to_string()),
            Symbol(Symbols::Colon),
            Identifier("i32".to_string()),
            Symbol(Symbols::Equals),
            Numeric("123".to_string(), Some(Types::UInt32Bit)),
        ];
        let desired_ast = vec![
            ExprWrapper::default(Expr::VarDecl(
                typ == Keywords::Def,
                "name".to_string(),
                Some("i32".to_string()),
                ExprWrapper::default(Expr::Literal(Literals::U32Num(123))),
            ))];
        expect_test(tokens, desired_ast);
    }
}

#[test]
fn test_assign() {
    let tokens = vec![
        Identifier("b".to_string()),
        Symbol(Symbols::Equals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
    ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::Assign(
            ExprWrapper::default(Expr::Var("b".to_string())),
            ExprWrapper::default(
                Expr::Literal(Literals::U32Num(123)),
            )
        ))
    ];
    expect_test(tokens, desired_ast);

    let tokens = vec![
        Identifier("b".to_string()),
        Symbol(Symbols::PlusEquals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
    ];
    let desired_ast = vec![
        assign!(var!("b"), += u32!(123))
    ];
    expect_test(tokens, desired_ast);

    let tokens = vec![
        Identifier("b".to_string()),
        Symbol(Symbols::MinusEquals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
    ];
    let desired_ast = vec![
        assign!(var!("b"), -= u32!(123))

    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_while_loop() {
    let tokens = vec![
        Keyword(Keywords::While),
        Identifier("a".to_string()),
        Symbol(Symbols::Plus),
        Identifier("b".to_string()),
        Symbol(Symbols::Comma),
        Indent(1),
        Identifier("b".to_string()),
        Symbol(Symbols::Equals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
    ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::WhileLoop(
            ExprWrapper::default(Expr::InfixOp(
                InfixOp::Add,
                ExprWrapper::default(Expr::Var("a".to_string())),
                ExprWrapper::default(Expr::Var("b".to_string())),
            )),
            ExprWrapper::default(Expr::Block(vec![
                ExprWrapper::default(Expr::Assign(
                    ExprWrapper::default(Expr::Var("b".to_string())),
                    ExprWrapper::default(
                        Expr::Literal(Literals::U32Num(123)),
                    )
                ))
            ]))
        ))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_indentation() {
    let indents = vec![(Indent(1), false), (Indent(2), true), (Indent(3), false)];
    for (indent, expect) in indents {
        let tokens = vec![
            Keyword(Keywords::While),
            Identifier("a".to_string()),
            Symbol(Symbols::Plus),
            Identifier("b".to_string()),
            Symbol(Symbols::Comma),
            Indent(1),
            Keyword(Keywords::While),
            Identifier("a".to_string()),
            Symbol(Symbols::Plus),
            Identifier("b".to_string()),
            Symbol(Symbols::Comma),
            indent,
            Identifier("b".to_string()),
            Symbol(Symbols::Equals),
            Numeric("123".to_string(), Some(Types::UInt32Bit)),
        ];
        let desired_ast = vec![
            ExprWrapper::default(Expr::WhileLoop(
                ExprWrapper::default(Expr::InfixOp(
                    InfixOp::Add,
                    ExprWrapper::default(Expr::Var("a".to_string())),
                    ExprWrapper::default(Expr::Var("b".to_string())),
                )),
                ExprWrapper::default(Expr::Block(vec![
                    ExprWrapper::default(Expr::WhileLoop(
                        ExprWrapper::default(Expr::InfixOp(
                            InfixOp::Add,
                            ExprWrapper::default(Expr::Var("a".to_string())),
                            ExprWrapper::default(Expr::Var("b".to_string())),
                        )),
                        ExprWrapper::default(Expr::Block(vec![
                            ExprWrapper::default(Expr::Assign(
                                ExprWrapper::default(Expr::Var("b".to_string())),
                                ExprWrapper::default(
                                    Expr::Literal(Literals::U32Num(123)),
                                )
                            ))
                        ]))
                    ))
                ]))
            ))
        ];
        if expect {
            expect_test(tokens, desired_ast);
        } else {
            unexpect_test(tokens, desired_ast);
        }
    }
}

#[test]
fn test_multiple_statements() {
    let tokens = vec![
        Keyword(Keywords::While),
        Identifier("a".to_string()),
        Symbol(Symbols::Plus),
        Identifier("b".to_string()),
        Symbol(Symbols::Comma),
        Indent(1),
        Identifier("b".to_string()),
        Symbol(Symbols::Equals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
        Indent(1),
        Identifier("b".to_string()),
        Symbol(Symbols::Equals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
    ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::WhileLoop(
            ExprWrapper::default(Expr::InfixOp(
                InfixOp::Add,
                ExprWrapper::default(Expr::Var("a".to_string())),
                ExprWrapper::default(Expr::Var("b".to_string())),
            )),
            ExprWrapper::default(Expr::Block(vec![
                ExprWrapper::default(Expr::Assign(
                    ExprWrapper::default(Expr::Var("b".to_string())),
                    ExprWrapper::default(
                        Expr::Literal(Literals::U32Num(123)),
                    )
                )),
                ExprWrapper::default(Expr::Assign(
                    ExprWrapper::default(Expr::Var("b".to_string())),
                    ExprWrapper::default(
                        Expr::Literal(Literals::U32Num(123)),
                    )
                )),
            ]))
        ))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_statements_on_one_indent_level() {
    let tokens = vec![
        Comment("Wow!".to_string()),
        Indent(0),
        Identifier("print".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
        Identifier("print".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
     ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::FnCall(
            "print".to_string(),
            vec![
                ExprWrapper::default(Expr::Var("meow".to_string())),
            ],
        )),
        ExprWrapper::default(Expr::FnCall(
            "print".to_string(),
            vec![
                ExprWrapper::default(Expr::Var("meow".to_string())),
            ],
        )),
    ];
    unexpect_test(tokens, desired_ast);
}

#[test]
fn test_indent_then_dedent() {
    let tokens = vec![
        Keyword(Keywords::While),
        Identifier("a".to_string()),
        Symbol(Symbols::Plus),
        Identifier("b".to_string()),
        Symbol(Symbols::Comma),
        Indent(1),
        Identifier("b".to_string()),
        Symbol(Symbols::Equals),
        Numeric("123".to_string(), Some(Types::UInt32Bit)),
        Indent(0),
        Identifier("print".to_string()),
        Symbol(Symbols::ParenOpen),
        Identifier("meow".to_string()),
        Symbol(Symbols::ParenClose),
     ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::WhileLoop(
            ExprWrapper::default(Expr::InfixOp(
                InfixOp::Add,
                ExprWrapper::default(Expr::Var("a".to_string())),
                ExprWrapper::default(Expr::Var("b".to_string())),
            )),
            ExprWrapper::default(Expr::Block(vec![
                ExprWrapper::default(Expr::Assign(
                    ExprWrapper::default(Expr::Var("b".to_string())),
                    ExprWrapper::default(
                        Expr::Literal(Literals::U32Num(123)),
                    )
                )),
            ])),
        )),
        ExprWrapper::default(Expr::FnCall(
            "print".to_string(),
            vec![
                ExprWrapper::default(Expr::Var("meow".to_string())),
            ],
        ))
    ];
    expect_test(tokens, desired_ast);
}

#[test]
fn test_nested_indent_then_dedent() {
    // while a,
    //     while a,
    //         while a,
    //             b = c
    //         d = e
    // f = g
    let tokens = vec![
        Keyword(Keywords::While),
        Identifier("a".to_string()),
        Symbol(Symbols::Comma),
        Indent(1),
            Keyword(Keywords::While),
            Identifier("a".to_string()),
            Symbol(Symbols::Comma),
            Indent(2),
                Keyword(Keywords::While),
                Identifier("a".to_string()),
                Symbol(Symbols::Comma),
                Indent(3),
                    Identifier("b".to_string()),
                    Symbol(Symbols::Equals),
                    Identifier("c".to_string()),
                Indent(2),
                Identifier("d".to_string()),
                Symbol(Symbols::Equals),
                Identifier("e".to_string()),
        Indent(0),
        Identifier("f".to_string()),
        Symbol(Symbols::Equals),
        Identifier("g".to_string()),
    ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::WhileLoop(
            ExprWrapper::default(Expr::Var("a".to_string())),
            ExprWrapper::default(Expr::Block(vec![
                ExprWrapper::default(Expr::WhileLoop(
                    ExprWrapper::default(Expr::Var("a".to_string())),
                    ExprWrapper::default(Expr::Block(vec![
                        ExprWrapper::default(Expr::WhileLoop(
                            ExprWrapper::default(Expr::Var("a".to_string())),
                            ExprWrapper::default(Expr::Block(vec![
                                ExprWrapper::default(Expr::Assign(
                                    ExprWrapper::default(Expr::Var("b".to_string())),
                                    ExprWrapper::default(Expr::Var("c".to_string())),
                                )),
                            ])),
                        )),
                        ExprWrapper::default(Expr::Assign(
                            ExprWrapper::default(Expr::Var("d".to_string())),
                            ExprWrapper::default(Expr::Var("e".to_string())),
                        )),
                    ])),
                )),
            ])),
        )),
        ExprWrapper::default(Expr::Assign(
            ExprWrapper::default(Expr::Var("f".to_string())),
            ExprWrapper::default(Expr::Var("g".to_string())),
        )),
    ];
    expect_test(tokens, desired_ast);
}



#[test]
fn test_indented_comment() {
    // while i,
    //         >> C
    //         i = i - 1
    let tokens = vec![
        Indent(0),
        Keyword(Keywords::While),
        Identifier("i".to_string()),
        Symbol(Symbols::Comma),

            Indent(1),
            Comment("Example".to_string()),

            Indent(1),
            Identifier("i".to_string()),
            Symbol(Symbols::Equals),
            Identifier("i".to_string()),
            Symbol(Symbols::Minus),
            Numeric("1".to_string(), None),
    ];
    let desired_ast = vec![
        ExprWrapper::default(Expr::WhileLoop(
            ExprWrapper::default(Expr::Var("i".to_string())),
            ExprWrapper::default(Expr::Block(vec![
                ExprWrapper::default(Expr::Assign(
                    ExprWrapper::default(Expr::Var("i".to_string())),
                    ExprWrapper::default(Expr::InfixOp(
                        InfixOp::Sub,
                        ExprWrapper::default(Expr::Var("i".to_string())),
                        ExprWrapper::default(Expr::Literal(Literals::I32Num(1))),
                    )),
                )),
            ])),
        )),
    ];
    expect_test(tokens, desired_ast);
}

// extern crate limonite;

// use limonite::syntax::expr::{Expr, ExprWrapper};
// use limonite::syntax::op::InfixOp::*;
// use limonite::syntax::literals::Literals::*;
// use limonite::semantic::type_checker::TypeChecker;
// use limonite::semantic::analyzer_trait::ASTAnalyzer;

// // Type Checker
// #[test]
// fn test_type_inference_builtin_type() {
//     // var string = "RHS"
//     // expands to
//     // var string: str = "RHS"
//     let mut input_ast = ExprWrapper::default(
//         Expr::VarDecl(
//             false,
//             "string".into(),
//             None,
//             ExprWrapper::default(Expr::Literal(UTF8String("RHS".into())))
//         )
//     );

//     TypeChecker::new().analyze(&mut input_ast);

//     let desired_ast = ExprWrapper::default(
//         Expr::VarDecl(
//             false,
//             "string".into(),
//             Some("str".into()),
//             ExprWrapper::default(Expr::Literal(UTF8String("RHS".into())))
//         )
//     );

//     assert!(input_ast == desired_ast);
// }

// #[test]
// fn test_infix_op_valid() {
//     // 'a' + 'b'
//     // 5 - 1
//     let mut input_ast = ExprWrapper::default(
//         Expr::InfixOp(
//             Add,
//             ExprWrapper::default(Expr::Literal(UTF8Char('a'))),
//             ExprWrapper::default(Expr::Literal(UTF8Char('b'))),
//         )
//     );

//     let mut input_ast2 = ExprWrapper::default(
//         Expr::InfixOp(
//             Sub,
//             ExprWrapper::default(Expr::Literal(U32Num(5))),
//             ExprWrapper::default(Expr::Literal(U32Num(1))),
//         )
//     );

//     TypeChecker::new().analyze(&mut input_ast);
//     TypeChecker::new().analyze(&mut input_ast2);
// }

// #[test]
// #[should_panic]
// // Won't panic when actually displaying errors correctly
// fn test_infix_op_invalid() {
//     // 'a' + 5
//     let mut input_ast = ExprWrapper::default(
//         Expr::InfixOp(
//             Add,
//             ExprWrapper::default(Expr::Literal(UTF8Char('a'))),
//             ExprWrapper::default(Expr::Literal(U32Num(5))),
//         )
//     );

//     TypeChecker::new().analyze(&mut input_ast);
// }

extern crate limonite;

use limonite::syntax::expr::{Expr, ExprWrapper};
use limonite::syntax::literals::Literals::*;
use limonite::semantic::type_checker::TypeChecker;
use limonite::semantic::analyzer_trait::ASTAnalyzer;

// Type Checker
#[test]
fn test_type_inference_builtin_type() {
    // var string = "RHS"
    // expands to
    // var string: str = "RHS"
    let mut input_ast = ExprWrapper::default(
        Expr::VarDecl(
            false,
            "string".into(),
            None,
            ExprWrapper::default(Expr::Literal(UTF8String("RHS".into())))
        )
    );

    TypeChecker::new().analyze(&mut input_ast);

    let desired_ast = ExprWrapper::default(
        Expr::VarDecl(
            false,
            "string".into(),
            Some("str".into()),
            ExprWrapper::default(Expr::Literal(UTF8String("RHS".into())))
        )
    );

    assert!(input_ast == desired_ast);
}

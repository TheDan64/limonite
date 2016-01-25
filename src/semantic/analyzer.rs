use std::marker::PhantomData;
use syntax::expr::{Expr, ExprWrapper};
use semantic::analyzer_trait::ASTAnalyzer;
use semantic::type_checker::TypeChecker;

pub struct SemanticAnalyzer<'a> {
    phantom: PhantomData<&'a i32>
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new() -> SemanticAnalyzer<'a> {
        SemanticAnalyzer {
            phantom: PhantomData
        }
    }
}

impl<'a> ASTAnalyzer<'a> for SemanticAnalyzer<'a> {
    fn analyze(&mut self, ast_root: &'a mut ExprWrapper) -> &'a mut ExprWrapper {
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?
        let mut type_checker = TypeChecker::new();
        // More stages

        let mut ast_root = type_checker.analyze(ast_root);

        ast_root
    }
}

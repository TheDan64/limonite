use std::marker::PhantomData;
use syntax::expr::{Expr, ExprWrapper};
use semantic::analyzer_trait::ASTAnalyzer;

pub struct TypeChecker<'a> {
    phantom: PhantomData<&'a i32>
}

impl<'a> TypeChecker<'a> {
    pub fn new() -> TypeChecker<'a> {
        TypeChecker {
            phantom: PhantomData
        }
    }
}

impl<'a> ASTAnalyzer<'a> for TypeChecker<'a> {
    fn analyze(&mut self, ast_root: &'a mut ExprWrapper) -> &'a mut ExprWrapper {
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?

        ast_root
    }
}

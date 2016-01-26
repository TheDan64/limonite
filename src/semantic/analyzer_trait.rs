use syntax::expr::ExprWrapper;

pub trait ASTAnalyzer {
    fn analyze(&mut self, ast_root: &mut ExprWrapper); // Return an Option<Vec<Error>>?
}

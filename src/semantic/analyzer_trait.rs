use syntax::expr::ExprWrapper;

pub trait ASTAnalyzer<T> {
    fn analyze(&mut self, ast_root: &mut ExprWrapper) -> T;
    // Data member with a Vec<Error>?
}

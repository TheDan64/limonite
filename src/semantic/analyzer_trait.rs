use syntax::expr::ExprWrapper;

pub trait ASTAnalyzer<'a> {
    fn analyze(&mut self, ast_root: &'a mut ExprWrapper) -> &'a mut ExprWrapper;
}

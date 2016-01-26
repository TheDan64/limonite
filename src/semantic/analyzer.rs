use syntax::expr::ExprWrapper;
use semantic::analyzer_trait::ASTAnalyzer;
use semantic::type_checker::TypeChecker;

pub struct SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer
    }
}

impl ASTAnalyzer for SemanticAnalyzer {
    fn analyze(&mut self, ast_root: &mut ExprWrapper){
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?
        let mut type_checker = TypeChecker::new();
        // More stages

        type_checker.analyze(ast_root);
    }
}

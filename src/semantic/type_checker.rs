use syntax::expr::ExprWrapper;
use syntax::expr::Expr::*;
use semantic::analyzer_trait::ASTAnalyzer;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker
    }
}

impl ASTAnalyzer for TypeChecker {
    fn analyze(&mut self, ast_root: &mut ExprWrapper) {
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?
        match ast_root.get_mut_expr() {
            &mut Block(ref mut vec) => {
                for expr_wrapper in vec {
                    println!("Looping over wrapper {:?}!", expr_wrapper);
                    self.analyze(expr_wrapper);
                }
            },
            &mut VarDecl(ref mut const_, ref mut name, ref mut type_, ref mut expr_wrapper) => {
                let expr_type = expr_wrapper.get_expr();

                // assert!(expr_type == Literal | FnCall | )

                match type_ {
                    &mut Some(ref t) => {
                        if *t != String::new() {
                            panic!("Error goes here")
                        }
                    },
                    &mut None => *type_ = Some(String::new())
                }

            },
            &mut FnCall(ref name, ref args) => (),
            node => panic!("Unimplemented node: {:?}", node)
        }
    }
}

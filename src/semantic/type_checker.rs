use std::str;

use lexical::types::Types;
use syntax::expr::ExprWrapper;
use syntax::expr::Expr::*;
use semantic::analyzer_trait::ASTAnalyzer;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker
    }

    fn analyze_to_type(&mut self, ast_root: &mut ExprWrapper) -> Option<String> {
        match ast_root.get_mut_expr() {
            &mut Block(ref mut vec) => {
                for expr_wrapper in vec {
                    println!("Looping over wrapper {:?}!", expr_wrapper);
                    self.analyze(expr_wrapper);
                }
            },
            &mut VarDecl(ref mut const_, ref mut name, ref mut type_, ref mut expr_wrapper) => {
                let rhs_type = self.analyze_to_type(expr_wrapper);

                println!("Var decl rhs type: {:?}", rhs_type);

                // How to get Expr's type?
                // Basics:
                // Literal -> Type
                // Variable -> VarName -> Lookup VarDecl -> Type
                // FnCall -> FnName -> LookUp FnDecl -> Type
                // InfixOp -> analyze bubble up type from Expr op Expr
                // UnaryOp -> analyze bubble up type from Expr

                // Future?:
                // if rust style let i = if a {} else {}
                // If needs lookup all returns in all possible code blocks, assert they're the same type




                match type_ {
                    &mut Some(ref str) => {
                        match str.parse::<Types>() {
                            Ok(t) => {
                                if t != Types::Bool { // FIXME
                                    panic!("Error goes here");
                                }
                            },
                            Err(()) => {
                                // TODO: Custom type
                                panic!("Found a custom type reference");
                            }
                        }
                    },
                    &mut None => *type_ = Some("i32".parse().unwrap()) // FIXME
                }

            },
            &mut FnCall(ref name, ref args) => (), // FIXME
            &mut Literal(ref literal) => return Some(literal.to_string()), // Done?
            node => panic!("Unimplemented node: {:?}", node)
        }

        None // tmp
    }
}

impl ASTAnalyzer for TypeChecker {
    fn analyze(&mut self, ast_root: &mut ExprWrapper) {
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?
        self.analyze_to_type(ast_root);
    }
}

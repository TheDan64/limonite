#![allow(unused_variables)]
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
        // How to get Expr's type?
        // Basics:
        // Literal -> Builtin Type (Done) or Custom Type
        // Variable -> VarName -> Lookup VarDecl -> Type
        // FnCall -> FnName -> LookUp FnDecl -> Type
        // InfixOp -> analyze bubble up type from Expr op Expr
        // UnaryOp -> analyze bubble up type from Expr

        // Future?:
        // if rust style let i = if a {} else {}
        // If needs lookup all returns in all possible code blocks, assert they're the same type

        match ast_root.get_mut_expr() {
            &mut Block(ref mut vec) => {
                for expr_wrapper in vec {
                    println!("Looping over expr {:?}!", expr_wrapper);
                    self.analyze(expr_wrapper);
                }
            },
            &mut VarDecl(ref mut const_, ref mut name, ref mut type_, ref mut expr_wrapper) => {
                let rhs_type = match self.analyze_to_type(expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // REVIEW
                };

                println!("Var decl rhs type: {:?}", rhs_type);

                match type_ {
                    &mut Some(ref str) => {
                        match (str.parse::<Types>(), rhs_type.parse::<Types>()) {
                            (Ok(lhs_type), Ok(rhs_type)) => {
                                if lhs_type != rhs_type {
                                    panic!("Error goes here"); // FIXME: Better errors
                                } else {
                                    return Some(str.clone()); // No way to not clone?
                                }
                            },
                            (Err(()), Err(())) => {
                                // TODO: Custom type comparison. May or may not be an error.
                                panic!("Found a custom type references");
                            },
                            // Found a builtin type and a custom type -> error
                            _ => panic!("Error goes here") // FIXME: Better errors
                        }
                    },
                    &mut None => *type_ = Some(rhs_type) // Done?
                    // REVIEW: The above was *type_ = Some("i32".parse().unwrap())
                    // Codegen doesn't seem to care what the actual type is.
                    // Does that matter if type checker does its job correctly?
                }

            },
            &mut FnCall(ref name, ref args) => (), // FIXME: Compare to fn declaration
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

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

    fn cmp_lhs_rhs(lhs_str: String, rhs_str: String) -> Option<String> {
        match (lhs_str.parse::<Types>(), rhs_str.parse::<Types>()) {
            (Ok(lhs_type), Ok(rhs_type)) => {
                if lhs_type != rhs_type {
                    panic!("Error goes here"); // FIXME: Better errors
                }

                Some(lhs_str)
            },
            (Err(()), Err(())) => {
                // TODO: Custom type comparison. May or may not be an error.
                panic!("Found a custom type references");
            },
            // Found a builtin type and a custom type -> error
            _ => panic!("Error goes here") // FIXME: Better errors
        }
    }
}

impl ASTAnalyzer<Option<String>> for TypeChecker {
    fn analyze(&mut self, ast_root: &mut ExprWrapper) -> Option<String> {
        // TODO: Handle errors, Result<ExprWrapper, String>?
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

        match *ast_root.get_mut_expr() {
            Assign(ref mut var_name_expr_wrapper, ref mut rhs_expr_wrapper) => {
                let lhs_type = self.analyze(var_name_expr_wrapper).unwrap();
                let rhs_type = self.analyze(rhs_expr_wrapper).unwrap();

                TypeChecker::cmp_lhs_rhs(lhs_type, rhs_type)
            },
            Block(ref mut vec) => {
                let mut last_seen_type: Option<String> = None;

                for expr_wrapper in vec {
                    debug!("Looping over expr {:?}!", expr_wrapper);

                    let opt_current_type = self.analyze(expr_wrapper);

                    if let Return(_) = *expr_wrapper.get_expr() {
                        let mut set_last_seen_type = false;

                        match (&last_seen_type, &opt_current_type) {
                            (&Some(ref last_type), &Some(ref current_type)) => {
                                if last_type != current_type {
                                    panic!("Found different types in block") // FIXME: Better errors
                                }

                                set_last_seen_type = true;
                            },
                            (&None, &Some(ref current_type)) => {
                                set_last_seen_type = true;
                            },
                            _ => ()
                        };

                        if set_last_seen_type {
                            last_seen_type = opt_current_type;
                        };
                    };
                }

                None // FIXME?
            },
            FnCall(ref fn_name, ref args) => None, // FIXME: Compare to fn declaration?
            FnDecl(ref fn_name, ref args, ref mut ret_type, ref mut body_expr_wrapper) => {
                for &(ref name, ref _type) in args {
                    if let Err(()) = _type.parse::<Types>() {
                        // TODO: Custom type found. Figure out if it is valid
                    }
                }

                if ret_type != &self.analyze(body_expr_wrapper) {
                    panic!("Error goes here") // FIXME: Better errors
                }

                ret_type.clone() // Better way than to clone?
            },
            If(ref mut cond_expr_wrapper, ref mut body_expr_wrapper, ref mut opt_else_expr_wrapper) => {
                // First arg should be Opt<>? for else {} clause? Or Else could eval this to True
                // if let Some(cond_expr_wrapper) = opt_cond_expr_wrapper {
                if self.analyze(cond_expr_wrapper) != Some("bool".into()) {
                    panic!("Error goes here, not a bool") // FIXME: Better errors
                }

                None // FIXME
            },
            InfixOp(ref op, ref mut lhs_expr_wrapper, ref mut rhs_expr_wrapper) => {
                let lhs_type = self.analyze(lhs_expr_wrapper).unwrap();
                let rhs_type = self.analyze(rhs_expr_wrapper).unwrap();

                TypeChecker::cmp_lhs_rhs(lhs_type, rhs_type)
            },
            Literal(ref literal) => Some(literal.to_string()), // Done?
            Return(ref mut opt_ret_type) => match *opt_ret_type { // Done?
                Some(ref mut expr_wrapper) => self.analyze(expr_wrapper),
                None => Some("None".into()) // None type?
            },
            UnaryOp(ref op, ref mut expr_wrapper) => self.analyze(expr_wrapper),
            Var(ref name) => unimplemented!(), // FIXME: Lookup VarDecl type
            VarDecl(ref const_, ref name, ref mut opt_type, ref mut expr_wrapper) => {
                let rhs_type = self.analyze(expr_wrapper).unwrap();

                match *opt_type {
                    Some(ref lhs_type) => TypeChecker::cmp_lhs_rhs(lhs_type.clone(), rhs_type),  // No way to not clone?
                    None => {
                        *opt_type = Some(rhs_type); // Done?

                        None
                    }
                }
            },
            WhileLoop(ref mut cond_expr_wrapper, ref mut body_expr_wrapper) => {
                if self.analyze(cond_expr_wrapper) != Some("bool".into()) {
                    panic!("Error goes here, not a bool") // FIXME: Better errors
                }

                // WhileLoop's type, if any, is the type of the body
                self.analyze(body_expr_wrapper)
            },
            NoOp => None,
        }
    }
}

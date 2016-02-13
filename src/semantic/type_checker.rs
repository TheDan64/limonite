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
        // TODO: Handle errors, Option<ExprWrapper> like elsewhere for now?
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
                let lhs_type = match self.analyze(var_name_expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // VERIFY, unwrap?
                };
                let rhs_type = match self.analyze(rhs_expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // VERIFY, unwrap?
                };

                TypeChecker::cmp_lhs_rhs(lhs_type, rhs_type)
            },
            Block(ref mut vec) => {
                for expr_wrapper in vec {
                    println!("Looping over expr {:?}!", expr_wrapper);
                    self.analyze(expr_wrapper);
                }

                None // FIXME?
            },
            FnCall(ref fn_name, ref args) => None, // FIXME: Compare to fn declaration?
            FnDecl(ref fn_name, ref args, ref mut ret_type, ref mut body_expr_wrapper) => {
                for &(ref name, ref type_) in args {
                    if let Err(()) = type_.parse::<Types>() {
                        // TODO: Custom type found. Figure out if it is valid
                    }
                }

                // TODO: Check if all return values return proper type
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
                let lhs_type = match self.analyze(lhs_expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // VERIFY, unwrap?
                };
                let rhs_type = match self.analyze(rhs_expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // VERIFY, unwrap?
                };

                TypeChecker::cmp_lhs_rhs(lhs_type, rhs_type)
            },
            Literal(ref literal) => Some(literal.to_string()), // Done?
            Return(ref mut opt_ret_type) => match *opt_ret_type { // Done?
                Some(ref mut expr_wrapper) => self.analyze(expr_wrapper),
                None => Some("None".into()) // None type?
            },
            UnaryOp(ref op, ref mut expr_wrapper) => self.analyze(expr_wrapper),
            Var(ref name) => None, // FIXME: Lookup VarDecl type
            VarDecl(ref const_, ref name, ref mut opt_type, ref mut expr_wrapper) => {
                let rhs_type = match self.analyze(expr_wrapper) {
                    Some(t) => t,
                    None => unreachable!("This should not happen??") // VERIFY, unwrap?
                };

                println!("Var decl rhs type: {:?}", rhs_type);

                match *opt_type {
                    Some(ref lhs_type) => TypeChecker::cmp_lhs_rhs(lhs_type.clone(), rhs_type),  // No way to not clone?
                    None => {
                        *opt_type = Some(rhs_type); // Done?

                        None
                    }
                    // REVIEW: The above was *opt_type = Some("i32".parse().unwrap())
                    // Codegen doesn't seem to care what the actual type is.
                    // Does that matter if type checker does its job correctly?
                }
            },
            WhileLoop(ref mut cond_expr_wrapper, ref mut body_expr_wrapper) => {
                if self.analyze(cond_expr_wrapper) != Some("bool".into()) {
                    panic!("Error goes here, not a bool") // FIXME: Better errors
                }

                // Type of a while loop is the type it returns? REVIEW
                self.analyze(body_expr_wrapper)

                // TypeChecker::cmp_lhs_rhs(lhs_type, rhs_type)
            },
            NoOp => None,
        }
    }
}

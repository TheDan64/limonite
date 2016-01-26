use std::fmt;

use lexical::tokens::Tokens;
use syntax::literals::*;
use syntax::op::*;

#[derive(PartialEq)]
pub struct ExprWrapper {
    expr: Box<Expr>,
    start_line: u64,
    start_column: u64,
    end_line: u64,
    end_column: u64
}

impl ExprWrapper {
    // Create an associated expression with start and end positions
    pub fn new(expr: Expr, startl: u64, startc: u64, endl: u64, endc: u64) -> ExprWrapper {
        ExprWrapper {
            expr: Box::new(expr),
            start_line: startl,
            start_column: startc,
            end_line: endl,
            end_column: endc
        }
    }

    pub fn default(expr: Expr) -> ExprWrapper {
        ExprWrapper {
            expr: Box::new(expr),
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
        }
    }

    pub fn get_expr(&self) -> &Expr {
        &self.expr
    }

    pub fn get_mut_expr(&mut self) -> &mut Expr {
        &mut self.expr
    }
}

impl fmt::Debug for ExprWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Operations between two expressions
    InfixOp(InfixOp, ExprWrapper, ExprWrapper),
    // Operation on a single expression
    UnaryOp(UnaryOp, ExprWrapper),
    // Literals such as numbers and strings
    Literal(Literals),
    // Run expression while conditional is true
    WhileLoop(ExprWrapper, ExprWrapper),
    // If condition true, run expression, optional elif, else
    If(ExprWrapper, ExprWrapper, Option<ExprWrapper>),
    // Assign an expression to an existing variable
    Assign(ExprWrapper, ExprWrapper),
    // Fn call with name and args.
    FnCall(String, Vec<ExprWrapper>),
    // Declare a function with a name, args(name, ident), return ident, and body expr
    FnDecl(String, Vec<(String, Tokens)>, Tokens, ExprWrapper),
    // Run consecutive expressions
    Block(Vec<ExprWrapper>),
    // Const declaration?, variable name, type(optional in parser but not SA), and expression
    VarDecl(bool, String, Option<String>, ExprWrapper),
    // Reference to a value in a variable
    Var(String),
    // Return an expression from a function
    Return(Option<ExprWrapper>),
    // Does nothing
    NoOp,
}

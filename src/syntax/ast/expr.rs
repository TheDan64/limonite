use syntax::ast::consts::*;
use syntax::ast::op::*;
use syntax::core::tokens::Token;

pub trait CodeGen {
    // Not sure if this should return anything
    fn gen_code(&self);
}

#[derive(Debug, PartialEq)]
pub struct ExprWrapper {
    expr: Box<Expr>,
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize
}

impl CodeGen for ExprWrapper {
    fn gen_code(&self) {
        self.expr.gen_code();
    }
}

impl ExprWrapper {
    // Create an associated expression with start and end positions
    pub fn new(expr: Box<Expr>, startl: usize, startc: usize, endl: usize, endc: usize) -> ExprWrapper {
        ExprWrapper {
            expr: expr,
            start_line: startl,
            start_column: startc,
            end_line: endl,
            end_column: endc
        }
    }

    pub fn default(expr: Box<Expr>) -> ExprWrapper {
        ExprWrapper {
            expr: expr,
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
        }
    }

    pub fn get_expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Operations between two expressions
    InfixOp(InfixOp, ExprWrapper, ExprWrapper),
    // Operation on a single expression
    UnaryOp(UnaryOp, ExprWrapper),
    // Constants such as numbers and strings
    Const(Const),
    // Run expression while conditional is true
    WhileLoop(ExprWrapper, ExprWrapper),
    // If condition true, run expression, optional elif, else
    If(ExprWrapper, ExprWrapper, Option<ExprWrapper>),
    // Assign a value to an expression
    Assign(ExprWrapper, ExprWrapper),
    // Fn call with args.
    // ToDo: Vec<Option<ExprWrapper>> for optional args?
    FnCall(ExprWrapper, Vec<ExprWrapper>),
    // Declare a function with a name, args(name, type | ident), return (type | ident), and expr
    // ToDo: Optional args
    FnDecl(String, Vec<(String, Token)>, Token, ExprWrapper),
    // Run consecutive expressions
    Block(Vec<ExprWrapper>),
    // Variable name and expression.
    // ToDo: Does this need a variable type here?
    // ToDo: A bool for whether it is const(def) or not(var)?
    VarDecl(Vec<(String, ExprWrapper)>),
    // Reference to a value in an identifier
    Ident(String),
    // Return an expression from a function
    Return(Option<ExprWrapper>),
    // A lot more to come
    NoOp,
}

impl CodeGen for Expr {
    fn gen_code(&self) {
        match *self {
            Expr::Block(ref vec) => {
                for expr in vec {
                    expr.gen_code();
                }
            },
            // Codegen for others:
            _ => ()
        }
    }
}

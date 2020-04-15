use crate::span::Spanned;
use crate::syntax::{Block, InfixOp, Literal, UnaryOp};

pub type Expr<'s> = Spanned<Box<ExprKind<'s>>>;

#[derive(Debug, PartialEq)]
pub enum ExprKind<'s> {
    // Operations between two expressions
    InfixOp(InfixOp, Expr<'s>, Expr<'s>),
    // Operation on a single expression
    UnaryOp(UnaryOp, Expr<'s>),
    // Literals such as numbers and strings
    Literal(Literal<'s>),
    // While conditional is true, run expression
    // WhileLoop(Expr<'s>, Expr<'s>),
    // If condition true, run expression, optional elif, else
    // If(ExprWrapper, ExprWrapper, Option<ExprWrapper>),
    // Assign an expression to an existing variable
    // Assign(ExprWrapper, ExprWrapper), // REVIEW: Shouldn't the first param just be a string?
    // Fn call with name and args.
    FnCall(Spanned<&'s str>, Vec<Expr<'s>>),
    // Declare a function with a name, args(name, type), return type, and body expr
    // FnDecl(String, Vec<(String, String)>, Option<String>, ExprWrapper),
    // Consecutive Stmts
    Block(Block<'s>),
    // Const declaration?, variable name, type(optional in parser but not SA), and expression
    VarDecl(bool, &'s str, Option<&'s str>, Expr<'s>),
    // Reference to a value in a variable
    Var(&'s str),
    // Return an expression from a function
    Return(Option<Expr<'s>>),
    // Does nothing
    NoOp,
}

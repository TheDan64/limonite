use crate::span::Spanned;
use crate::syntax::{Block, InfixOp, Literal, UnaryOp};

pub type Expr<'s> = Spanned<Box<ExprKind<'s>>>;

// Should Return be a Stmt instead? Or do we want Rust-like Expr Returns?

#[derive(Debug, PartialEq)]
pub enum ExprKind<'s> {
    // Operations between two expressions
    InfixOp(Spanned<InfixOp>, Expr<'s>, Expr<'s>),
    // Operation on a single expression
    UnaryOp(Spanned<UnaryOp>, Expr<'s>),
    // Literals such as numbers and strings
    Literal(Literal<'s>),
    // While conditional is true, run block
    WhileLoop(Expr<'s>, Block<'s>),
    // If condition true, run block, optional elif, else
    If(Expr<'s>, Block<'s>, Option<Expr<'s>>),
    // Assign an expression to an existing variable
    Assign(Spanned<&'s str>, Expr<'s>),
    // Fn call with name and args.
    FnCall(Spanned<&'s str>, Vec<Expr<'s>>),
    // Consecutive Stmts
    Block(Block<'s>),
    // Reference to a value in a variable
    Var(&'s str),
    // Return an expression from a function
    Return(Option<Expr<'s>>),
    // Does nothing
    NoOp,
}

use syntax::ast::consts::*;
use syntax::ast::op::*;

#[derive(Debug, PartialEq)]
pub enum Expr {
	// Operations between two expressions
	NumOpExpr(NumOp, Box<Expr>, Box<Expr>),
	// Operation on a single expression
	UnaryOpExpr(UnaryOp, Box<Expr>),
	// Constants such as numbers and strings
	ConstExpr(Const),
	// Run expression while conditional is true
	WhileLoopExpr(Box<Expr>, Box<Expr>),
	// Run expression if condition true, optional elif, else
	IfExpr(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
	// Assign a value to an expression
	AssignExpr(Box<Expr>, Box<Expr>),
	// Fn call with args.
	// ToDo: Vec<Option<Expr>> for optional args?
	FnCallExpr(Box<Expr>, Vec<Expr>),
	// Decale a function with a name, args, and expr
	// ToDo: Have a Vec of a struct for optional arg values?
	// ToDo: Does return type go here?
	FnDeclExpr(String, Vec<String>, Box<Expr>),
	// Run consecutive expressions
	BlockExpr(Vec<Expr>),
	// Variable name and expression.
	// ToDo: Does this need a variable type here?
	// ToDo: A bool for whether it is const(def) or not(var)?
	VarDeclExpr(Vec<(String, Expr)>),
	// Reference to a value in an identifier
	IdentExpr(String)

	// A lot more to come
}

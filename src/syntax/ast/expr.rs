use syntax::ast::consts::*;
use syntax::ast::op::*;

#[derive(Debug, PartialEq)]
pub struct ExprWrapper {
	expr: Expr,
	start_line: usize,
	start_column: usize,
	end_line: usize,
	end_column: usize
}

impl ExprWrapper {
	// Create an associated expression with start and end positions
	pub fn new(expr: Expr, startl: usize, startc: usize, endl: usize, endc: usize) -> ExprWrapper {
		ExprWrapper {
			expr: expr,
			start_line: startl,
			start_column: startc,
			end_line: endl,
			end_column: endc
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum Expr {
	// Operations between two expressions
	NumOpExpr(NumOp, Box<ExprWrapper>, Box<ExprWrapper>),
	// Operation on a single expression
	UnaryOpExpr(UnaryOp, Box<ExprWrapper>),
	// Constants such as numbers and strings
	ConstExpr(Const),
	// Run expression while conditional is true
	WhileLoopExpr(Box<ExprWrapper>, Box<ExprWrapper>),
	// Run expression if condition true, optional elif, else
	IfExpr(Box<ExprWrapper>, Box<ExprWrapper>, Option<Box<ExprWrapper>>),
	// Assign a value to an expression
	AssignExpr(Box<ExprWrapper>, Box<ExprWrapper>),
	// Fn call with args.
	// ToDo: Vec<Option<ExprWrapper>> for optional args?
	FnCallExpr(Box<ExprWrapper>, Vec<ExprWrapper>),
	// Declare a function with a name, args, and expr
	// ToDo: Have a Vec of a struct for optional arg values?
	// ToDo: Does return type go here?
	FnDeclExpr(String, Vec<String>, Box<ExprWrapper>),
	// Run consecutive expressions
	BlockExpr(Vec<ExprWrapper>),
	// Variable name and expression.
	// ToDo: Does this need a variable type here?
	// ToDo: A bool for whether it is const(def) or not(var)?
	VarDeclExpr(Vec<(String, ExprWrapper)>),
	// Reference to a value in an identifier
	IdentExpr(String)

	// A lot more to come
}

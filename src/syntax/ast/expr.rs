use syntax::ast::consts::*;
use syntax::ast::op::*;

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

	pub fn get_expr(&mut self) -> &mut Expr {
		&mut *self.expr
	}
}

#[derive(Debug, PartialEq)]
pub enum Expr {
	// Operations between two expressions
	NumOpExpr(NumOp, ExprWrapper, ExprWrapper),
	// Operation on a single expression
	UnaryOpExpr(UnaryOp, ExprWrapper),
	// Constants such as numbers and strings
	ConstExpr(Const),
	// Run expression while conditional is true
	WhileLoopExpr(ExprWrapper, ExprWrapper),
	// Run expression if condition true, optional elif, else
	IfExpr(ExprWrapper, ExprWrapper, Option<ExprWrapper>),
	// Assign a value to an expression
	AssignExpr(ExprWrapper, ExprWrapper),
	// Fn call with args.
	// ToDo: Vec<Option<ExprWrapper>> for optional args?
	FnCallExpr(ExprWrapper, Vec<ExprWrapper>),
	// Declare a function with a name, args, and expr
	// ToDo: Have a Vec of a struct for optional arg values?
	// ToDo: Does return type go here?
	FnDeclExpr(String, Vec<String>, ExprWrapper),
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

impl CodeGen for Expr {
	fn gen_code(&self) {
		match *self {
			Expr::BlockExpr(ref vec) => {
				for expr in vec {
					expr.gen_code();
				}
			},
			// Codegen for others:
			_ => ()
		}
	}
}
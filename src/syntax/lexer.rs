#![allow(dead_code)]
#![feature(globs)]

extern crate keywords;

pub enum Token {
	// True, False
	BooleanLiteral(bool),

	// Variables, fn names
	Identifier(String),

	// Reserved words
	Keyword(keywords::Keyword),

	// >> Comments. Eventually multiline as well
	Comment(String),

	// End of file
	EOF
}



pub struct Lexer {
	pub tokens : Vec<Token>,
	line_number : uint,
	column_number : uint
}



// Temporary main
fn main() {
	
}
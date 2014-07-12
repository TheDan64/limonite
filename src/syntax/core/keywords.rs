// keywords.rs
// based on code from TomBebbington's js.rs
// https://github.com/TomBebbington/js.rs

//use std::fmt::{Formatter, Result, Show};
use std::from_str::FromStr;

pub enum Keyword {
	And,
	Or,
	Not,
	Is,
	Class,
	Fn,
	Var,
	Def,
	Assert,
	Else,
	If,
	For,
	While,
	Return,
	Equals,
	Use,
	From,
	As,
	When,
	Throws,
	Break,
	Continue,
	Pass,
	
	// Print is temporaily a statement until we have functions
	Print 
}

impl FromStr for Keyword {
	fn from_str(s: &str) -> Option<Keyword> {
		match s {
			"and"      => Some(And),
			"or"       => Some(Or),
			"not"      => Some(Not),
			"is"       => Some(Is),
			"class"    => Some(Class),
			"fn"       => Some(Fn),
			"var"      => Some(Var),
			"def"      => Some(Def),
			"assert"   => Some(Assert),
			"else"     => Some(Else),
			"if"       => Some(If),
			"for"      => Some(For),
			"while"    => Some(While),
			"return"   => Some(Return),
			"equals"   => Some(Equals),
			"use"      => Some(Use),
			"from"     => Some(From),
			"as"       => Some(As),
			"when"     => Some(When),
			"throws"   => Some(Throws),
			"break"    => Some(Break),
			"continue" => Some(Continue),
			"pass"     => Some(Pass),
			"print"    => Some(Print),
			_          => None
		}
	}
}
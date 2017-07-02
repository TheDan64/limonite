extern crate limonite;

use limonite::lexical::keywords::Keywords::{Def, Function, If, Is, Return, Var};
use limonite::lexical::symbols::Symbols::{Comma, Equals, ParenClose, ParenOpen, PlusEquals, RightThinArrow};
use limonite::lexical::tokens::Tokens;
use limonite::lexical::tokens::Tokens::{BoolLiteral, CharLiteral, Comment, EOF, Error, Identifier, Indent, Keyword, Numeric, Symbol, StrLiteral};
use limonite::lexical::types::Types::{Float32Bit, Float64Bit, Int32Bit, Int64Bit, UInt8Bit, UInt32Bit, UInt64Bit, Float128Bit, UInt128Bit};
use limonite::lexical::lexer::{Lexer};

fn cmp_tokens(mut lexer: Lexer, vec: Vec<Tokens>) {
    for desired_tok in vec.iter() {
        let tok = lexer.next();
        if let Some(tok) = tok {
            if tok == *desired_tok {
                continue;
            }
            panic!(format!("Unexpected token `{0:?}` found. Expected `{1:?}`.", tok, desired_tok));
        }
    }

    let tok = lexer.next();
    if let Some(tok) = tok {
        if !tok.expect(EOF) {
            panic!(format!("Unexpected token `{:?}` found. Expected `EOF`", tok));
        }
    }
}

#[test]
fn test_hello_world() {
    let input_string = "\
>> Hello World!

print(\"Hello World!\")";

    let lexer = Lexer::new(&input_string);
    let desired_output = vec![Comment(" Hello World!".to_string()), Indent(0),
                              Indent(0),
                              Identifier("print".to_string()),
                              Symbol(ParenOpen),
                              StrLiteral("Hello World!".to_string()),
                              Symbol(ParenClose),
                              EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_indentation() {
    let input_string = "\
>>>
    Test of indentation and a few keywords.
<<<

if True,
	func()

	if False,
		func2()";

    let lexer = Lexer::new(&input_string);
    let desired_output = vec![Comment("\n    Test of indentation and a few keywords.\n".to_string()), Indent(0),
                              Indent(0),
                              Keyword(If), BoolLiteral(true), Symbol(Comma), Indent(1),
                              Identifier("func".to_string()), Symbol(ParenOpen), Symbol(ParenClose), Indent(0),
                              Indent(1),
                              Keyword(If), BoolLiteral(false), Symbol(Comma), Indent(2),
                              Identifier("func2".to_string()), Symbol(ParenOpen), Symbol(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_valid_numerics() {
    // REVIEW: is f32/64 a valid suffix without decimal place? Thinking yes. Check rust
    let input_string = "\
0xF3a
0xfffi32
0xfffi64
0xfffu32
0xfffu64
0b111
0b101i32
0b101i64
0b101u32
0b101u64
42
42i32
42i64
42u32
42u64
42.0
42.0f32
42.0f64
0xFFFF_FFFF
0b0101_0101
400_000
400_000.000_000
0f32
0001
0000_0000
0000_0001u8
1.0f128
117u128";

    let lexer = Lexer::new(&input_string);
    let desired_output = vec![Numeric("0xF3a".to_string(), None), Indent(0),
                              Numeric("0xfff".to_string(), Some(Int32Bit)), Indent(0),
                              Numeric("0xfff".to_string(), Some(Int64Bit)), Indent(0),
                              Numeric("0xfff".to_string(), Some(UInt32Bit)), Indent(0),
                              Numeric("0xfff".to_string(), Some(UInt64Bit)), Indent(0),
                              Numeric("0b111".to_string(), None), Indent(0),
                              Numeric("0b101".to_string(), Some(Int32Bit)), Indent(0),
                              Numeric("0b101".to_string(), Some(Int64Bit)), Indent(0),
                              Numeric("0b101".to_string(), Some(UInt32Bit)), Indent(0),
                              Numeric("0b101".to_string(), Some(UInt64Bit)), Indent(0),
                              Numeric("42".to_string(), None), Indent(0),
                              Numeric("42".to_string(), Some(Int32Bit)), Indent(0),
                              Numeric("42".to_string(), Some(Int64Bit)), Indent(0),
                              Numeric("42".to_string(), Some(UInt32Bit)), Indent(0),
                              Numeric("42".to_string(), Some(UInt64Bit)), Indent(0),
                              Numeric("42.0".to_string(), None), Indent(0),
                              Numeric("42.0".to_string(), Some(Float32Bit)), Indent(0),
                              Numeric("42.0".to_string(), Some(Float64Bit)), Indent(0),
                              Numeric("0xFFFF_FFFF".to_string(), None), Indent(0),
                              Numeric("0b0101_0101".to_string(), None), Indent(0),
                              Numeric("400_000".to_string(), None), Indent(0),
                              Numeric("400_000.000_000".to_string(), None), Indent(0),
                              Numeric("0".to_string(), Some(Float32Bit)), Indent(0),
                              Numeric("0001".to_string(), None), Indent(0),
                              Numeric("0000_0000".to_string(), None), Indent(0),
                              Numeric("0000_0001".to_string(), Some(UInt8Bit)), Indent(0),
                              Numeric("1.0".to_string(), Some(Float128Bit)), Indent(0),
                              Numeric("117".to_string(), Some(UInt128Bit)), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_invalid_numerics() {
    let input_string = "\
0x
0xz
0xfz
0xfi3
0xfi31
0xfi6
0xfi63
0b
0ba
0b1a
0b1f
42i3
42i31
42.
42.0f
42.0f3
42.0f31
42.0f8
42.0f16";

    let lexer = Lexer::new(&input_string);
    let desired_output = vec![Error("No hexadecimal value was found.".to_string()), Indent(0),
                              Error("No hexadecimal value was found.".to_string()), Identifier("z".to_string()), Indent(0),
                              Error("Invalid suffix z. Did you mean u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, or f128?".to_string()), Indent(0),
                              Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i6. Did you mean i64?".to_string()), Indent(0),
                              Error("Invalid suffix i63. Did you mean i64?".to_string()), Indent(0),
                              // Error("Invalid suffix u8. Did you mean u32 or u64?".to_string()), Indent(0),
                              Error("No binary value was found.".to_string()), Indent(0),
                              Error("No binary value was found.".to_string()), Identifier("a".to_string()), Indent(0),
                              Error("Invalid suffix a. Did you mean u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, or f128?".to_string()), Indent(0),
                              Error("Invalid suffix f. Did you mean f32, f64, or f128?".to_string()), Indent(0),
                              // Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Numeric("32".to_string(), None), Indent(0),
                              Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
                              Error("No numbers found after the decimal point.".to_string()), Indent(0),
                              Error("Invalid suffix f. Did you mean f32, f64, or f128?".to_string()), Indent(0),
                              Error("Invalid suffix f3. Did you mean f32?".to_string()), Indent(0),
                              Error("Invalid suffix f31. Did you mean f32?".to_string()), Indent(0),
                              Error("Invalid suffix f8. Did you mean f32, f64, or f128?".to_string()), Indent(0),
                              Error("Invalid suffix f16. Did you mean f128?".to_string()), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_functions() {
    let input_string = "\
fn basic_func() -> str
	def ch = 'g'
	var string = \"strin\"

	if ch is 'g',
		string += ch

	return string";

    let lexer = Lexer::new(&input_string);
    let desired_output = vec![Keyword(Function), Identifier("basic_func".to_string()), Symbol(ParenOpen), Symbol(ParenClose),
                              Symbol(RightThinArrow), Identifier("str".to_string()), Indent(1),
                              Keyword(Def), Identifier("ch".to_string()), Symbol(Equals), CharLiteral('g'), Indent(1),
                              Keyword(Var), Identifier("string".to_string()), Symbol(Equals), StrLiteral("strin".to_string()), Indent(0),
                              Indent(1),
                              Keyword(If), Identifier("ch".to_string()), Keyword(Is), CharLiteral('g'), Symbol(Comma), Indent(2),
                              Identifier("string".to_string()), Symbol(PlusEquals), Identifier("ch".to_string()), Indent(0),
                              Indent(1),
                              Keyword(Return), Identifier("string".to_string()), EOF];

    cmp_tokens(lexer, desired_output);
}

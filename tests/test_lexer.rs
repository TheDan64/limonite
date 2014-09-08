extern crate limonite;

use std::io::File;
use std::io::BufferedReader;
use limonite::syntax::lexer::{Lexer, Tokenizer};
use limonite::syntax::core::tokens::{Token, Comment, Error, Keyword, Identifier, Indent, Numeric, Punctuation, BoolLiteral, StrLiteral, EOF};
use limonite::syntax::core::keywords::{If, Print};
use limonite::syntax::core::types::{Int32Bit, Int64Bit, UInt32Bit, UInt64Bit, Float32Bit, Float64Bit};
use limonite::syntax::core::punctuation::{Comma, ParenOpen, ParenClose};

fn cmp_tokens(mut lexer: Lexer, vec: Vec<Token>) {
    let mut tok: Token;

    for desired_tok in vec.iter() {
        tok = lexer.get_tok();
 
        if tok == *desired_tok { continue; }

        fail!(format!("Unexpected token `{0}` found. Expected `{1}`.", tok, desired_tok));
    }

    tok = lexer.get_tok();

    if tok != EOF {
        fail!(format!("Unexpected token `{}` found. Expected `EOF`", tok));
    }
}

#[test]
fn test_hello_world() {
    let path = Path::new("tests/lang/test_hello_world.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    let desired_output = vec![Comment(" Hello World!".to_string()), Indent(0),
                              Indent(0),
                              Keyword(Print), Punctuation(ParenOpen), StrLiteral("Hello World!".to_string()), Punctuation(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_indentation() {
    let path = Path::new("tests/lang/test_indentation.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    let desired_output = vec![Comment("\n    Test of indentation and a few keywords.\n".to_string()), Indent(0),
                              Indent(0),
                              Keyword(If), BoolLiteral(true), Punctuation(Comma), Indent(1),
                              Identifier("func".to_string()), Punctuation(ParenOpen), Punctuation(ParenClose), Indent(0),
                              Indent(1),
                              Keyword(If), BoolLiteral(false), Punctuation(Comma), Indent(2),
                              Identifier("func2".to_string()), Punctuation(ParenOpen), Punctuation(ParenClose), EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_numerics() {
    let path = Path::new("tests/lang/test_numerics.lim");
    let file = BufferedReader::new(File::open(&path));
    let lexer = Lexer::new(file);
    let desired_output = vec![Comment(" Valid".to_string()), Indent(0),
                              Numeric("0xF3a".to_string(), None), Indent(0),
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
                              Indent(0),
                              Comment(" Invalid".to_string()), Indent(0),
                              Error("No hexadecimal value was found.".to_string()), Indent(0),
                              Error("No hexadecimal value was found.".to_string()), Identifier("z".to_string()), Indent(0),
                              Error("Invalid suffix z. Did you mean u32, u64, i32, or i64?".to_string()), Identifier("z".to_string()), Indent(0),
                              Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i6. Did you mean i64?".to_string()), Indent(0),
                              Error("Invalid suffix i63. Did you mean i64?".to_string()), Indent(0),
                              Error("Invalid suffix u8. Did you mean u32 or u64?".to_string()), Indent(0),
                              Error("No binary value was found.".to_string()), Indent(0),
                              Error("No binary value was found.".to_string()), Identifier("a".to_string()), Indent(0),
                              Error("Invalid suffix a. Did you mean u32, u64, i32, or i64?".to_string()), Indent(0),
                              Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Indent(0),
                              Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Numeric("32".to_string(), None), Indent(0),
                              Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
                              Error("Invalid suffix f. Did you mean f32 or f64?".to_string()), Indent(0),
                              Error("Invalid suffix f3. Did you mean f32?".to_string()), Indent(0),
                              Error("Invalid suffix f31. Did you mean f32?".to_string()), EOF];

    cmp_tokens(lexer, desired_output);
}
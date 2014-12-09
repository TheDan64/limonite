extern crate limonite;

use std::io::File;
use std::io::BufferedReader;
use limonite::syntax::lexer::{Lexer, Tokenizer};
use limonite::syntax::core::tokens::Token;
use limonite::syntax::core::keywords::Keywords;
use limonite::syntax::core::types::Types;
use limonite::syntax::core::punctuation::Punctuations;

fn cmp_tokens(mut lexer: Lexer, vec: Vec<Token>) {
    let mut tok: Token;

    for desired_tok in vec.iter() {
        tok = lexer.get_tok();
 
        if tok == *desired_tok { continue; }

        panic!(format!("Unexpected token `{0}` found. Expected `{1}`.", tok, desired_tok));
    }

    tok = lexer.get_tok();

    if tok != Token::EOF {
        panic!(format!("Unexpected token `{}` found. Expected `EOF`", tok));
    }
}

#[test]
fn test_hello_world() {
    let path = Path::new("tests/lang/test_hello_world.lim");
    let input_string = BufferedReader::new(File::open(&path)).read_to_string().unwrap();
    let lexer = Lexer::new(input_string.as_slice());
    let desired_output = vec![Token::Comment(" Hello World!".to_string()), Token::Indent(0),
                              Token::Indent(0),
                              Token::Keyword(Keywords::Print), Token::Punctuation(Punctuations::ParenOpen), Token::StrLiteral("Hello World!".to_string()), Token::Punctuation(Punctuations::ParenClose), Token::EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_indentation() {
    let path = Path::new("tests/lang/test_indentation.lim");
    let input_string = BufferedReader::new(File::open(&path)).read_to_string().unwrap();
    let lexer = Lexer::new(input_string.as_slice());
    let desired_output = vec![Token::Comment("\n    Test of indentation and a few keywords.\n".to_string()), Token::Indent(0),
                              Token::Indent(0),
                              Token::Keyword(Keywords::If), Token::BoolLiteral(true), Token::Punctuation(Punctuations::Comma), Token::Indent(1),
                              Token::Identifier("func".to_string()), Token::Punctuation(Punctuations::ParenOpen), Token::Punctuation(Punctuations::ParenClose), Token::Indent(0),
                              Token::Indent(1),
                              Token::Keyword(Keywords::If), Token::BoolLiteral(false), Token::Punctuation(Punctuations::Comma), Token::Indent(2),
                              Token::Identifier("func2".to_string()), Token::Punctuation(Punctuations::ParenOpen), Token::Punctuation(Punctuations::ParenClose), Token::EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_numerics() {
    let path = Path::new("tests/lang/test_numerics.lim");
    let input_string = BufferedReader::new(File::open(&path)).read_to_string().unwrap();
    let lexer = Lexer::new(input_string.as_slice());
    let desired_output = vec![Token::Comment(" Valid".to_string()), Token::Indent(0),
                              Token::Numeric("0xF3a".to_string(), None), Token::Indent(0),
                              Token::Numeric("0xfff".to_string(), Some(Types::Int32Bit)), Token::Indent(0),
                              Token::Numeric("0xfff".to_string(), Some(Types::Int64Bit)), Token::Indent(0),
                              Token::Numeric("0xfff".to_string(), Some(Types::UInt32Bit)), Token::Indent(0),
                              Token::Numeric("0xfff".to_string(), Some(Types::UInt64Bit)), Token::Indent(0),
                              Token::Numeric("0b111".to_string(), None), Token::Indent(0),
                              Token::Numeric("0b101".to_string(), Some(Types::Int32Bit)), Token::Indent(0),
                              Token::Numeric("0b101".to_string(), Some(Types::Int64Bit)), Token::Indent(0),
                              Token::Numeric("0b101".to_string(), Some(Types::UInt32Bit)), Token::Indent(0),
                              Token::Numeric("0b101".to_string(), Some(Types::UInt64Bit)), Token::Indent(0),
                              Token::Numeric("42".to_string(), None), Token::Indent(0),
                              Token::Numeric("42".to_string(), Some(Types::Int32Bit)), Token::Indent(0),
                              Token::Numeric("42".to_string(), Some(Types::Int64Bit)), Token::Indent(0),
                              Token::Numeric("42".to_string(), Some(Types::UInt32Bit)), Token::Indent(0),
                              Token::Numeric("42".to_string(), Some(Types::UInt64Bit)), Token::Indent(0),
                              Token::Numeric("42.0".to_string(), None), Token::Indent(0),
                              Token::Numeric("42.0".to_string(), Some(Types::Float32Bit)), Token::Indent(0),
                              Token::Numeric("42.0".to_string(), Some(Types::Float64Bit)), Token::Indent(0),
                              Token::Numeric("0xFFFF_FFFF".to_string(), None), Token::Indent(0),
                              Token::Numeric("0b0101_0101".to_string(), None), Token::Indent(0),
                              Token::Numeric("400_000".to_string(), None), Token::Indent(0),
                              Token::Numeric("400_000.000_000".to_string(), None), Token::Indent(0),
                              Token::Indent(0),
                              Token::Comment(" Invalid".to_string()), Token::Indent(0),
                              Token::Error("No hexadecimal value was found.".to_string()), Token::Indent(0),
                              Token::Error("No hexadecimal value was found.".to_string()), Token::Identifier("z".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix z. Did you mean u32, u64, i32, or i64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix i3. Did you mean i32?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix i31. Did you mean i32?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix i6. Did you mean i64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix i63. Did you mean i64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix u8. Did you mean u32 or u64?".to_string()), Token::Indent(0),
                              Token::Error("No binary value was found.".to_string()), Token::Indent(0),
                              Token::Error("No binary value was found.".to_string()), Token::Identifier("a".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix a. Did you mean u32, u64, i32, or i64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Token::Numeric("32".to_string(), None), Token::Indent(0),
                              Token::Error("Invalid suffix i3. Did you mean i32?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix i31. Did you mean i32?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix f. Did you mean f32 or f64?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix f3. Did you mean f32?".to_string()), Token::Indent(0),
                              Token::Error("Invalid suffix f31. Did you mean f32?".to_string()), Token::EOF];

    cmp_tokens(lexer, desired_output);
}

#[test]
fn test_functions() {
    let path = Path::new("tests/lang/test_functions.lim");
    let input_string = BufferedReader::new(File::open(&path)).read_to_string().unwrap();
    let lexer = Lexer::new(input_string.as_slice());
    let desired_output = vec![Token::Keyword(Keywords::Fn), Token::Identifier("basic_func".to_string()), Token::Punctuation(Punctuations::ParenOpen), Token::Punctuation(Punctuations::ParenClose),
                              Token::Punctuation(Punctuations::RightThinArrow), Token::Type(Types::Str), Token::Indent(1),
                              Token::Keyword(Keywords::Def), Token::Identifier("ch".to_string()), Token::Punctuation(Punctuations::Equals), Token::CharLiteral('g'), Token::Indent(1),
                              Token::Keyword(Keywords::Var), Token::Identifier("string".to_string()), Token::Punctuation(Punctuations::Equals), Token::StrLiteral("strin".to_string()), Token::Indent(0),
                              Token::Indent(1),
                              Token::Keyword(Keywords::If), Token::Identifier("ch".to_string()), Token::Keyword(Keywords::Is), Token::CharLiteral('g'), Token::Punctuation(Punctuations::Comma), Token::Indent(2),
                              Token::Identifier("string".to_string()), Token::Punctuation(Punctuations::PlusEquals), Token::Identifier("ch".to_string()), Token::Indent(0), 
                              Token::Indent(1),
                              Token::Keyword(Keywords::Return), Token::Identifier("string".to_string()), Token::EOF];

    cmp_tokens(lexer, desired_output);
}

use limonite::lexical::keywords::Keyword::If;
use limonite::interner::StrId;
use limonite::lexical::Symbol::{Comma, ParenClose, ParenOpen};
use limonite::lexical::{CommentKind, Lexer, LexerError, TokenKind::{self, *}, TokenResult};

fn cmp_tokens<'s>(mut lexer: Lexer<'s>, tokens: &[TokenResult<'s>], skip_indents: bool) {
    let mut iter: &mut dyn Iterator<Item=TokenResult<'s>> = &mut lexer;
    let mut filter;

    if skip_indents {
        filter = iter.filter(|tok_res| match tok_res {
            Ok(t) => !matches!(t.node(), TokenKind::Indent(_)),
            Err(_) => true,
        });
        iter = &mut filter;
    }

    for desired_tok in tokens {
        let tok = iter.next();

        assert_eq!(tok, Some(*desired_tok));
    }
}

#[test]
fn test_hello_world() {
    let input_string = "\
>> Hello World!

print(\"Hello World!\")";

    let lexer = Lexer::new(&input_string, StrId::DUMMY);
    let desired_output = vec![
        Ok(span!(Comment(CommentKind::Single(span!(" Hello World!", 2, 14))), 0, 14)),
        Ok(span!(Indent(0), 15, 15)),
        Ok(span!(Indent(0), 16, 16)),
        Ok(span!(Identifier("print"), 17, 21)),
        Ok(span!(Symbol(ParenOpen), 22, 22)),
        Ok(span!(StrLiteral("\"Hello World!\""), 23, 36)),
        Ok(span!(Symbol(ParenClose), 37, 37)),
    ];

    cmp_tokens(lexer, &desired_output, false);
}

#[test]
fn test_indentation() {
    let input_string = "
if True,
	func()

	if False,
		func2()";

    let lexer = Lexer::new(&input_string, StrId::DUMMY);
    let desired_output = vec![
        Ok(span!(Indent(0), 0, 0)),
        Ok(span!(Keyword(If), 1, 2)),
        Ok(span!(BoolLiteral(true), 4, 7)),
        Ok(span!(Symbol(Comma), 8, 8)),
        Ok(span!(Indent(1), 9, 10)),
        Ok(span!(Identifier("func"), 11, 14)),
        Ok(span!(Symbol(ParenOpen), 15, 15)),
        Ok(span!(Symbol(ParenClose), 16, 16)),
        Ok(span!(Indent(0), 17, 17)),
        Ok(span!(Indent(1), 18, 19)),
        Ok(span!(Keyword(If), 20, 21)),
        Ok(span!(BoolLiteral(false), 23, 27)),
        Ok(span!(Symbol(Comma), 28, 28)),
        Ok(span!(Indent(2), 29, 31)),
        Ok(span!(Identifier("func2"), 32, 36)),
        Ok(span!(Symbol(ParenOpen), 37, 37)),
        Ok(span!(Symbol(ParenClose), 38, 38)),
    ];

    cmp_tokens(lexer, &desired_output, false);

    assert_eq!(&input_string[desired_output[0].unwrap().span()], "\n");
    assert_eq!(&input_string[desired_output[1].unwrap().span()], "if");
    assert_eq!(&input_string[desired_output[2].unwrap().span()], "True");
    assert_eq!(&input_string[desired_output[3].unwrap().span()], ",");
    assert_eq!(&input_string[desired_output[4].unwrap().span()], "\n	");
    assert_eq!(&input_string[desired_output[5].unwrap().span()], "func");
    assert_eq!(&input_string[desired_output[6].unwrap().span()], "(");
    assert_eq!(&input_string[desired_output[7].unwrap().span()], ")");
    assert_eq!(&input_string[desired_output[8].unwrap().span()], "\n");
    assert_eq!(&input_string[desired_output[9].unwrap().span()], "\n	");
    assert_eq!(&input_string[desired_output[10].unwrap().span()], "if");
    assert_eq!(&input_string[desired_output[11].unwrap().span()], "False");
    assert_eq!(&input_string[desired_output[12].unwrap().span()], ",");
    assert_eq!(&input_string[desired_output[13].unwrap().span()], "\n		");
    assert_eq!(&input_string[desired_output[14].unwrap().span()], "func2");
    assert_eq!(&input_string[desired_output[15].unwrap().span()], "(");
    assert_eq!(&input_string[desired_output[16].unwrap().span()], ")");
}

#[test]
fn test_valid_numerics() {
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
42f32
42f64
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

    let lexer = Lexer::new(&input_string, StrId::DUMMY);

    assert_eq!(&input_string[span!("0b111", 42, 46).span()], "0b111");

    let desired_output = vec![
        Ok(span!(Numeric(span!("0xF3a", 0, 4), None), 0, 4)),
        Ok(span!(Numeric(span!("0xfff", 6, 10), Some(span!("i32", 11, 13))), 6, 13)),
        Ok(span!(Numeric(span!("0xfff", 15, 19), Some(span!("i64", 20, 22))), 15, 22)),
        Ok(span!(Numeric(span!("0xfff", 24, 28), Some(span!("u32", 29, 31))), 24, 31)),
        Ok(span!(Numeric(span!("0xfff", 33, 37), Some(span!("u64", 38, 40))), 33, 40)),
        Ok(span!(Numeric(span!("0b111", 42, 46), None), 42, 46)),
        Ok(span!(Numeric(span!("0b101", 48, 52), Some(span!("i32", 53, 55))), 48, 55)),
        Ok(span!(Numeric(span!("0b101", 57, 61), Some(span!("i64", 62, 64))), 57, 64)),
        Ok(span!(Numeric(span!("0b101", 66, 70), Some(span!("u32", 71, 73))), 66, 73)),
        Ok(span!(Numeric(span!("0b101", 75, 79), Some(span!("u64", 80, 82))), 75, 82)),
        Ok(span!(Numeric(span!("42", 84, 85), None), 84, 85)),
        Ok(span!(Numeric(span!("42", 87, 88), Some(span!("i32", 89, 91))), 87, 91)),
        Ok(span!(Numeric(span!("42", 93, 94), Some(span!("i64", 95, 97))), 93, 97)),
        Ok(span!(Numeric(span!("42", 99, 100), Some(span!("u32", 101, 103))), 99, 103)),
        Ok(span!(Numeric(span!("42", 105, 106), Some(span!("u64", 107, 109))), 105, 109)),
        Ok(span!(Numeric(span!("42", 111, 112), Some(span!("f32", 113, 115))), 111, 115)),
        Ok(span!(Numeric(span!("42", 117, 118), Some(span!("f64", 119, 121))), 117, 121)),
        Ok(span!(Numeric(span!("42.0", 123, 126), None), 123, 126)),
        Ok(span!(Numeric(span!("42.0", 128, 131), Some(span!("f32", 132, 134))), 128, 134)),
        Ok(span!(Numeric(span!("42.0", 136, 139), Some(span!("f64", 140, 142))), 136, 142)),
        Ok(span!(Numeric(span!("0xFFFF_FFFF", 144, 154), None), 144, 154)),
        Ok(span!(Numeric(span!("0b0101_0101", 156, 166), None), 156, 166)),
        Ok(span!(Numeric(span!("400_000", 168, 174), None), 168, 174)),
        Ok(span!(Numeric(span!("400_000.000_000", 176, 190), None), 176, 190)),
        Ok(span!(Numeric(span!("0", 192, 192), Some(span!("f32", 193, 195))), 192, 195)),
        Ok(span!(Numeric(span!("0001", 197, 200), None), 197, 200)),
        Ok(span!(Numeric(span!("0000_0000", 202, 210), None), 202, 210)),
        Ok(span!(Numeric(span!("0000_0001", 212, 220), Some(span!("u8", 221, 222))), 212, 222)),
        Ok(span!(Numeric(span!("1.0", 224, 226), Some(span!("f128", 227, 230))), 224, 230)),
        Ok(span!(Numeric(span!("117", 232, 234), Some(span!("u128", 235, 238))), 232, 238)),
    ];

    cmp_tokens(lexer, &desired_output, true);
}

#[test]
fn test_invalid_numerics() {
    // Invalid as defined by the parser, not the lexer
    let input_string = "\
0x
0xz
0xfz
0xfi3
0b
0ba
0b1f
42i3
42i31
42.
42.0f
42.0f31
0b2";

    let lexer = Lexer::new(&input_string, StrId::DUMMY);
    let desired_output = vec![
        Err(LexerError::IncompleteNumeric(span!("0x", 0, 1))),
        Err(LexerError::InvalidNumeric(span!("0x", 3, 4), span!("z", 5, 5))),
        Ok(span!(Numeric(span!("0xf", 7, 9), Some(span!("z", 10, 10))), 7, 10)),
        Ok(span!(Numeric(span!("0xf", 12, 14), Some(span!("i3", 15, 16))), 12, 16)),
        Err(LexerError::IncompleteNumeric(span!("0b", 18, 19))),
        Err(LexerError::InvalidNumeric(span!("0b", 21, 22), span!("a", 23, 23))),
        Ok(span!(Numeric(span!("0b1", 25, 27), Some(span!("f", 28, 28))), 25, 28)),
        Ok(span!(Numeric(span!("42", 30, 31), Some(span!("i3", 32, 33))), 30, 33)),
        Ok(span!(Numeric(span!("42", 35, 36), Some(span!("i31", 37, 39))), 35, 39)),
        Ok(span!(Numeric(span!("42.", 41, 43), None), 41, 43)),
        Ok(span!(Numeric(span!("42.0", 45, 48), Some(span!("f", 49, 49))), 45, 49)),
        Ok(span!(Numeric(span!("42.0", 51, 54), Some(span!("f31", 55, 57))), 51, 57)),
        Err(LexerError::InvalidNumeric(span!("0b", 59, 60), span!("2", 61, 61))),
    ];

    cmp_tokens(lexer, &desired_output, true);
}

// #[test]
// fn test_functions() {
//     let input_string = "\
// fn basic_func() -> str
// 	def ch = 'g'
// 	var string = \"strin\"

// 	if ch is 'g',
// 		string += ch

// 	return string";

//     let lexer = Lexer::new(&input_string);
//     let desired_output = vec![Keyword(Function), Identifier("basic_func".to_string()), Symbol(ParenOpen), Symbol(ParenClose),
//                               Symbol(RightThinArrow), Identifier("str".to_string()), Indent(1),
//                               Keyword(Def), Identifier("ch".to_string()), Symbol(Equals), CharLiteral('g'), Indent(1),
//                               Keyword(Var), Identifier("string".to_string()), Symbol(Equals), StrLiteral("strin".to_string()), Indent(0),
//                               Indent(1),
//                               Keyword(If), Identifier("ch".to_string()), Keyword(Is), CharLiteral('g'), Symbol(Comma), Indent(2),
//                               Identifier("string".to_string()), Symbol(PlusEquals), Identifier("ch".to_string()), Indent(0),
//                               Indent(1),
//                               Keyword(Return), Identifier("string".to_string())];

//     cmp_tokens(lexer, desired_output);
// }

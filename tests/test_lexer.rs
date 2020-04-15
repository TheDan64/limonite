// use limonite::lexical::keywords::Keywords::{Def, Function, If, Is, Return, Var};
use limonite::interner::StrId;
use limonite::lexical::Symbol::{Comma, Equals, ParenClose, ParenOpen, PlusEquals, RightThinArrow};
use limonite::lexical::{CommentKind, Lexer, Token, TokenKind::{self, *}, TokenResult};
use limonite::span::{Span, Spanned};

macro_rules! span {
    ($e:expr, $start:expr, $end:expr) => {
        Spanned::new($e, Span::new(StrId::DUMMY, $start, $end))
    };
}

fn cmp_tokens<'s>(mut lexer: Lexer<'s>, tokens: &[Token<'s>], skip_indents: bool) {
    let mut iter: &mut dyn Iterator<Item=TokenResult<'s>> = &mut lexer;
    let mut filter;

    if skip_indents {
        filter = iter.filter(|tok_res| match tok_res {
            Ok(t) => {
                dbg!((&t, !matches!(t.node(), TokenKind::Indent(_))));

                !matches!(t.node(), TokenKind::Indent(_))
            },
            Err(_) => true,
        });
        iter = &mut filter;
    }

    for desired_tok in tokens {
        let tok = iter.next();

        assert_eq!(tok, Some(Ok(*desired_tok)));
    }
}

#[test]
fn test_hello_world() {
    let input_string = "\
>> Hello World!

print(\"Hello World!\")";

    let lexer = Lexer::new(&input_string, StrId::DUMMY);
    let desired_output = vec![
        span!(Comment(CommentKind::Single(span!(" Hello World!", 2, 14))), 0, 14),
        span!(Indent(0), 15, 15),
        span!(Indent(0), 16, 16),
        span!(Identifier("print"), 17, 21),
        span!(Symbol(ParenOpen), 22, 22),
        span!(StrLiteral("\"Hello World!\""), 23, 36),
        span!(Symbol(ParenClose), 37, 37),
    ];

    cmp_tokens(lexer, &desired_output, false);
}

// #[test]
// fn test_indentation() {
//     let input_string = "\
// >>>
//     Test of indentation and a few keywords.
// <<<

// if True,
// 	func()

// 	if False,
// 		func2()";

//     let lexer = Lexer::new(&input_string);
//     let desired_output = vec![
//         Comment("\n    Test of indentation and a few keywords.\n".to_string()), Indent(0),
//         Indent(0),
//         Keyword(If), BoolLiteral(true), Symbol(Comma), Indent(1),
//         Identifier("func".to_string()), Symbol(ParenOpen), Symbol(ParenClose), Indent(0),
//         Indent(1),
//         Keyword(If), BoolLiteral(false), Symbol(Comma), Indent(2),
//         Identifier("func2".to_string()), Symbol(ParenOpen), Symbol(ParenClose),
//     ];

//     cmp_tokens(lexer, desired_output);
// }

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
        span!(Numeric(span!("0xF3a", 0, 4), None), 0, 4),
        span!(Numeric(span!("0xfff", 6, 10), Some(span!("i32", 11, 13))), 6, 13),
        span!(Numeric(span!("0xfff", 15, 19), Some(span!("i64", 20, 22))), 15, 22),
        span!(Numeric(span!("0xfff", 24, 28), Some(span!("u32", 29, 31))), 24, 31),
        span!(Numeric(span!("0xfff", 33, 37), Some(span!("u64", 38, 40))), 33, 40),
        span!(Numeric(span!("0b111", 42, 46), None), 42, 46),
        span!(Numeric(span!("0b101", 48, 52), Some(span!("i32", 53, 55))), 48, 55),
        span!(Numeric(span!("0b101", 57, 61), Some(span!("i64", 62, 64))), 57, 64),
        span!(Numeric(span!("0b101", 66, 70), Some(span!("u32", 71, 73))), 66, 73),
        span!(Numeric(span!("0b101", 75, 79), Some(span!("u64", 80, 82))), 75, 82),
        span!(Numeric(span!("42", 84, 85), None), 84, 85),
        span!(Numeric(span!("42", 87, 88), Some(span!("i32", 89, 91))), 87, 91),
        span!(Numeric(span!("42", 93, 94), Some(span!("i64", 95, 97))), 93, 97),
        span!(Numeric(span!("42", 99, 100), Some(span!("u32", 101, 103))), 99, 103),
        span!(Numeric(span!("42", 105, 106), Some(span!("u64", 107, 109))), 105, 109),
        span!(Numeric(span!("42", 111, 112), Some(span!("f32", 113, 115))), 111, 115),
        span!(Numeric(span!("42", 117, 118), Some(span!("f64", 119, 121))), 117, 121),
        span!(Numeric(span!("42.0", 123, 126), None), 123, 126),
        span!(Numeric(span!("42.0", 128, 131), Some(span!("f32", 132, 134))), 128, 134),
        span!(Numeric(span!("42.0", 136, 139), Some(span!("f64", 140, 142))), 136, 142),
        span!(Numeric(span!("0xFFFF_FFFF", 144, 154), None), 144, 154),
        span!(Numeric(span!("0b0101_0101", 156, 166), None), 156, 166),
        span!(Numeric(span!("400_000", 168, 174), None), 168, 174),
        span!(Numeric(span!("400_000.000_000", 176, 190), None), 176, 190),
        span!(Numeric(span!("0", 192, 192), Some(span!("f32", 193, 195))), 192, 195),
        span!(Numeric(span!("0001", 197, 200), None), 197, 200),
        span!(Numeric(span!("0000_0000", 202, 210), None), 202, 210),
        span!(Numeric(span!("0000_0001", 212, 220), Some(span!("u8", 221, 222))), 212, 222),
        span!(Numeric(span!("1.0", 224, 226), Some(span!("f128", 227, 230))), 224, 230),
        span!(Numeric(span!("117", 232, 234), Some(span!("u128", 235, 238))), 232, 238),
    ];

    cmp_tokens(lexer, &desired_output, true);
}

// #[test]
// fn test_invalid_numerics() {
//     let input_string = "\
// 0x
// 0xz
// 0xfz
// 0xfi3
// 0xfi31
// 0xfi6
// 0xfi63
// 0b
// 0ba
// 0b1a
// 0b1f
// 42i3
// 42i31
// 42.
// 42.0f
// 42.0f3
// 42.0f31
// 42.0f8
// 42.0f16";

//     let lexer = Lexer::new(&input_string);
//     let desired_output = vec![Error("No hexadecimal value was found.".to_string()), Indent(0),
//                               Error("No hexadecimal value was found.".to_string()), Identifier("z".to_string()), Indent(0),
//                               Error("Invalid suffix z. Did you mean u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, or f128?".to_string()), Indent(0),
//                               Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
//                               Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
//                               Error("Invalid suffix i6. Did you mean i64?".to_string()), Indent(0),
//                               Error("Invalid suffix i63. Did you mean i64?".to_string()), Indent(0),
//                               // Error("Invalid suffix u8. Did you mean u32 or u64?".to_string()), Indent(0),
//                               Error("No binary value was found.".to_string()), Indent(0),
//                               Error("No binary value was found.".to_string()), Identifier("a".to_string()), Indent(0),
//                               Error("Invalid suffix a. Did you mean u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, or f128?".to_string()), Indent(0),
//                               Error("Invalid suffix f. Did you mean f32, f64, or f128?".to_string()), Indent(0),
//                               // Error("Invalid suffix f. Did you mean u32, u64, i32, or i64?".to_string()), Numeric("32".to_string(), None), Indent(0),
//                               Error("Invalid suffix i3. Did you mean i32?".to_string()), Indent(0),
//                               Error("Invalid suffix i31. Did you mean i32?".to_string()), Indent(0),
//                               Error("No numbers found after the decimal point.".to_string()), Indent(0),
//                               Error("Invalid suffix f. Did you mean f32, f64, or f128?".to_string()), Indent(0),
//                               Error("Invalid suffix f3. Did you mean f32?".to_string()), Indent(0),
//                               Error("Invalid suffix f31. Did you mean f32?".to_string()), Indent(0),
//                               Error("Invalid suffix f8. Did you mean f32, f64, or f128?".to_string()), Indent(0),
//                               Error("Invalid suffix f16. Did you mean f128?".to_string())];

//     cmp_tokens(lexer, desired_output);
// }

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

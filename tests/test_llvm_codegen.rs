extern crate limonite;

use limonite::codegen::llvm::LLVMGenerator;
use limonite::syntax::expr::{Expr, ExprWrapper};
use limonite::syntax::op::InfixOp;
use limonite::syntax::literals::Literals;

use std::mem::transmute;

macro_rules! block {
    ($($args:tt)*) => {
        ExprWrapper::default(Expr::Block(vec![$($args)*]))
    }
}

macro_rules! var {
    ($arg:expr) => {
        ExprWrapper::default(Expr::Var($arg.into()))
    }
}

macro_rules! ret {
    () => {
        ExprWrapper::default(Expr::Return(None))
    };
    ($arg:expr) => {
        ExprWrapper::default(Expr::Return(Some($arg)))
    };
}

macro_rules! u8 {
    ($arg:tt) => {
        ExprWrapper::default(Expr::Literal(Literals::U8Num($arg)))
    }
}

macro_rules! string {
    ($arg:tt) => {
        ExprWrapper::default(Expr::Literal(Literals::UTF8String($arg.into())))
    }
}

macro_rules! op {
    ($left_arg:expr, + $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Add, $left_arg, $right_arg))
    };
    ($left_arg:expr, < $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Lt, $left_arg, $right_arg))
    };
    ($left_arg:expr, > $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Gt, $left_arg, $right_arg))
    };
}

macro_rules! assign {
    ($left_arg:expr, = $right_arg:expr) => {
        ExprWrapper::default(Expr::Assign($left_arg, $right_arg))
    };
    ($left_arg:expr, += $right_arg:expr) => {
        ExprWrapper::default(Expr::Assign($left_arg, ExprWrapper::default(Expr::InfixOp(InfixOp::Add, $left_arg, $right_arg))))
    };
    ($left_arg:expr, -= $right_arg:expr) => {
        ExprWrapper::default(Expr::Assign($left_arg, ExprWrapper::default(Expr::InfixOp(InfixOp::Sub, $left_arg, $right_arg))))
    };
}

#[test]
fn test_sum_function() {
    // Creates a limonite function that looks like:
    // fn add_two_ints(left: u64, right: u64) -> u64,
    //     return left + right

    let ret = ret!(op!(var!("left"), + var!("right")));
    let fn_args = vec![("left".into(), "u64".into()), ("right".into(), "u64".into())];
    let ast = ExprWrapper::default(Expr::FnDecl("add_two_ints".into(), fn_args, Some("u64".into()), ret));

    let mut llvm_generator = LLVMGenerator::new();

    llvm_generator.add_module(ast, false, false);
    llvm_generator.initialize(true);

    let address = llvm_generator.get_function_address("add_two_ints").expect("Could not find function address");

    let add_two_ints: extern "C" fn(u64, u64) -> u64 = unsafe { transmute(address) };

    assert_eq!(add_two_ints(456, 987), 1443);
}

#[test]
fn test_while_lt_increment_u8() {
    // Creates a limonite function that looks like:
    // fn inc_until() -> u8,
    //     var i = 0u8
    //
    //     while i < 10,
    //         i += 1
    //
    //     return i

    let var_decl = ExprWrapper::default(Expr::VarDecl(false, "i".into(), Some("u8".into()), u8!(0)));
    let loop_cond = op!(var!("i"), < u8!(10));
    let loop_body = assign!(var!("i"), += u8!(1));
    let while_loop = ExprWrapper::default(Expr::WhileLoop(loop_cond, loop_body));
    let ret = ret!(var!("i"));
    let body = block![
        var_decl,
        while_loop,
        ret
    ];
    let ast = ExprWrapper::default(Expr::FnDecl("inc_until".into(), Vec::new(), Some("u8".into()), body));

    let mut llvm_generator = LLVMGenerator::new();

    llvm_generator.add_module(ast, false, false);
    llvm_generator.initialize(true);

    let address = llvm_generator.get_function_address("inc_until").expect("Could not find function address");

    let inc_until: extern "C" fn() -> u8 = unsafe { transmute(address) };

    assert_eq!(inc_until(), 10);
}

#[test]
fn test_while_gt_decrement_u8() {
    // Creates a limonite function that looks like:
    // fn dec_until() -> u8,
    //     var i = 10u8
    //
    //     while i > 0,
    //         i -= 1
    //
    //     return i

    let var_decl = ExprWrapper::default(Expr::VarDecl(false, "i".into(), Some("u8".into()), u8!(10)));
    let loop_cond = op!(var!("i"), > u8!(0));
    let loop_body = assign!(var!("i"), -= u8!(1));
    let while_loop = ExprWrapper::default(Expr::WhileLoop(loop_cond, loop_body));
    let ret = ret!(var!("i"));
    let body = block![
        var_decl,
        while_loop,
        ret
    ];
    let ast = ExprWrapper::default(Expr::FnDecl("dec_until".into(), Vec::new(), Some("u8".into()), body));

    let mut llvm_generator = LLVMGenerator::new();

    llvm_generator.add_module(ast, false, false);
    llvm_generator.initialize(true);

    let address = llvm_generator.get_function_address("dec_until").expect("Could not find function address");

    let dec_until: extern "C" fn() -> u8 = unsafe { transmute(address) };

    assert_eq!(dec_until(), 0);
}

#[test]
fn test_hello_world() {
    // Creates a limonite function that looks like:
    // fn hello_world(),
    //     var s = "Hello, World!"
    //     print(s)
    //     return

    let var_decl = ExprWrapper::default(Expr::VarDecl(false, "s".into(), Some("std.string.String".into()), string!("Hello, World!")));
    let print_call = ExprWrapper::default(Expr::FnCall("print".into(), vec![var!("s")]));

    let body = block![
        var_decl,
        print_call,
        ret!(),
    ];
    let ast = ExprWrapper::default(Expr::FnDecl("hello_world".into(), Vec::new(), None, body));

    let mut llvm_generator = LLVMGenerator::new();

    llvm_generator.add_module(ast, false, true);
    llvm_generator.initialize(true);

    let address = llvm_generator.get_function_address("hello_world").expect("Could not find function address");

    let hello_world: extern "C" fn() = unsafe { transmute(address) };

    // REVIEW: Is there a way to capture stdout to ensure the right text is being printed?

    hello_world();
}

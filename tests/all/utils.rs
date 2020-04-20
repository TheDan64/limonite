macro_rules! span {
    ($e:expr, $start:expr, $end:expr) => {{
        use limonite::span::{Span, Spanned};
        Spanned::new($e, Span::new(StrId::DUMMY, $start, $end))
    }};
    (boxed $e:expr, $start:expr, $end:expr) => {{
        span!(Box::new($e), $start, $end)
    }};
}

macro_rules! block {
    ($indent:literal => [$($stmt_kinds:expr,)*]) => {{
        use limonite::syntax::{Block, Stmt};
        Block::new($indent, vec![$(Stmt::new($stmt_kinds),)*])
    }};
}

// macro_rules! var {
//     ($arg:expr) => {
//         ExprWrapper::default(Expr::Var($arg.into()))
//     };
// }

// macro_rules! ret {
//     () => {
//         ExprWrapper::default(Expr::Return(None))
//     };
//     ($arg:expr) => {
//         ExprWrapper::default(Expr::Return(Some($arg)))
//     };
// }

// macro_rules! u8 {
//     ($arg:tt) => {
//         ExprWrapper::default(Expr::Literal(Literals::U8Num($arg)))
//     }
// }

// macro_rules! u32 {
//     ($arg:tt) => {
//         ExprWrapper::default(Expr::Literal(Literals::U32Num($arg)))
//     }
// }

// macro_rules! string {
//     ($arg:tt) => {
//         ExprWrapper::default(Expr::Literal(Literals::UTF8String($arg.into())))
//     }
// }

// macro_rules! op {
//     ($left_arg:expr, + $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Add, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, - $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Sub, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, * $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Mul, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, / $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Div, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, < $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Lt, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, <= $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Lte, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, > $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Gt, $left_arg, $right_arg))
//     };
//     ($left_arg:expr, >= $right_arg:expr) => {
//         ExprWrapper::default(Expr::InfixOp(InfixOp::Gte, $left_arg, $right_arg))
//     };
// }

// macro_rules! assign {
//     ($left_arg:expr, = $right_arg:expr) => {
//         ExprWrapper::default(Expr::Assign($left_arg, $right_arg))
//     };
//     ($left_arg:expr, += $right_arg:expr) => {
//         ExprWrapper::default(Expr::Assign($left_arg, op!($left_arg, + $right_arg)))
//     };
//     ($left_arg:expr, -= $right_arg:expr) => {
//         ExprWrapper::default(Expr::Assign($left_arg, op!($left_arg, - $right_arg)))
//     };
//     ($left_arg:expr, *= $right_arg:expr) => {
//         ExprWrapper::default(Expr::Assign($left_arg, op!($left_arg, * $right_arg)))
//     };
//     ($left_arg:expr, /= $right_arg:expr) => {
//         ExprWrapper::default(Expr::Assign($left_arg, op!($left_arg, / $right_arg)))
//     };
// }

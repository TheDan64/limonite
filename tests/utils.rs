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
    ($left_arg:expr, <= $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Lte, $left_arg, $right_arg))
    };
    ($left_arg:expr, > $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Gt, $left_arg, $right_arg))
    };
    ($left_arg:expr, >= $right_arg:expr) => {
        ExprWrapper::default(Expr::InfixOp(InfixOp::Gte, $left_arg, $right_arg))
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
    ($left_arg:expr, *= $right_arg:expr) => {
        ExprWrapper::default(Expr::Assign($left_arg, ExprWrapper::default(Expr::InfixOp(InfixOp::Mul, $left_arg, $right_arg))))
    };
    ($left_arg:expr, /= $right_arg:expr) => {
        ExprWrapper::default(Expr::Assign($left_arg, ExprWrapper::default(Expr::InfixOp(InfixOp::Div, $left_arg, $right_arg))))
    };
}

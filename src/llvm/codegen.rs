extern crate rustc;

use self::rustc::llvm::*;
use syntax::ast::expr::*;
//use syntax::ast::consts::*;

pub trait CodeGen {
    fn gen_code(&self, llvm_builder: *mut Builder_opaque);
}

impl CodeGen for ExprWrapper {
    fn gen_code(&self, llvm_builder: *mut Builder_opaque) {
        self.expr.gen_code(llvm_builder);
    }
}

// LLVMInsertIntoBuilder(llvm_builder, )
// LLVMBuildAdd(builder, leftint, rightint)?
impl CodeGen for Expr {
    fn gen_code(&self, llvm_builder: *mut Builder_opaque) {
        match *self {
            Expr::Block(ref vec) => {
                for expr in vec {
                    expr.gen_code(llvm_builder);
                }
            },
            // Expr::InfixOp(ref op, ref expr1, ref expr2) => {
            //     let lhs = expr1.get_expr();
            //     let rhs = expr2.get_expr();

            //     if let (&Expr::Const(ref lhs_const), &Expr::Const(ref rhs_const)) = (lhs, rhs) {
            //         match (lhs_const, rhs_const) {
            //             (&Const::I32Num(lhs_num), &Const::I32Num(rhs_num)) => {
            //                 // No idea if this works

            //                 let name = concat!("idk", "\0").as_ptr() as *const i8;
            //                 // Returns *mut Value_opaque
            //                 LLVMBuildAdd(llvm_builder, lhs_num, rhs_num, name);
            //             },
            //             _ => ()
            //         }

            //     }
            // },
            Expr::NoOp => (),
            _ => ()
        }
    }
}

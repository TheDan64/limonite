use crate::span::Spanned;
use crate::syntax::{Expr, Type};
// Const declaration?, variable name, type(optional in parser but not SA), and expression
    // VarDecl(bool, Spanned<&'s str>, Option<Spanned<&'s str>>, Expr<'s>),

#[derive(Debug, PartialEq)]
pub struct Local<'s> {
    // TODO: Pat instead of name
    // pat: Pat,
    ident: Spanned<&'s str>,
    is_immut: bool,
    ty: Option<Type<'s>>, // MaybeSpanned<Type>?
    init: Expr<'s>,
}
impl<'s> Local<'s> {
    pub fn new(is_immut: bool, ident: Spanned<&'s str>, ty: Option<Type<'s>>, init: Expr<'s>) -> Self {
        Local { is_immut, ident, ty, init, }
    }
}

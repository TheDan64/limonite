use crate::syntax::{Expr, Local};

#[derive(Debug, PartialEq)]
pub struct Stmt<'s> {
    kind: StmtKind<'s>,
}

impl<'s> Stmt<'s> {
    pub fn new<K: Into<StmtKind<'s>>>(k: K) -> Self {
        Stmt {
            kind: k.into(),
        }
    }

    pub fn kind(&self) -> &StmtKind<'s> {
        &self.kind
    }
}

#[derive(Debug, PartialEq)]
pub enum StmtKind<'s> {
    Local(Local<'s>),
    Item,
    Expr(Expr<'s>),
}

impl<'s> From<Expr<'s>> for StmtKind<'s> {
    fn from(e: Expr<'s>) -> Self {
        StmtKind::Expr(e)
    }
}

impl<'s> From<Local<'s>> for StmtKind<'s> {
    fn from(l: Local<'s>) -> Self {
        StmtKind::Local(l)
    }
}

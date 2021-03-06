use crate::syntax::{Expr, Item, Local};

#[derive(Clone, Debug, PartialEq)]
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

    pub fn kind_mut(&mut self) -> &mut StmtKind<'s> {
        &mut self.kind
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind<'s> {
    Local(Local<'s>),
    Item(Item<'s>),
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

impl<'s> From<Item<'s>> for StmtKind<'s> {
    fn from(i: Item<'s>) -> Self {
        StmtKind::Item(i)
    }
}

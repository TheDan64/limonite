use crate::syntax::Stmt;

#[derive(Debug, PartialEq)]
pub struct Block<'s> {
    stmts: Vec<Stmt<'s>>,
}

impl<'s> Block<'s> {
    pub fn new(stmts: Vec<Stmt<'s>>) -> Self {
        Block {
            stmts,
        }
    }

    pub fn stmts(&self) -> &[Stmt<'s>] {
        &self.stmts
    }
}

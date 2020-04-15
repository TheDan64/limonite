use crate::syntax::Stmt;

#[derive(Debug, PartialEq)]
pub struct Block<'s> {
    indent: u32,
    stmts: Vec<Stmt<'s>>,
}

impl<'s> Block<'s> {
    pub fn new(stmts: Vec<Stmt<'s>>) -> Self {
        Block {
            // TODO:
            indent: 0,
            stmts,
        }
    }

    pub fn stmts(&self) -> &[Stmt<'s>] {
        &self.stmts
    }
}

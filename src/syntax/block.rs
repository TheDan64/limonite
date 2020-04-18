use crate::syntax::Stmt;

#[derive(Debug, PartialEq)]
pub struct Block<'s> {
    indent: u32,
    stmts: Vec<Stmt<'s>>,
}

impl<'s> Block<'s> {
    pub fn new(indent: u32, stmts: Vec<Stmt<'s>>) -> Self {
        Block {
            indent,
            stmts,
        }
    }

    pub fn stmts(&self) -> &[Stmt<'s>] {
        &self.stmts
    }
}

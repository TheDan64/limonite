use crate::syntax::Stmt;

#[derive(Debug, PartialEq)]
pub struct Block<'s> {
    pub(crate) indent: u32,
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

    pub fn stmts_mut(&mut self) -> &mut [Stmt<'s>] {
        &mut self.stmts
    }

    pub fn append_stmt(&mut self, stmt: Stmt<'s>) {
        self.stmts.push(stmt);
    }

    pub fn indent(&self) -> u32 {
        self.indent
    }
}

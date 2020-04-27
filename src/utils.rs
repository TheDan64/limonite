use crate::syntax::{Block, ExprKind, ItemKind, Literal, Local, Stmt};
use crate::syntax::visitor::AstVisitor;

use debug_tree::TreeBuilder;
use debug_tree::scoped_branch::ScopedBranch;

pub struct DebugTree(TreeBuilder, Vec<ScopedBranch>);

impl DebugTree {
    pub fn new() -> Self {
        DebugTree(TreeBuilder::new(), Vec::new())
    }

    fn add_branch(&mut self, text: &str) {
        self.1.push(self.0.add_branch(text));
    }

    fn pop_branch(&mut self) {
        self.1.pop().unwrap();
    }
}

impl Drop for DebugTree {
    fn drop(&mut self) {
        drop(std::mem::replace(&mut self.1, Vec::new()));
        self.0.print();
    }
}

impl<'s> AstVisitor<'s> for DebugTree {
    fn visit_block(&mut self, block: &mut Block<'s>) {
        self.add_branch(&format!("Block Indent {}", block.indent()));
    }

    fn exit_block(&mut self, _block: &mut Block<'s>) {
        self.pop_branch();
    }

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind<'s>) {
        let string = match expr_kind {
            ExprKind::InfixOp(op, ..) => format!("Expr: {}", op.get_node().as_str()),
            ExprKind::If(..) => "Expr: If".into(),
            ExprKind::Var(s) => {
                self.0.add_leaf(&format!("Expr: Var {}", s));

                return;
            },
            ExprKind::FnCall(name, ..) => format!("Expr: FnCall {}", name.node()),
            ExprKind::Literal(lit) => {
                let string = match lit {
                    Literal::I8Num(n) => format!("Expr: {}", n),
                    Literal::I16Num(n) => format!("Expr: {}", n),
                    Literal::I32Num(n) => format!("Expr: {}", n),
                    Literal::I64Num(n) => format!("Expr: {}", n),
                    Literal::I128Num(n) => format!("Expr: {}", n),
                    Literal::U8Num(n) => format!("Expr: {}", n),
                    Literal::U16Num(n) => format!("Expr: {}", n),
                    Literal::U32Num(n) => format!("Expr: {}", n),
                    Literal::U64Num(n) => format!("Expr: {}", n),
                    Literal::U128Num(n) => format!("Expr: {}", n),
                    Literal::UTF8String(s) => format!("Expr: {}", s),
                    Literal::UTF8Char(ch) => format!("Expr: {}", ch),
                    _ => unimplemented!(),
                };

                self.0.add_leaf(&string);

                return;
            },
            ExprKind::WhileLoop(..) => "Expr: While".into(),
            e => unimplemented!("{:?}", e),
        };

        self.add_branch(&string);
    }

    fn exit_expr_kind(&mut self, expr_kind: &mut ExprKind<'s>) {
        // Don't pop leaf nodes
        match expr_kind {
            ExprKind::Literal(..)
            | ExprKind::Var(..) => return,
            _ => (),
        };

        self.pop_branch();
    }

    fn visit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) {
        let string = match item_kind {
            ItemKind::FnDef(name, _sig, _block) => {
                format!("Item: FnDef {}", name.node())
            },
            _ => unimplemented!(),
        };

        self.add_branch(&string);
    }

    fn exit_item_kind(&mut self, _item_kind: &mut ItemKind<'s>) {
        self.pop_branch();
    }

    fn visit_local(&mut self, _local: &mut Local<'s>) {
        self.add_branch("Local");
    }

    fn exit_local(&mut self, _local: &mut Local<'s>) {
        self.pop_branch();
    }

    fn visit_stmt(&mut self, _stmt: &mut Stmt<'s>) {
        self.add_branch("Stmt");
    }

    fn exit_stmt(&mut self, _stmt: &mut Stmt<'s>) {
        self.pop_branch();
    }
}

#[macro_export]
macro_rules! _dbg_ast {
    ($block: expr) => {{
        use crate::syntax::visitor::Visitor;
        use crate::utils::DebugTree;

        // FIXME: Shouldn't need to take ast mutably
        Visitor::new(DebugTree::new()).run(&mut $block);
    }};
}

pub use _dbg_ast as dbg_ast;

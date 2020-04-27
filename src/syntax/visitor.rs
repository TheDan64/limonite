use crate::syntax::{Block, Expr, ExprKind, Item, ItemKind, Local, Stmt, StmtKind, Type, TypeKind};

pub trait AstVisitor<'s> {
    #[inline(always)]
    fn exit_block(&mut self, _block: &mut Block<'s>) {}
    #[inline(always)]
    fn exit_expr(&mut self, _expr: &mut Expr<'s>) {}
    #[inline(always)]
    fn exit_expr_kind(&mut self, _expr_kind: &mut ExprKind<'s>) {}
    #[inline(always)]
    fn exit_item(&mut self, _item: &mut Item<'s>) {}
    #[inline(always)]
    fn exit_item_kind(&mut self, _item_kind: &mut ItemKind<'s>) {}
    #[inline(always)]
    fn exit_local(&mut self, _local: &mut Local<'s>) {}
    #[inline(always)]
    fn exit_stmt(&mut self, _stmt: &mut Stmt<'s>) {}
    #[inline(always)]
    fn exit_stmt_kind(&mut self, _stmt_kind: &mut StmtKind<'s>) {}
    #[inline(always)]
    fn exit_type(&mut self, _ty: &mut Type<'s>) {}
    #[inline(always)]
    fn exit_type_kind(&mut self, _ty_kind: &mut TypeKind<'s>) {}
    #[inline(always)]
    fn visit_block(&mut self, _block: &mut Block<'s>) {}
    #[inline(always)]
    fn visit_expr(&mut self, _expr: &mut Expr<'s>) {}
    #[inline(always)]
    fn visit_expr_kind(&mut self, _expr_kind: &mut ExprKind<'s>) {}
    #[inline(always)]
    fn visit_item(&mut self, _item: &mut Item<'s>) {}
    #[inline(always)]
    fn visit_item_kind(&mut self, _item_kind: &mut ItemKind<'s>) {}
    #[inline(always)]
    fn visit_local(&mut self, _local: &mut Local<'s>) {}
    #[inline(always)]
    fn visit_stmt(&mut self, _stmt: &mut Stmt<'s>) {}
    #[inline(always)]
    fn visit_stmt_kind(&mut self, _stmt_kind: &mut StmtKind<'s>) {}
    #[inline(always)]
    fn visit_type(&mut self, _ty: &mut Type<'s>) {}
    #[inline(always)]
    fn visit_type_kind(&mut self, _ty_kind: &mut TypeKind<'s>) {}
}

pub struct Visitor<V> {
    visitor: V,
}

impl<'s, V: AstVisitor<'s>> Visitor<V> {
    pub fn new(visitor: V) -> Self {
        Visitor {
            visitor,
        }
    }

    pub fn run(&mut self, block: &mut Block<'s>) {
        self.visit_block(block);
    }

    fn visit_block(&mut self, block: &mut Block<'s>) {
        self.visitor.visit_block(block);

        for stmt in block.stmts_mut() {
            self.visit_stmt(stmt);
        }

        self.visitor.exit_block(block);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'s>) {
        self.visitor.visit_stmt(stmt);
        self.visit_stmt_kind(stmt.kind_mut());
        self.visitor.exit_stmt(stmt);
    }

    fn visit_stmt_kind(&mut self, stmt_kind: &mut StmtKind<'s>) {
        self.visitor.visit_stmt_kind(stmt_kind);

        match stmt_kind {
            StmtKind::Local(local) => self.visit_local(local),
            StmtKind::Item(item) => self.visit_item(item),
            StmtKind::Expr(expr) => self.visit_expr(expr),
        }

        self.visitor.exit_stmt_kind(stmt_kind);
    }

    fn visit_expr(&mut self, expr: &mut Expr<'s>) {
        self.visitor.visit_expr(expr);
        self.visit_expr_kind(expr.get_node_mut());
        self.visitor.exit_expr(expr);
    }

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind<'s>) {
        self.visitor.visit_expr_kind(expr_kind);

        // TODO:
        match expr_kind {
            ExprKind::InfixOp(_op, lhs_expr, rhs_expr) => {
                self.visit_expr(lhs_expr);
                self.visit_expr(rhs_expr);
            },
            ExprKind::UnaryOp(_op, expr) => {
                self.visit_expr(expr);
            },
            ExprKind::WhileLoop(expr, block) => {
                self.visit_expr(expr);
                self.visit_block(block);
            },
            ExprKind::If(expr, block, else_expr) => {
                self.visit_expr(expr);
                self.visit_block(block);

                if let Some(expr) = else_expr {
                    self.visit_expr(expr);
                }
            },
            ExprKind::Assign(_path, rhs_expr) => {
                self.visit_expr(rhs_expr);
            },
            ExprKind::FnCall(_ident, params) => {
                for expr in params {
                    self.visit_expr(expr);
                }
            },
            ExprKind::Block(block) => self.visit_block(block),
            ExprKind::Return(Some(expr)) => {
                self.visit_expr(expr)
            },
            _ => {},
        }

        self.visitor.exit_expr_kind(expr_kind);
    }

    fn visit_local(&mut self, local: &mut Local<'s>) {
        self.visitor.visit_local(local);

        // Destructure the local so we don't forget about newly added fields
        let Local { ident: _, is_immut: _, ty, init } = local;

        self.visit_expr(init);

        if let Some(ty) = ty {
            self.visit_type(ty);
        }

        self.visitor.exit_local(local);
    }

    fn visit_type(&mut self, ty: &mut Type<'s>) {
        self.visitor.visit_type(ty);
        self.visit_type_kind(ty.get_node_mut());
        self.visitor.exit_type(ty);
    }

    fn visit_type_kind(&mut self, ty_kind: &mut TypeKind<'s>) {
        self.visitor.visit_type_kind(ty_kind);

        // TODO:
        match ty_kind {
            _ => (),
        }

        self.visitor.visit_type_kind(ty_kind);
    }

    fn visit_item(&mut self, item: &mut Item<'s>) {
        self.visitor.visit_item(item);
        self.visit_item_kind(item.get_node_mut());
        self.visitor.exit_item(item);
    }

    fn visit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) {
        self.visitor.visit_item_kind(item_kind);

        // TODO:
        match item_kind {
            ItemKind::FnDef(_name, _fn_sig, block) => {
                self.visit_block(block);
            },
            _ => (),
        }

        self.visitor.exit_item_kind(item_kind);
    }
}

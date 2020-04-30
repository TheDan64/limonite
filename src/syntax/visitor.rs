use crate::syntax::{Block, Expr, ExprKind, Item, ItemKind, Local, Stmt, StmtKind, Type, TypeKind};

use std::marker::PhantomData;

#[derive(Debug)]
pub struct VisitOutcome<T> {
    node: Option<T>,
    visit_children: bool,
}

impl<T> Default for VisitOutcome<T> {
    fn default() -> Self {
        VisitOutcome {
            node: None,
            visit_children: true,
        }
    }
}

impl<T> VisitOutcome<T> {
    pub fn new<N: Into<Option<T>>>(node: N) -> Self {
        VisitOutcome { node: node.into(), visit_children: true }
    }

    pub fn new_with_stop<N: Into<Option<T>>>(node: N) -> Self {
        VisitOutcome { node: node.into(), visit_children: false }
    }

    pub fn into_node(self) -> T {
        self.node.expect("Called into_node on a None node")
    }

    #[inline(always)]
    pub fn will_visit_children(&self) -> bool {
        self.visit_children
    }
}

// type VisitResult<T, E> = Result<VisitOutcome<T>, E>;

pub trait AstVisitor<'s, R = ()> {
    #[inline(always)]
    fn exit_block(&mut self, _block: &mut Block<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_expr(&mut self, _expr: &mut Expr<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_expr_kind(&mut self, _expr_kind: &mut ExprKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_item(&mut self, _item: &mut Item<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_item_kind(&mut self, _item_kind: &mut ItemKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_local(&mut self, _local: &mut Local<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_stmt(&mut self, _stmt: &mut Stmt<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_stmt_kind(&mut self, _stmt_kind: &mut StmtKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_type(&mut self, _ty: &mut Type<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn exit_type_kind(&mut self, _ty_kind: &mut TypeKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_block(&mut self, _block: &mut Block<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_expr(&mut self, _expr: &mut Expr<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_expr_kind(&mut self, _expr_kind: &mut ExprKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_item(&mut self, _item: &mut Item<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_item_kind(&mut self, _item_kind: &mut ItemKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_local(&mut self, _local: &mut Local<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_stmt(&mut self, _stmt: &mut Stmt<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_stmt_kind(&mut self, _stmt_kind: &mut StmtKind<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_type(&mut self, _ty: &mut Type<'s>) -> VisitOutcome<R> { Default::default() }
    #[inline(always)]
    fn visit_type_kind(&mut self, _ty_kind: &mut TypeKind<'s>) -> VisitOutcome<R> { Default::default() }
}

pub struct Visitor<V, T> {
    visitor: V,
    phantom: PhantomData<T>,
}

impl<'s, R, V: AstVisitor<'s, R>> Visitor<V, R> {
    pub fn new(visitor: V) -> Self {
        Visitor {
            visitor,
            phantom: PhantomData,
        }
    }

    pub fn run(&mut self, block: &mut Block<'s>) {
        self.visit_block(block);
    }

    fn visit_block(&mut self, block: &mut Block<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_block(block).will_visit_children() {
            for stmt in block.stmts_mut() {
                self.visit_stmt(stmt);
            }
        }

        self.visitor.exit_block(block)
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_stmt(stmt).will_visit_children() {
            self.visit_stmt_kind(stmt.kind_mut());
        }
        self.visitor.exit_stmt(stmt)
    }

    fn visit_stmt_kind(&mut self, stmt_kind: &mut StmtKind<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_stmt_kind(stmt_kind).will_visit_children() {
            match stmt_kind {
                StmtKind::Local(local) => self.visit_local(local),
                StmtKind::Item(item) => self.visit_item(item),
                StmtKind::Expr(expr) => self.visit_expr(expr),
            };
        }

        self.visitor.exit_stmt_kind(stmt_kind)
    }

    fn visit_expr(&mut self, expr: &mut Expr<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_expr(expr).will_visit_children() {
            self.visit_expr_kind(expr.get_node_mut());
        }
        self.visitor.exit_expr(expr)
    }

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_expr_kind(expr_kind).will_visit_children() {
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
                ExprKind::Block(block) => { self.visit_block(block); },
                ExprKind::Return(Some(expr)) => {
                    self.visit_expr(expr);
                },
                _ => {},
            }
        }

        self.visitor.exit_expr_kind(expr_kind)
    }

    fn visit_local(&mut self, local: &mut Local<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_local(local).will_visit_children() {
            // Destructure the local so we don't forget about newly added fields
            let Local { ident: _, is_immut: _, ty, init } = local;

            self.visit_expr(init);

            if let Some(ty) = ty {
                self.visit_type(ty);
            }
        }

        self.visitor.exit_local(local)
    }

    fn visit_type(&mut self, ty: &mut Type<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_type(ty).will_visit_children() {
            self.visit_type_kind(ty.get_node_mut());
        }
        self.visitor.exit_type(ty)
    }

    fn visit_type_kind(&mut self, ty_kind: &mut TypeKind<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_type_kind(ty_kind).will_visit_children() {
            // TODO:
            match ty_kind {
                _ => (),
            }
        }

        self.visitor.exit_type_kind(ty_kind)
    }

    fn visit_item(&mut self, item: &mut Item<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_item(item).will_visit_children() {
            self.visit_item_kind(item.get_node_mut());
        }
        self.visitor.exit_item(item)
    }

    fn visit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) -> VisitOutcome<R> {
        if self.visitor.visit_item_kind(item_kind).will_visit_children() {
            // TODO:
            match item_kind {
                ItemKind::FnDef(_name, _fn_sig, block) => {
                    self.visit_block(block);
                },
                _ => (),
            }
        }

        self.visitor.exit_item_kind(item_kind)
    }
}

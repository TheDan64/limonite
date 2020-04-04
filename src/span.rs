#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub(crate) fn new(node: T, start_idx: usize, end_idx: usize) -> Self {
        Spanned {
            node,
            span: Span::new(start_idx, end_idx),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn get_node(&self) -> &T {
        &self.node
    }
}

impl<T: Copy> Spanned<T> {
    pub fn node(&self) -> T {
        self.node
    }
}

impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Spanned {
            node: T::default(),
            span: Span::default(),
        }
    }
}

impl<T: Copy> Copy for Spanned<T> {}
impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Spanned {
            node: self.node.clone(),
            span: self.span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    start_idx: usize,
    end_idx: usize,
}

impl Span {
    pub(crate) fn new(start_idx: usize, end_idx: usize) -> Self {
        Span { start_idx, end_idx }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span {
            start_idx: 0,
            end_idx: 0,
        }
    }
}

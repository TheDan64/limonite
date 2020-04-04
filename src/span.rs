use crate::interner::StrId;

#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub(crate) fn new(node: T, span: Span) -> Self {
        Spanned {
            node,
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn get_span_mut(&mut self) -> &mut Span {
        &mut self.span
    }

    pub fn get_node(&self) -> &T {
        &self.node
    }

    pub fn map<F: FnOnce(&T) -> R, R>(&self, f: F) -> Spanned<R> {
        Spanned::new(f(&self.node), self.span)
    }
}

impl<'s> Spanned<&'s str> {
    pub fn new_start(&mut self, start_idx: usize, input: &'s str) {
        debug_assert!(start_idx < self.span.end_idx);

        self.span.new_start(start_idx);
        self.node = &input[start_idx..=self.span.end_idx];
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

impl<T: Default> Spanned<T> {
    pub(crate) fn default_with_file_id(file_id: StrId) -> Self {
        let mut spanned = Spanned::default();

        spanned.span.file_id = file_id;
        spanned
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
    file_id: StrId,
    pub(crate) start_idx: usize,
    pub(crate) end_idx: usize,
}

impl Span {
    pub(crate) fn new(file_id: StrId, start_idx: usize, end_idx: usize) -> Self {
        Span { file_id, start_idx, end_idx }
    }

    pub fn new_start(&mut self, start_idx: usize) {
        self.start_idx = start_idx;
    }

    #[cfg(test)]
    pub fn indexes(&self) -> (usize, usize) {
        (self.start_idx, self.end_idx)
    }
}

impl Default for Span {
    fn default() -> Self {
        Span {
            file_id: StrId::DUMMY,
            start_idx: 0,
            end_idx: 0,
        }
    }
}

use crate::interner::StrId;

use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::ops::Index;

#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
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

    pub fn replace<R>(&self, r: R) -> Spanned<R> {
        self.map(|_| r)
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

#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    pub(crate) file_id: StrId,
    pub(crate) start_idx: usize,
    pub(crate) end_idx: usize,
}

impl Span {
    pub fn new(file_id: StrId, start_idx: usize, end_idx: usize) -> Self {
        Span { file_id, start_idx, end_idx }
    }

    pub fn new_start(&mut self, start_idx: usize) {
        self.start_idx = start_idx;
    }

    pub fn width(&self) -> usize {
        // Inclusive range, so must add 1
        self.end_idx - self.start_idx + 1
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

impl Index<Span> for str {
    type Output = str;

    fn index(&self, span: Span) -> &Self::Output {
        &self.index(span.start_idx..=span.end_idx)
    }
}

// Compact Span repr so that it's easier to read in debug output.
// Omitting file_id at least for now since there's currently only
// support for a single file.
impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Span")
            // .field(&self.file_id)
            .field(&format_args!("{}..={}", self.start_idx, self.end_idx))
            .finish()
    }
}

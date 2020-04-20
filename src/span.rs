use crate::interner::StrId;

use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::ops::Index;

// REVIEW: Shouldn't need to be pub?

pub struct StartIdx(usize);
pub struct EndIdx(usize);

#[derive(PartialEq)]
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

    pub fn boxed(node: T, span: Span) -> Spanned<Box<T>> {
        Spanned {
            node: Box::new(node),
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

    pub fn start_idx(&self) -> usize {
        self.span().start_idx
    }

    pub fn end_idx(&self) -> usize {
        self.span().end_idx
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

// Compact Spanned repr so that it's easier to read in debug output.
impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Spanned")
            .field(&self.node)
            .field(&self.span)
            .finish()
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    pub(crate) file_id: StrId,
    pub(crate) start_idx: usize,
    pub(crate) end_idx: usize,
}

impl Span {
    pub fn new<F: Into<StrId>, S: Into<StartIdx>, E: Into<EndIdx>>(file_id: F, start_idx: S, end_idx: E) -> Self {
        Span { file_id: file_id.into(), start_idx: start_idx.into().0, end_idx: end_idx.into().0 }
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

impl<T> From<Spanned<T>> for StrId {
    fn from(sp: Spanned<T>) -> Self {
        sp.span().file_id
    }
}

impl<T> From<Spanned<T>> for StartIdx {
    fn from(sp: Spanned<T>) -> Self {
        StartIdx(sp.span().start_idx)
    }
}

impl<T> From<Spanned<T>> for EndIdx {
    fn from(sp: Spanned<T>) -> Self {
        EndIdx(sp.span().end_idx)
    }
}

impl From<usize> for StartIdx {
    fn from(u: usize) -> Self {
        StartIdx(u)
    }
}

impl From<usize> for EndIdx {
    fn from(u: usize) -> Self {
        EndIdx(u)
    }
}

impl From<Span> for StrId {
    fn from(sp: Span) -> Self {
        sp.file_id
    }
}

impl From<Span> for StartIdx {
    fn from(sp: Span) -> Self {
        StartIdx(sp.start_idx)
    }
}

impl From<Span> for EndIdx {
    fn from(sp: Span) -> Self {
        EndIdx(sp.end_idx)
    }
}

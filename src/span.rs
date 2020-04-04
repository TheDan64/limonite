#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    // TODO
}

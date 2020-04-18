use crate::span::Spanned;
use crate::syntax::{Block, Type};

pub type Item<'s> = Spanned<ItemKind<'s>>;

pub enum ItemKind<'s> {
    // Declare a function with a name, signature, and body
    Fn(Spanned<&'s str>, /* FnSig, */ Block<'s>),
    // Declare a struct with a name and fields
    Struct(Spanned<&'s str>, Vec<(Spanned<&'s str>, Type)>),
    Use,
}


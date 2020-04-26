use crate::span::Spanned;
use crate::syntax::{Block, Type};

pub type Item<'s> = Spanned<ItemKind<'s>>;

// TODO: Better place for this
#[derive(Debug, PartialEq)]
pub struct FnSig<'s> {
    params: Vec<(Spanned<&'s str>, Type<'s>)>,
    ret: Option<Type<'s>>,
}

impl<'s> FnSig<'s> {
    pub fn new(params: Vec<(Spanned<&'s str>, Type<'s>)>, ret: Option<Type<'s>>) -> Self {
        FnSig { params, ret }
    }
}

#[derive(Debug, PartialEq)]
pub enum ItemKind<'s> {
    // Define a function with a name, signature, and body
    FnDef(Spanned<&'s str>, Spanned<FnSig<'s>>, Block<'s>),
    // Define a struct with a name and fields
    StructDef(Spanned<&'s str>, Vec<(Spanned<&'s str>, Type<'s>)>),
    Use,
}


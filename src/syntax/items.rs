use crate::span::Spanned;
use crate::syntax::{Block, Type};

pub type Item<'s> = Spanned<ItemKind<'s>>;

// TODO: Better place for this
#[derive(Clone, Debug, PartialEq)]
pub struct FnSig<'s> {
    params: Vec<(Spanned<&'s str>, Type<'s>)>,
    ret: Option<Type<'s>>,
}

impl<'s> FnSig<'s> {
    pub fn new(params: Vec<(Spanned<&'s str>, Type<'s>)>, ret: Option<Type<'s>>) -> Self {
        FnSig { params, ret }
    }

    pub fn return_type(&self) -> Option<&Type<'s>> {
        self.ret.as_ref()
    }

    pub fn params(&self) -> &[(Spanned<&'s str>, Type<'s>)] {
        self.params.as_ref()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind<'s> {
    // Define a function with a name, signature, and body
    FnDef(Spanned<&'s str>, Spanned<FnSig<'s>>, Block<'s>),
    // Define a struct with a name and fields
    StructDef(Spanned<&'s str>, Vec<(Spanned<&'s str>, Type<'s>)>),
    // Import a function or type into scope. Use keyword and double colon (rust-style) path
    Use(Spanned<()>, Vec<Spanned<&'s str>>),
}


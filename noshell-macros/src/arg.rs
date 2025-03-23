//! Argument parsing and manipulation.

use proc_macro2::Span;
use syn::{Field, Ident, Type, spanned::Spanned};

use crate::attr::Attr;

/// Argument metadata.
pub struct MetaArg {
    /// Argument span.
    pub span: Span,

    /// Argument identifier (i.e. field identifier).
    pub id: Ident,

    /// Type.
    pub ty: Type,

    /// Attributes.
    pub attrs: Vec<Attr>,
}

impl MetaArg {
    /// Create a new argument metadata.
    pub fn new(field: &Field, attrs: Vec<Attr>) -> Self {
        MetaArg {
            span: field.span(),
            id: field.ident.clone().unwrap(),
            ty: field.ty.clone(),
            attrs,
        }
    }
}

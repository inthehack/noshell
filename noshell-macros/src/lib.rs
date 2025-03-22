//! noshell Parser.

#![deny(missing_docs)]

use proc_macro::TokenStream;

mod attr;
mod derive;
mod helpers;
mod ty;

/// `Parser` derive macro.
#[proc_macro_derive(Parser, attributes(arg))]
pub fn derive_parser(item: TokenStream) -> TokenStream {
    derive::run(item.into()).into()
}

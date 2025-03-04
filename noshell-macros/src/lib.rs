use proc_macro::TokenStream;

mod derive;
mod helpers;

#[proc_macro_derive(Parser, attributes(arg))]
pub fn derive_parser(item: TokenStream) -> TokenStream {
    derive::run(item.into()).into()
}

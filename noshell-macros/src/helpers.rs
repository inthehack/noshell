use std::fmt::Display;

use proc_macro2::TokenStream;
use quote::ToTokens;

pub fn token_stream_with_error(mut tokens: TokenStream, err: syn::Error) -> TokenStream {
    tokens.extend(err.into_compile_error());
    tokens
}

pub fn error<A: ToTokens, T: Display>(tokens: &mut TokenStream, obj: A, msg: T) {
    tokens.extend(syn::Error::new_spanned(obj.into_token_stream(), msg).into_compile_error())
}

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    ext::IdentExt, spanned::Spanned, Data, DataStruct, DeriveInput, Field, Fields, Path,
    PathArguments, Type, TypePath,
};

use crate::helpers::token_stream_with_error;

pub fn run(item: TokenStream) -> TokenStream {
    let errors = TokenStream::new();

    let input: DeriveInput = match syn::parse2(item.clone()) {
        Ok(x) => x,
        Err(e) => return token_stream_with_error(item, e),
    };

    let generated = match build_struct_parser(&input) {
        Ok(x) => x,
        Err(e) => return token_stream_with_error(item, e),
    };

    quote! {
        #generated

        #errors
    }
}

fn build_struct_parser(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &input.ident;

    let args = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => fields
            .named
            .iter()
            .map(build_arg_parser)
            .collect::<Result<Vec<_>, syn::Error>>()?,

        _ => {
            return Err(syn::Error::new(input.span(), "invalid arg struct"));
        }
    };

    Ok(quote! {
        #[automatically_derived]
        impl #ident {
            fn parse<'a>(__argv: &'a [&'a str]) -> Self {
                let __tokens = noshell::Lexer::new(__argv);
                let __args = noshell::ParsedArgs::parse(__tokens);

                #ident {
                    #(
                        #args
                    ),*
                }
            }
        }
    })
}

fn build_arg_parser(field: &Field) -> Result<TokenStream, syn::Error> {
    let ty = &field.ty;
    let args = format_ident!("__args");

    let ident = field.ident.clone().unwrap();
    let id = ident.unraw().to_string();

    let value = if is_option_ty(ty) {
        quote_spanned! { ty.span()=>
            #args.get(#id)?
        }
    } else {
        quote_spanned! { ty.span()=>
            #args.get(#id)
                .ok_or_else(||
                    syn::Error::new(Span::new(), concat!("missing required value for: ", #id)))?
        }
    };

    Ok(quote_spanned! {ty.span()=>
        #ident: #value
    })
}

fn is_option_ty(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => {
            // Look for the last type component in a::b::type.
            if let Some(last) = segments.last() {
                // Check that the type identifier is actually `Option`.
                if last.ident == "Option" {
                    // Check that the `Option` type has arguments inside brackets (i.e. <...>).
                    if let PathArguments::AngleBracketed(args) = &last.arguments {
                        return args.args.last().is_some();
                    }
                }
            }

            // Not type path.
            false
        }

        // Not a type.
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_check_option_type() {
        let ty = syn::parse_quote!(Option<i32>);
        assert!(is_option_ty(&ty));

        let ty = syn::parse_quote!(core::ops::Option<i32>);
        assert!(is_option_ty(&ty));

        let ty = syn::parse_quote!(Option<mod1::Type1>);
        assert!(is_option_ty(&ty));

        let ty = syn::parse_quote!(i32);
        assert!(!is_option_ty(&ty));

        let ty = syn::parse_quote!(Result<i32, Error>);
        assert!(!is_option_ty(&ty));
    }
}

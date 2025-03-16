use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::{
    Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Path, PathArguments, Type, TypePath,
    spanned::Spanned,
};

use crate::helpers::{error, token_stream_with_error};

pub fn run(item: TokenStream) -> TokenStream {
    let input: DeriveInput = match syn::parse2(item.clone()) {
        Ok(x) => x,
        Err(e) => return token_stream_with_error(item, e),
    };

    run_derive(&input).unwrap_or_else(|err| {
        let mut errors = TokenStream::new();
        error(&mut errors, &input, err.to_string());
        quote! {
            #errors
        }
    })
}

pub fn run_derive(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &input.ident;

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let fields = collect_arg_fields(fields)?;
            let init = build_arg_parsers(&fields)?;

            Ok(quote! {
                impl #ident {
                    pub fn parse<'a>(__argv: &'a [&'a str]) -> Result<Self, noshell::Error> {
                        use noshell::parser::ParsedArgs;

                        let __tokens = noshell::parser::Tokens::new(__argv);
                        let __args: ParsedArgs<'_, 64> = noshell::parser::ParsedArgs::parse(__tokens);

                        Ok(#ident #init)
                    }
                }
            })
        }

        // FIXME: do not support unamed struct or enum.
        _ => {
            let span = proc_macro2::Span::call_site();
            let error = syn::Error::new(span, "#[derive(Parser)] only support named structs");
            Err(error)
        }
    }
}

fn collect_arg_fields(fields: &FieldsNamed) -> Result<Vec<&Field>, syn::Error> {
    Ok(fields.named.iter().collect())
}

fn build_arg_parsers(fields: &[&Field]) -> Result<TokenStream, syn::Error> {
    let args = fields
        .iter()
        .map(|&field| build_arg_parser(field))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {{
        #(
            #args
        ),*
    }})
}

fn build_arg_parser(field: &Field) -> Result<TokenStream, syn::Error> {
    let ty = &field.ty;
    let args = format_ident!("__args");

    let ident = field.ident.clone().unwrap();
    let id = ident.unraw().to_string();

    let value = if is_option_ty(ty) {
        quote_spanned! { ty.span()=>
            #args.try_get_one(#id)?
        }
    } else {
        quote_spanned! { ty.span()=>
            #args.try_get_one(#id)?.ok_or_else(|| noshell::parser::Error::MissingArgument)?
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

    #[test]
    fn it_should_build_field_parser() {
        let field = syn::parse_quote!(value: u32);
        assert_eq!(
            quote!(
                value: __args.try_get_one("value")?
                    .ok_or_else(|| noshell::parser::Error::MissingArgument)?
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_option_field_parser() {
        let field = syn::parse_quote!(value: Option<u32>);
        assert_eq!(
            quote!(value: __args.try_get_one("value")?).to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_option_field_parser_with_compound_type() {
        let field = syn::parse_quote!(value: Option<mod1::mod2::u32>);
        assert_eq!(
            quote!(value: __args.try_get_one("value")?).to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_struct_derive() {
        let derive = syn::parse_quote! {
            struct MyArgs {
                field1: u32,
                field2: Option<u32>,
            }
        };

        assert_eq!(
            quote! {
                impl MyArgs {
                    pub fn parse<'a>(__argv: &'a [&'a str]) -> Result<Self, noshell::Error> {
                        use noshell::parser::ParsedArgs;

                        let __tokens = noshell::parser::Tokens::new(__argv);
                        let __args: ParsedArgs<'_, 64> = noshell::parser::ParsedArgs::parse(__tokens);

                        Ok(MyArgs {
                            field1: __args.try_get_one("field1")?
                                .ok_or_else(|| noshell::parser::Error::MissingArgument)?,
                            field2: __args.try_get_one("field2")?
                        })
                    }
                }
            }
            .to_string(),
            run_derive(&derive).unwrap().to_string()
        )
    }
}

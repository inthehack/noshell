use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::{Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, spanned::Spanned};

use crate::helpers::{error, token_stream_with_error};
use crate::ty::Ty;

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

    let value = match Ty::from_syn_ty(ty) {
        Ty::Option => quote_spanned! { ty.span()=>
            #args.try_get_one(#id)?
        },

        Ty::OptionOption => quote_spanned! { ty.span()=>
            if #args.contains(#id) {
                Some(#args.try_get_one(#id)?)
            } else {
                None
            }
        },

        Ty::OptionVec => quote_spanned! { ty.span()=>
            if #args.contains(#id) {
                Some(#args.try_get_many::<Vec<_>, _>(#id)?)
            } else {
                None
            }
        },

        Ty::Vec => quote_spanned! { ty.span()=>
            #args.try_get_many::<Vec<_>, _>(#id)?
        },

        Ty::Simple => quote_spanned! { ty.span()=>
            #args.try_get_one(#id)?.ok_or_else(|| noshell::parser::Error::MissingArgument)?
        },
    };

    Ok(quote_spanned! { field.span()=>
        #ident: #value
    })
}

#[cfg(test)]
mod tests {
    use super::*;

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

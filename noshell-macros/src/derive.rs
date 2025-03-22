use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::{Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Fields, FieldsNamed, Lit, spanned::Spanned};

use crate::attr::{Attr, AttrKind, AttrName, AttrValue};
use crate::helpers::{error, token_stream_with_error};
use crate::ty::{Ty, get_inner_ty};

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

// This is the default value.
const PARSER_ARG_COUNT_MAX: usize = 16;

pub fn run_derive(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &input.ident;

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let fields = collect_arg_fields(fields)?;
            let init = build_arg_parsers(&fields)?;

            let attrs = Attr::parse_all(&input.attrs)?;

            let limit = parse_attr_of_expr_lit_with(
                    &attrs,
                    AttrKind::NoShell,
                    AttrName::Limit,
                    |x| if let Lit::Int(val) = x {
                        val.base10_parse().ok()
                    } else {
                        None
                    }
                )
                .unwrap_or(PARSER_ARG_COUNT_MAX);

            Ok(quote! {
                impl #ident {
                    pub fn parse<'a>(__argv: &'a [&'a str]) -> Result<Self, noshell::Error> {
                        use noshell::parser::ParsedArgs;

                        let __tokens = noshell::parser::Tokens::new(__argv);
                        let __args: ParsedArgs<'_, #limit> = noshell::parser::ParsedArgs::parse(__tokens);

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
    let inner_ty = get_inner_ty(ty);

    let args = format_ident!("__args");
    let try_get_one = quote_spanned!(inner_ty.span()=> try_get_one::<#inner_ty>);
    let try_get_many = quote_spanned!(inner_ty.span()=> try_get_many::<_, #inner_ty>);

    let ident = field.ident.clone().unwrap();
    let id = ident.unraw().to_string();

    let value = match Ty::from_syn_ty(ty) {
        // Optional argument with required value.
        Ty::Option => quote_spanned! { ty.span()=>
            if #args.contains(#id) {
                Some(
                    #args.#try_get_one(#id)
                        .map(Option::unwrap)
                        .and_then(noshell::parser::utils::check_value_is_missing)
                        .map(Option::unwrap)?
                )
            } else {
                None
            }
        },

        // Optional argument with optional value.
        Ty::OptionOption => quote_spanned! { ty.span()=>
            if #args.contains(#id) {
                Some(
                    #args.#try_get_one(#id).map(Option::flatten)?
                )
            } else {
                None
            }
        },

        // Optional argument with required non-empty sequence of values.
        Ty::OptionVec => quote_spanned! { ty.span()=>
            if #args.contains(#id) {
                Some(
                    #args.#try_get_many(#id)
                        .map(Option::unwrap)
                        .and_then(noshell::parser::utils::check_vec_is_missing)?
                )
            } else {
                None
            }
        },

        // Required argument with required non-empty sequence of values.
        Ty::Vec => quote_spanned! { ty.span()=>
            #args.#try_get_many(#id)
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_vec_is_missing)?
        },

        // Required argument with required value.
        Ty::Simple => quote_spanned! { ty.span()=>
            #args.#try_get_one(#id)
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_value_is_missing)
                .map(Option::unwrap)?
        },
    };

    Ok(quote_spanned! { field.span()=>
        #ident: #value
    })
}

fn parse_attr_of_expr_lit_with<T, F>(attrs: &[Attr], kind: AttrKind, name: AttrName, parser: F) -> Option<T>
where
    F: FnOnce(&Lit) -> Option<T>,
{
    attrs.iter()
        .find(|x| x.kind == kind && x.name == Some(name))
        .and_then(|x|
            match &x.value {
                Some(AttrValue::Expr(Expr::Lit(ExprLit {
                    lit,
                    ..
                }))) => parser(lit),

                _ => None,
            }
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_build_parser_for_simple_type() {
        let field = syn::parse_quote!(value: u32);
        assert_eq!(
            quote!(
                value: __args.try_get_one::<u32>("value")
                    .and_then(noshell::parser::utils::check_arg_is_missing)
                    .map(Option::unwrap)
                    .and_then(noshell::parser::utils::check_value_is_missing)
                    .map(Option::unwrap)?
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_parser_for_option_type() {
        let field = syn::parse_quote!(value: Option<u32>);
        assert_eq!(
            quote!(
                value: if __args.contains("value") {
                    Some(
                        __args.try_get_one::<u32>("value")
                            .map(Option::unwrap)
                            .and_then(noshell::parser::utils::check_value_is_missing)
                            .map(Option::unwrap)?
                    )
                } else {
                    None
                }
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_parser_for_option_option_type() {
        let field = syn::parse_quote!(value: Option<Option<u32>>);
        assert_eq!(
            quote!(value:
                if __args.contains("value") {
                    Some(
                        __args.try_get_one::<u32>("value").map(Option::flatten)?
                    )
                } else {
                    None
                }
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_parser_for_option_vec_type() {
        let field = syn::parse_quote!(value: Option<Vec<u32>>);
        assert_eq!(
            quote!(value:
                if __args.contains("value") {
                    Some(
                        __args.try_get_many::<_, u32>("value")
                            .map(Option::unwrap)
                            .and_then(noshell::parser::utils::check_vec_is_missing)?
                    )
                } else {
                    None
                }
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_parser_for_vec_type() {
        let field = syn::parse_quote!(value: Vec<u32, 8>);
        assert_eq!(
            quote!(
                value: __args.try_get_many::<_, u32>("value")
                    .and_then(noshell::parser::utils::check_arg_is_missing)
                    .map(Option::unwrap)
                    .and_then(noshell::parser::utils::check_vec_is_missing)?
            )
            .to_string(),
            build_arg_parser(&field).unwrap().to_string()
        );
    }

    #[test]
    fn it_should_build_struct_derive() {
        let derive = syn::parse_quote! {
            struct MyArgs {
                value1: u32,
                value2: u32,
            }
        };

        assert_eq!(
            quote! {
                impl MyArgs {
                    pub fn parse<'a>(__argv: &'a [&'a str]) -> Result<Self, noshell::Error> {
                        use noshell::parser::ParsedArgs;

                        let __tokens = noshell::parser::Tokens::new(__argv);
                        let __args: ParsedArgs<'_, 16usize> = noshell::parser::ParsedArgs::parse(__tokens);

                        Ok(MyArgs {
                            value1: __args.try_get_one::<u32>("value1")
                                .and_then(noshell::parser::utils::check_arg_is_missing)
                                .map(Option::unwrap)
                                .and_then(noshell::parser::utils::check_value_is_missing)
                                .map(Option::unwrap)?,
                                
                            value2: __args.try_get_one::<u32>("value2")
                                .and_then(noshell::parser::utils::check_arg_is_missing)
                                .map(Option::unwrap)
                                .and_then(noshell::parser::utils::check_value_is_missing)
                                .map(Option::unwrap)?
                        })
                    }
                }
            }
            .to_string(),
            run_derive(&derive).unwrap().to_string()
        )
    }
}

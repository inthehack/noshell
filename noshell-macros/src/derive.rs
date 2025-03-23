//! Derive macro implementation.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::LitStr;
use syn::ext::IdentExt;
use syn::{
    Data, DataStruct, DeriveInput, Expr, ExprLit, Fields, FieldsNamed, Lit, spanned::Spanned,
};

use crate::arg::MetaArg;
use crate::attr::{Attr, AttrKind, AttrName, AttrValue};
use crate::helpers::{error, token_stream_with_error};
use crate::ty::{Ty, get_inner_ty};

pub fn run(item: TokenStream) -> TokenStream {
    let input: DeriveInput = match syn::parse2(item.clone()) {
        Ok(x) => x,
        Err(e) => return token_stream_with_error(item, e),
    };

    try_run(&input).unwrap_or_else(|err| {
        let mut errors = TokenStream::new();
        error(&mut errors, &input, err.to_string());
        quote! {
            #errors
        }
    })
}

// This is the default value.
const PARSER_ARG_COUNT_MAX: usize = 16;

pub fn try_run(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &input.ident;

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let args = collect_args_meta(fields)?;
            let init = build_args_init(&args)?;

            check_args_duplicate_flags(&args)?;

            let attrs = Attr::parse_all(&input.attrs)?;
            let limit = get_noshell_attr_limit_arg_value(&attrs)?.unwrap_or(PARSER_ARG_COUNT_MAX);

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

fn collect_args_meta(fields: &FieldsNamed) -> Result<Vec<MetaArg>, syn::Error> {
    let meta = fields
        .named
        .iter()
        .map(|x| {
            let attrs = Attr::parse_all(&x.attrs)?;
            Result::<_, syn::Error>::Ok(MetaArg::new(x, attrs))
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(meta)
}

fn build_args_init(fields: &[MetaArg]) -> Result<TokenStream, syn::Error> {
    let args = fields
        .iter()
        .map(build_arg_parser)
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {{
        #(
            #args
        ),*
    }})
}

fn build_arg_parser(arg: &MetaArg) -> Result<TokenStream, syn::Error> {
    let ty = &arg.ty;
    let inner_ty = get_inner_ty(ty);

    let args = format_ident!("__args");
    let try_get_one = quote_spanned!(inner_ty.span()=> try_get_one::<#inner_ty>);
    let try_get_many = quote_spanned!(inner_ty.span()=> try_get_many::<_, #inner_ty>);

    let ident = arg.id.unraw();
    let id = ident.to_string();

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

    Ok(quote_spanned! { arg.span=>
        #ident: #value
    })
}

fn find_attr_with<P>(attrs: &[Attr], mut predicate: P) -> Option<&Attr>
where
    P: FnMut(&Attr) -> bool,
{
    attrs.iter().find(|&x| predicate(x))
}

fn parse_attr_of_literal_string_with<T, P>(attr: &Attr, parser: P) -> Result<T, syn::Error>
where
    P: FnOnce(&LitStr) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::LitStr(lit)) => lit,
        _ => return Err(syn::Error::new(attr.id.span(), "expected a literal value")),
    };

    parser(lit)
}

fn parse_attr_of_literal_expr_with<T, P>(attr: &Attr, parser: P) -> Result<T, syn::Error>
where
    P: FnOnce(&Lit) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::Expr(Expr::Lit(ExprLit { lit, .. }))) => lit,
        _ => return Err(syn::Error::new(attr.id.span(), "expected a literal value")),
    };

    parser(lit)
}

fn parse_noshell_attr_limit_arg(attr: &Attr) -> Result<usize, syn::Error> {
    parse_attr_of_literal_expr_with(attr, |lit| {
        if let Lit::Int(val) = lit {
            val.base10_parse()
                .map_err(|_| syn::Error::new(attr.id.span(), "expected an unsigned integer"))
        } else {
            Err(syn::Error::new(
                attr.id.span(),
                "expected arg `limit` as a literal integer",
            ))
        }
    })
}

fn get_noshell_attr_limit_arg_value(attrs: &[Attr]) -> Result<Option<usize>, syn::Error> {
    if let Some(attr) = find_attr_with(attrs, |x| {
        x.kind == AttrKind::NoShell && x.name == Some(AttrName::Limit)
    }) {
        return Ok(Some(parse_noshell_attr_limit_arg(attr)?));
    }

    Ok(None)
}

fn parse_attr_arg_short_arg(attr: &Attr) -> Result<char, syn::Error> {
    parse_attr_of_literal_expr_with(attr, |lit| {
        if let Lit::Char(val) = lit {
            Ok(val.value())
        } else {
            Err(syn::Error::new(
                attr.id.span(),
                "expected `short` to be a character",
            ))
        }
    })
}

fn parse_attr_arg_long_arg(attr: &Attr) -> Result<String, syn::Error> {
    parse_attr_of_literal_string_with(attr, |lit| Ok(lit.value()))
}

fn check_args_duplicate_flags(args: &[MetaArg]) -> Result<(), syn::Error> {
    let mut short_keys: HashSet<char> = HashSet::new();
    let mut long_keys: HashSet<String> = HashSet::new();

    for arg in args {
        // Check for short flags.
        let shorts = arg
            .attrs
            .iter()
            .filter(|x| x.kind == AttrKind::Arg && x.name == Some(AttrName::Short))
            .map(|attr| Result::<_, syn::Error>::Ok((attr, parse_attr_arg_short_arg(attr)?)))
            .collect::<Result<Vec<_>, _>>()?;

        for (attr, key) in shorts {
            if !short_keys.insert(key) {
                return Err(syn::Error::new(
                    attr.id.span(),
                    format!("short flag `{}` is already used elsewhere", key),
                ));
            }
        }

        // Check for long flags.
        let longs = arg
            .attrs
            .iter()
            .filter(|x| x.kind == AttrKind::Arg && x.name == Some(AttrName::Long))
            .map(|attr| Result::<_, syn::Error>::Ok((attr, parse_attr_arg_long_arg(attr)?)))
            .collect::<Result<Vec<_>, _>>()?;

        for (attr, key) in &longs {
            if !long_keys.insert(key.clone()) {
                return Err(syn::Error::new(
                    attr.id.span(),
                    format!("long flag `{}` is already used elsewhere", key),
                ));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use syn::Field;

    use super::*;

    #[test]
    fn it_should_build_parser_for_simple_type() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote!(value: u32);

        let attrs = Attr::parse_all(&field.attrs)?;
        let meta = MetaArg::new(&field, attrs);
        let given = build_arg_parser(&meta)?;

        let expected = quote!(
            value: __args.try_get_one::<u32>("value")
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_value_is_missing)
                .map(Option::unwrap)?
        );

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_parser_for_option_type() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote!(value: Option<u32>);

        let attrs = Attr::parse_all(&field.attrs)?;
        let meta = MetaArg::new(&field, attrs);
        let given = build_arg_parser(&meta)?;

        let expected = quote!(
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
        );

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_parser_for_option_option_type() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote!(value: Option<Option<u32>>);

        let attrs = Attr::parse_all(&field.attrs)?;
        let meta = MetaArg::new(&field, attrs);
        let given = build_arg_parser(&meta)?;

        let expected = quote!(value:
            if __args.contains("value") {
                Some(
                    __args.try_get_one::<u32>("value").map(Option::flatten)?
                )
            } else {
                None
            }
        );

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_parser_for_option_vec_type() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote!(value: Option<Vec<u32>>);

        let attrs = Attr::parse_all(&field.attrs)?;
        let meta = MetaArg::new(&field, attrs);
        let given = build_arg_parser(&meta)?;

        let expected = quote!(value:
            if __args.contains("value") {
                Some(
                    __args.try_get_many::<_, u32>("value")
                        .map(Option::unwrap)
                        .and_then(noshell::parser::utils::check_vec_is_missing)?
                )
            } else {
                None
            }
        );

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_parser_for_vec_type() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote!(value: Vec<u32, 8>);

        let attrs = Attr::parse_all(&field.attrs)?;
        let meta = MetaArg::new(&field, attrs);
        let given = build_arg_parser(&meta)?;

        let expected = quote!(
            value: __args.try_get_many::<_, u32>("value")
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_vec_is_missing)?
        );

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_struct_derive() -> anyhow::Result<()> {
        let derive: DeriveInput = syn::parse_quote! {
            struct MyArgs {
                value1: u32,
                value2: u32,
            }
        };

        let given = try_run(&derive)?;

        let expected = quote! {
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
        };

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }
}

//! Derive macro implementation.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::{
    Data, DataStruct, DeriveInput, Expr, ExprLit, Fields, FieldsNamed, Lit, LitStr,
    spanned::Spanned,
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

            let (ids_ty, ids) = parse_and_build_arg_ids(&args)?;

            let attrs = Attr::parse_all(&input.attrs)?;
            let limit = get_noshell_attr_limit_arg_value(&attrs)?.unwrap_or(PARSER_ARG_COUNT_MAX);

            Ok(quote! {
                impl #ident {
                    pub fn parse<'a>(__argv: &'a [&'a str]) -> Result<Self, noshell::Error> {
                        use noshell::parser::ParsedArgs;

                        let __ids = Self::ids();
                        let __tokens = noshell::parser::Tokens::new(__argv);
                        let __args: ParsedArgs<'_, #limit> = noshell::parser::ParsedArgs::parse(__tokens, __ids);

                        Ok(#ident #init)
                    }

                    pub fn ids() -> &'static [(noshell::parser::lexer::Flag<'static>, &'static str)] {
                        static IDS: #ids_ty = #ids;
                        &IDS
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

fn parse_attr_of_literal_string_with<T, P>(attr: &Attr, parser: P) -> Result<Option<T>, syn::Error>
where
    P: FnOnce(&LitStr) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::LitStr(lit)) => lit,
        _ => return Ok(None),
    };

    parser(lit).map(Some)
}

fn parse_attr_of_literal_expr_with<T, P>(attr: &Attr, parser: P) -> Result<Option<T>, syn::Error>
where
    P: FnOnce(&Lit) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::Expr(Expr::Lit(ExprLit { lit, .. }))) => lit,
        _ => return Ok(None),
    };

    parser(lit).map(Some)
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
    .and_then(|x| {
        x.ok_or_else(|| {
            syn::Error::new(
                attr.id.span(),
                "missing value of limit in `noshell` attribute",
            )
        })
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

fn parse_attr_arg_short_arg(attr: &Attr) -> Result<Option<char>, syn::Error> {
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

fn parse_attr_arg_long_arg(attr: &Attr) -> Result<Option<String>, syn::Error> {
    parse_attr_of_literal_string_with(attr, |lit| Ok(lit.value()))
}

fn parse_and_build_arg_ids(args: &[MetaArg]) -> Result<(TokenStream, TokenStream), syn::Error> {
    let mut items = TokenStream::new();

    let mut short_keys: HashSet<char> = HashSet::new();
    let mut long_keys: HashSet<String> = HashSet::new();

    let mut ids_size: usize = 0;

    for arg in args {
        // The argument identifier.
        let id = arg.id.unraw().to_string();

        // Check for short flags.
        let shorts = arg
            .attrs
            .iter()
            .filter(|x| x.kind == AttrKind::Arg && x.name == Some(AttrName::Short))
            .map(|attr| Result::<_, syn::Error>::Ok((attr, parse_attr_arg_short_arg(attr)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let mut errors = None;

        for (i, (attr, key)) in shorts.iter().enumerate() {
            // SAFETY: one can ensure that a field identifier has at least one character.
            let key = key.unwrap_or_else(|| id.chars().next().unwrap());

            if !short_keys.insert(key) {
                return Err(syn::Error::new(
                    attr.id.span(),
                    format!("short flag `{}` is already used elsewhere", key),
                ));
            }

            let flag = quote!(noshell::parser::lexer::Flag::Short(#key));
            let pair = quote!( ( #flag, #id ), );
            pair.to_tokens(&mut items);

            if i > 0 {
                if errors.is_none() {
                    errors = Some(syn::Error::new(
                        attr.id.span(),
                        "must have at most one short flag",
                    ));
                }

                errors.as_mut().unwrap().combine(syn::Error::new(
                    attr.id.span(),
                    "another short flag is defined here",
                ));
            }

            // Add a short flag id.
            ids_size += 1;
        }

        if let Some(err) = errors {
            return Err(err);
        }

        // Check for long flags.
        let longs = arg
            .attrs
            .iter()
            .filter(|x| x.kind == AttrKind::Arg && x.name == Some(AttrName::Long))
            .map(|attr| Result::<_, syn::Error>::Ok((attr, parse_attr_arg_long_arg(attr)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let mut errors = None;

        for (i, (attr, key)) in longs.iter().enumerate() {
            // SAFETY: one can ensure that a field identifier has at least one character.
            let key = key.as_ref().unwrap_or(&id);

            if !long_keys.insert(key.clone()) {
                return Err(syn::Error::new(
                    attr.id.span(),
                    format!("long flag `{}` is already used elsewhere", key),
                ));
            }

            let flag = quote!(noshell::parser::lexer::Flag::Long(#key));
            let pair = quote!( ( #flag, #id ), );
            pair.to_tokens(&mut items);

            if i > 0 {
                if errors.is_none() {
                    errors = Some(syn::Error::new(
                        attr.id.span(),
                        "must have at most one long flag",
                    ));
                }

                errors.as_mut().unwrap().combine(syn::Error::new(
                    attr.id.span(),
                    "another long flag is defined here",
                ));
            }

            // Add a long flag id.
            ids_size += 1;
        }

        // If the argument has no defined short or long flag, add a long flag by default. This
        // default long flag has the same value as the field.
        if shorts.is_empty() && longs.is_empty() {
            if !long_keys.insert(id.clone()) {
                return Err(syn::Error::new(
                    arg.id.span(),
                    format!("long flag `{}` is already used elsewhere", id),
                ));
            }

            let flag = quote!(noshell::parser::lexer::Flag::Long(#id));
            let pair = quote!( ( #flag, #id ), );
            pair.to_tokens(&mut items);

            // Add a long flag id.
            ids_size += 1;
        }
    }

    let ids_ty = quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); #ids_size] };
    let ids = quote! { [ #items ] };

    Ok((ids_ty, ids))
}

#[cfg(test)]
mod tests {
    use syn::Field;

    use super::*;

    #[test]
    fn it_should_build_id_lookup_table_with_no_attrs() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(0, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 1usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [(noshell::parser::lexer::Flag::Long("value"), "value"),] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_id_lookup_table_with_one_default_long_flag() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            #[arg(long)]
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(1, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 1usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [(noshell::parser::lexer::Flag::Long("value"), "value"),] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_id_lookup_table_with_one_long_flag() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            #[arg(long = "other")]
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(1, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 1usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [(noshell::parser::lexer::Flag::Long("other"), "value"),] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_id_lookup_table_with_one_default_short_flag() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            #[arg(short)]
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(1, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 1usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [(noshell::parser::lexer::Flag::Short('v'), "value"),] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_id_lookup_table_with_one_short_flag() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            #[arg(short = 'd')]
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(1, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 1usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [(noshell::parser::lexer::Flag::Short('d'), "value"),] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

    #[test]
    fn it_should_build_id_lookup_table_with_one_short_and_one_long_flags() -> anyhow::Result<()> {
        let field: Field = syn::parse_quote! {
            #[arg(short, long)]
            value: u32
        };

        let attrs = Attr::parse_all(&field.attrs)?;
        assert_eq!(2, attrs.len());

        let meta = MetaArg::new(&field, attrs);
        let (ids_ty, ids) = parse_and_build_arg_ids(&[meta])?;

        let expected_ty =
            quote! { [(noshell::parser::lexer::Flag<'static>, &'static str); 2usize] };
        assert_eq!(expected_ty.to_string(), ids_ty.to_string());

        let expected = quote! { [
            (noshell::parser::lexer::Flag::Short('v'), "value"),
            (noshell::parser::lexer::Flag::Long("value"), "value"),
        ] };
        assert_eq!(expected.to_string(), ids.to_string());

        Ok(())
    }

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

                    let __ids = Self::ids();
                    let __tokens = noshell::parser::Tokens::new(__argv);
                    let __args: ParsedArgs<'_, 16usize> = noshell::parser::ParsedArgs::parse(__tokens, __ids);

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

                pub fn ids() -> &'static [(noshell::parser::lexer::Flag<'static>, &'static str)] {
                    static IDS: [(noshell::parser::lexer::Flag<'static>, &'static str); 2usize] = [
                        (noshell::parser::lexer::Flag::Long("value1"), "value1"),
                        (noshell::parser::lexer::Flag::Long("value2"), "value2"),
                    ];
                    &IDS
                }
            }
        };

        assert_eq!(expected.to_string(), given.to_string());

        Ok(())
    }
}

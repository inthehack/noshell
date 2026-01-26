//! Derive macro implementation.

use std::collections::HashSet;

use proc_macro_error::abort_call_site;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::{
    Data, DataStruct, DeriveInput, Expr, ExprLit, Fields, FieldsNamed, Lit, LitStr,
    spanned::Spanned,
};
use syn::{Ident, Type};

use crate::arg::MetaArg;
use crate::attr::{Attr, AttrKind, AttrName, AttrValue};
use crate::ty::{Ty, get_inner_ty};

pub fn run(item: TokenStream) -> TokenStream {
    let input: DeriveInput = match syn::parse2(item) {
        Ok(x) => x,
        Err(err) => abort_call_site!("failed to parse input, {}", err),
    };

    match try_run(&input) {
        Ok(derived) => derived,
        Err(err) => abort_call_site!("failed to derive input, {}", err),
    }
}

// This is the default value.
const PARSED_ARGS_DEFAULT_CAPACITY: usize = 32;

pub fn try_run(input: &DeriveInput) -> syn::Result<TokenStream> {
    let ident = &input.ident;

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let args = collect_args_meta(fields)?;
            let init = build_args_init(&args, format_ident!("args"))?;

            let lookup = build_arg_lookup_table(&args)?;

            let attrs = Attr::parse_all(&input.attrs)?;
            let size =
                get_noshell_attr_limit_arg_value(&attrs)?.unwrap_or(PARSED_ARGS_DEFAULT_CAPACITY);

            Ok(quote! {
                impl #ident {
                    pub fn try_parse_from<'a>(input: &'a [&'a str]) -> Result<Self, noshell::Error>
                    {
                        use noshell::parser::{ArgLookupTable, ParsedArgs};

                        static LOOKUP_TABLE: ArgLookupTable<'_> = ArgLookupTable::new(&#lookup);
                        let args = ParsedArgs::<'_, #size>::try_parse_from(input, &LOOKUP_TABLE)?;

                        Ok(#ident #init)
                    }

                    pub fn parse_from<'a>(iter: &'a [&'a str]) -> Self {
                        Self::try_parse_from(iter).expect("should parse arguments from iterator")
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

fn collect_args_meta(fields: &FieldsNamed) -> syn::Result<Vec<MetaArg>> {
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

fn build_args_init(fields: &[MetaArg], ident: Ident) -> syn::Result<TokenStream> {
    let args = fields
        .iter()
        .map(|x| build_arg_parser(x, ident.clone()))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {{
        #(
            #args
        ),*
    }})
}

fn build_arg_parser(arg: &MetaArg, args_ident: Ident) -> syn::Result<TokenStream> {
    let ty = &arg.ty;
    let inner_ty = get_inner_ty(ty);

    let try_get_one = quote_spanned!(inner_ty.span()=> try_get_one::<#inner_ty>);
    let try_get_many = quote_spanned!(inner_ty.span()=> try_get_many::<_, #inner_ty>);

    let arg_ident = arg.id.unraw();
    let arg_id = arg_ident.to_string();

    let value = match Ty::from_syn_ty(ty) {
        // Optional argument with required value.
        Ty::Option => quote_spanned! { ty.span()=>
            if #args_ident.contains(#arg_id) {
                Some(
                    #args_ident.#try_get_one(#arg_id)
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
            if #args_ident.contains(#arg_id) {
                Some(
                    #args_ident.#try_get_one(#arg_id).map(Option::flatten)?
                )
            } else {
                None
            }
        },

        // Optional argument with required non-empty sequence of values.
        Ty::OptionVec => quote_spanned! { ty.span()=>
            if #args_ident.contains(#arg_id) {
                Some(
                    #args_ident.#try_get_many(#arg_id)
                        .map(Option::unwrap)
                        .and_then(noshell::parser::utils::check_vec_is_missing)?
                )
            } else {
                None
            }
        },

        // Required argument with required non-empty sequence of values.
        Ty::Vec => quote_spanned! { ty.span()=>
            #args_ident.#try_get_many(#arg_id)
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_vec_is_missing)?
        },

        // Required argument with required value.
        Ty::Simple => quote_spanned! { ty.span()=>
            #args_ident.#try_get_one(#arg_id)
                .and_then(noshell::parser::utils::check_arg_is_missing)
                .map(Option::unwrap)
                .and_then(noshell::parser::utils::check_value_is_missing)
                .map(Option::unwrap)?
        },
    };

    Ok(quote_spanned! { arg.span=>
        #arg_ident: #value
    })
}

fn find_attr_with<P>(attrs: &[Attr], mut predicate: P) -> Option<&Attr>
where
    P: FnMut(&Attr) -> bool,
{
    attrs.iter().find(|&x| predicate(x))
}

fn parse_attr_of_literal_string_with<T, P>(attr: &Attr, parser: P) -> syn::Result<Option<T>>
where
    P: FnOnce(&LitStr) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::LitStr(lit)) => lit,
        _ => return Ok(None),
    };

    parser(lit).map(Some)
}

fn parse_attr_of_literal_expr_with<T, P>(attr: &Attr, parser: P) -> syn::Result<Option<T>>
where
    P: FnOnce(&Lit) -> Result<T, syn::Error>,
{
    let lit = match &attr.value {
        Some(AttrValue::Expr(Expr::Lit(ExprLit { lit, .. }))) => lit,
        _ => return Ok(None),
    };

    parser(lit).map(Some)
}

fn parse_noshell_attr_limit_arg(attr: &Attr) -> syn::Result<usize> {
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

fn get_noshell_attr_limit_arg_value(attrs: &[Attr]) -> syn::Result<Option<usize>> {
    if let Some(attr) = find_attr_with(attrs, |x| {
        x.kind == AttrKind::NoShell && x.name == Some(AttrName::Limit)
    }) {
        return Ok(Some(parse_noshell_attr_limit_arg(attr)?));
    }

    Ok(None)
}

fn parse_attr_arg_short_arg(attr: &Attr) -> syn::Result<Option<char>> {
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

fn parse_attr_arg_long_arg(attr: &Attr) -> syn::Result<Option<String>> {
    parse_attr_of_literal_string_with(attr, |lit| Ok(lit.value()))
}

fn build_arg_lookup_table(args: &[MetaArg]) -> syn::Result<TokenStream> {
    let mut items = Vec::new();

    let mut short_keys: HashSet<char> = HashSet::new();
    let mut long_keys: HashSet<String> = HashSet::new();

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
            let atmost = parse_atmost_with_type(&arg.ty);
            items.push(quote! { (#flag, #id, #atmost) });

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
            let atmost = parse_atmost_with_type(&arg.ty);
            items.push(quote! { (#flag, #id, #atmost) });

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
        }

        // If the argument has no defined short or long flag, add a long flag by default. This
        // default long flag has the same value as the field.
        // TODO: make this case as positional argument.
        if shorts.is_empty() && longs.is_empty() {
            if !long_keys.insert(id.clone()) {
                return Err(syn::Error::new(
                    arg.id.span(),
                    format!("long flag `{}` is already used elsewhere", id),
                ));
            }

            let flag = quote!(noshell::parser::lexer::Flag::Long(#id));
            let atmost = parse_atmost_with_type(&arg.ty);
            items.push(quote! { (#flag, #id, #atmost) });
        }
    }

    Ok(quote! { [ #(#items),* ] })
}

fn parse_atmost_with_type(ty: &Type) -> TokenStream {
    match Ty::from_syn_ty(ty) {
        Ty::Simple | Ty::Option | Ty::OptionOption => quote!(noshell::parser::AtMost::One),
        Ty::Vec | Ty::OptionVec => quote!(noshell::parser::AtMost::Many),
    }
}

#[cfg(test)]
mod tests;

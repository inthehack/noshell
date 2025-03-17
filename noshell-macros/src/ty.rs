//! Type helpers.

#![allow(dead_code)]

use syn::{GenericArgument, Path, PathArguments, PathSegment, Type, TypePath};

/// Type variants that are supported by the parsers.
pub(crate) enum Ty {
    Option,
    OptionOption,
    OptionVec,
    Vec,
    Simple,
}

impl Ty {
    pub(crate) fn from_syn_ty(ty: &Type) -> Self {
        if has_ty_param_if_name(ty, "Vec") {
            Ty::Vec
        } else if let Some(param_ty) = get_ty_param_if_name(ty, "Option") {
            if has_ty_param_if_name(param_ty, "Option") {
                Ty::OptionOption
            } else if has_ty_param_if_name(param_ty, "Vec") {
                Ty::OptionVec
            } else {
                Ty::Option
            }
        } else {
            Ty::Simple
        }
    }
}

// These following helpers have been take from the `clap` crate.

pub(crate) fn get_inner_ty(ty: &Type) -> &Type {
    let field_ty = Ty::from_syn_ty(ty);

    match field_ty {
        Ty::Option | Ty::Vec => get_ty_param(ty).unwrap_or(ty),
        Ty::OptionOption | Ty::OptionVec => get_ty_param(ty).and_then(get_ty_param).unwrap_or(ty),
        Ty::Simple => ty,
    }
}

pub(crate) fn is_simple_ty(ty: &Type, name: &str) -> bool {
    only_last_path_segment(ty)
        .map(|segment| {
            if let PathArguments::None = segment.arguments {
                segment.ident == name
            } else {
                false
            }
        })
        .unwrap_or(false)
}

pub(crate) fn has_ty_param_if_name(ty: &Type, name: &str) -> bool {
    get_ty_param_if_name(ty, name).is_some()
}

pub(crate) fn get_ty_param(ty: &Type) -> Option<&Type> {
    get_ty_param_if(ty, |_| true)
}

pub(crate) fn get_ty_param_if_name<'a>(ty: &'a Type, name: &str) -> Option<&'a Type> {
    get_ty_param_if(ty, |x| x.ident == name)
}

pub(crate) fn get_ty_param_if<F>(ty: &Type, f: F) -> Option<&Type>
where
    F: FnOnce(&PathSegment) -> bool,
{
    only_last_path_segment(ty)
        .filter(|segment| f(segment))
        .and_then(|segment| {
            if let PathArguments::AngleBracketed(args) = &segment.arguments {
                // NOTE: Only consider the first type parameter, which is assumed to hold
                // inner type.
                args.args.iter().next().and_then(|arg| {
                    if let GenericArgument::Type(ty) = arg {
                        Some(ty)
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        })
}

pub(crate) fn only_last_path_segment(mut ty: &Type) -> Option<&PathSegment> {
    // What is this for?
    while let Type::Group(syn::TypeGroup { elem, .. }) = ty {
        ty = elem;
    }

    match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => only_one(segments.iter()),

        _ => None,
    }
}

pub(crate) fn only_one<I, T>(mut iter: I) -> Option<T>
where
    I: Iterator<Item = T>,
{
    iter.next().filter(|_| iter.next().is_none())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_match_simple_type() {
        let ty = syn::parse_quote!(i32);
        assert!(is_simple_ty(&ty, "i32"));
    }

    #[test]
    fn it_should_match_option_type() {
        let ty = syn::parse_quote!(Option<i32>);
        assert!(has_ty_param_if_name(&ty, "Option"));

        let ty = syn::parse_quote!(Option<mod1::Type1>);
        assert!(has_ty_param_if_name(&ty, "Option"));

        let ty = syn::parse_quote!(core::ops::Option<i32>);
        assert!(!has_ty_param_if_name(&ty, "Option"));
    }

    #[test]
    fn it_should_match_option_option_type() {
        let ty = syn::parse_quote!(Option<Option<i32>>);
        assert!(has_ty_param_if_name(&ty, "Option"));

        let ty = syn::parse_quote!(Option<Option<mod1::Type1>>);
        assert!(has_ty_param_if_name(&ty, "Option"));
    }

    #[test]
    fn it_should_match_option_vec_type() {
        let ty = syn::parse_quote!(Option<Vec<i32>>);
        assert!(has_ty_param_if_name(&ty, "Option"));
        assert_eq!(
            Some(true),
            get_ty_param_if_name(&ty, "Option")
                .and_then(|x| get_ty_param_if_name(x, "Vec"))
                .and_then(|x| Some(is_simple_ty(x, "i32"))),
        );
    }

    #[test]
    fn it_should_match_vec_type() {
        let ty = syn::parse_quote!(Option<i32>);
        assert!(has_ty_param_if_name(&ty, "Option"));

        let ty = syn::parse_quote!(Option<mod1::Type1>);
        assert!(has_ty_param_if_name(&ty, "Option"));

        let ty = syn::parse_quote!(core::ops::Option<i32>);
        assert!(!has_ty_param_if_name(&ty, "Option"));
    }
}

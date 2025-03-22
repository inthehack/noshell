//! Atrtibute helpers.

// FIXME: use this until the feature is fully implemented.
#![allow(dead_code)]

use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Attribute, Expr, Ident, LitStr, Token};

/// Attribute name.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum AttrKind {
    /// Top-level noshell attribute.
    NoShell,

    /// Argument attribute.
    Arg,
}

/// Attribute name.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum AttrName {
    /// Limit number of values.
    Limit,

    /// Long flag.
    Long,

    /// Short flag.
    Short,
}

/// Attribute value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttrValue {
    /// LitStr is a literal string.
    LitStr(LitStr),

    /// Expr is an expression.
    Expr(Expr),
}

/// This defines an attribute.
#[derive(Clone, Debug)]
pub struct Attr {
    /// The attribute kind.
    pub kind: AttrKind,

    /// The attribute identifier.
    pub id: Ident,

    /// The attribute optional name.
    pub name: Option<AttrName>,

    /// The attribute optional value.
    pub value: Option<AttrValue>,
}

impl Attr {
    /// Parse all attributes given as input.
    pub fn parse_all(attrs: &[Attribute]) -> Result<Vec<Self>, syn::Error> {
        let mut parsed = Vec::new();

        for attr in attrs {
            let kind = if attr.path().is_ident("noshell") {
                AttrKind::NoShell
            } else if attr.path().is_ident("arg") {
                AttrKind::Arg
            } else {
                continue;
            };

            let args = attr.parse_args_with(Punctuated::<Self, Token![,]>::parse_terminated)?;

            // Flatten all the attributes and keep their individual kind for further processing.
            for mut arg in args {
                arg.kind = kind;
                parsed.push(arg);
            }
        }

        Ok(parsed)
    }
}

impl Parse for Attr {
    fn parse(input: ParseStream<'_>) -> Result<Self, syn::Error> {
        let id: Ident = input.parse()?;
        let name = id.to_string();

        let name = match name.as_str() {
            "limit" => Some(AttrName::Limit),
            "long" => Some(AttrName::Long),
            "short" => Some(AttrName::Short),
            _ => None,
        };

        let value = if input.peek(Token![=]) {
            // Skip the assign sign.
            let assign = input.parse::<Token![=]>()?;

            if input.peek(LitStr) {
                Some(AttrValue::LitStr(input.parse::<LitStr>()?))
            } else if let Ok(expr) = input.parse::<Expr>() {
                Some(AttrValue::Expr(expr))
            } else {
                return Err(syn::Error::new(
                    assign.span,
                    "expected a string or an expression after `=`",
                ));
            }
        } else {
            None
        };

        let attr = Attr {
            kind: AttrKind::NoShell,
            id: id.clone(),
            name,
            value,
        };

        Ok(attr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_parse_noshell_empty() {
        let attr: Attribute = syn::parse_quote!(#[noshell()]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(0, attrs.len());
    }

    #[test]
    fn it_should_parse_noshell_with_empty_limit() {
        let attr: Attribute = syn::parse_quote!(#[noshell(limit)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::NoShell, attr.kind);
        assert_eq!(Some(AttrName::Limit), attr.name);
        assert_eq!(None, attr.value);
    }

    #[test]
    fn it_should_parse_noshell_with_non_empty_limit() {
        let attr: Attribute = syn::parse_quote!(#[noshell(limit = 45)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::NoShell, attr.kind);
        assert_eq!(Some(AttrName::Limit), attr.name);
        assert_eq!(Some(AttrValue::Expr(syn::parse_quote!(45))), attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_empty_limit() {
        let attr: Attribute = syn::parse_quote!(#[arg(limit)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Limit), attr.name);
        assert_eq!(None, attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_non_empty_limit() {
        let attr: Attribute = syn::parse_quote!(#[arg(limit = 45)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Limit), attr.name);
        assert_eq!(Some(AttrValue::Expr(syn::parse_quote!(45))), attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_empty_short_flag() {
        let attr: Attribute = syn::parse_quote!(#[arg(short)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Short), attr.name);
        assert_eq!(None, attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_non_empty_short_flag() {
        let attr: Attribute = syn::parse_quote!(#[arg(short = 'v')]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Short), attr.name);
        assert_eq!(Some(AttrValue::Expr(syn::parse_quote!('v'))), attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_empty_long_flag() {
        let attr: Attribute = syn::parse_quote!(#[arg(long)]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Long), attr.name);
        assert_eq!(None, attr.value);
    }

    #[test]
    fn it_should_parse_arg_with_non_empty_long_flag() {
        let attr: Attribute = syn::parse_quote!(#[arg(long = "verbose")]);

        let res = Attr::parse_all(&[attr]);
        assert!(res.is_ok());

        let attrs = res.unwrap();
        assert_eq!(1, attrs.len());

        let attr = attrs.first().unwrap();
        assert_eq!(AttrKind::Arg, attr.kind);
        assert_eq!(Some(AttrName::Long), attr.name);
        assert_eq!(
            Some(AttrValue::LitStr(syn::parse_quote!("verbose"))),
            attr.value
        );
    }
}

use quote::ToTokens;

use crate::tests::utils::format_rust_token_stream;

use super::*;

#[test]
fn it_should_build_id_lookup_table_with_no_attrs() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(0, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_id_lookup_table_with_one_default_long_flag() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        #[arg(long)]
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(1, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_id_lookup_table_with_one_long_flag() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        #[arg(long = "other")]
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(1, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_id_lookup_table_with_one_default_short_flag() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        #[arg(short)]
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(1, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_id_lookup_table_with_one_short_flag() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        #[arg(short = 'd')]
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(1, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_id_lookup_table_with_one_short_and_one_long_flags() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote! {
        #[arg(short, long)]
        value: u32
    };

    let attrs = Attr::parse_all(&field.attrs)?;
    assert_eq!(2, attrs.len());

    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_lookup_table(&[meta])?;

    insta::with_settings!({
        description => format!("input: `{}`", field.into_token_stream().to_string()),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_parser_for_simple_type() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote!(value: u32);

    let attrs = Attr::parse_all(&field.attrs)?;
    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_parser(&meta, format_ident!("__args"))?;

    insta::with_settings!({
        description => field.into_token_stream().to_string(),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_parser_for_option_type() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote!(value: Option<u32>);

    let attrs = Attr::parse_all(&field.attrs)?;
    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_parser(&meta, format_ident!("__args"))?;

    insta::with_settings!({
        description => field.into_token_stream().to_string(),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_parser_for_option_option_type() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote!(value: Option<Option<u32>>);

    let attrs = Attr::parse_all(&field.attrs)?;
    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_parser(&meta, format_ident!("__args"))?;

    insta::with_settings!({
        description => field.into_token_stream().to_string(),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_parser_for_option_vec_type() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote!(value: Option<Vec<u32>>);

    let attrs = Attr::parse_all(&field.attrs)?;
    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_parser(&meta, format_ident!("__args"))?;

    insta::with_settings!({
        description => field.into_token_stream().to_string(),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_parser_for_vec_type() -> anyhow::Result<()> {
    let field: syn::Field = syn::parse_quote!(value: Vec<u32, 8>);

    let attrs = Attr::parse_all(&field.attrs)?;
    let meta = MetaArg::new(&field, attrs);
    let output = build_arg_parser(&meta, format_ident!("__args"))?;

    insta::with_settings!({
        description => field.into_token_stream().to_string(),
        omit_expression => true
    }, {
        insta::assert_snapshot!(output);
    });

    Ok(())
}

#[test]
fn it_should_build_struct_derive() -> anyhow::Result<()> {
    let derive: syn::DeriveInput = syn::parse_quote! {
        struct MyArgs {
            value1: u32,
            value2: u32,
        }
    };

    let output = format_rust_token_stream(try_run(&derive)?);
    insta::assert_snapshot!(output);

    Ok(())
}

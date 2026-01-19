use speculoos::prelude::*;

use crate as noshell;

#[cfg(not(feature = "parser"))]
compile_error!("missing `parser` feature for running tests");

#[test]
fn it_should_parse_args_with_simple_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: u32,
    }

    let argv = ["--value", "233"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_equal_to(233);
}

#[test]
fn it_should_parse_args_with_option_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<u32>,
    }

    let argv = [].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_none();

    let argv = ["--value", "233"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_some().is_equal_to(233);
}

#[test]
fn it_should_parse_args_with_option_option_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<Option<u32>>,
    }

    let argv = [].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_none();

    let argv = ["--value"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_some().is_none();
}

#[test]
fn it_should_parse_args_with_option_vec_type() {
    use heapless::Vec;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<Vec<u32, 8>>,
    }

    // No argument.
    let argv = [].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();

    let args = res.unwrap();
    assert_that!(args.value).is_none();

    // Argument without value.
    let argv = ["--value"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_err();

    // Argument with single value.
    let argv = ["--value", "23"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();
    let args = res.unwrap();

    assert_that!(args.value).is_some();
    let vals = args.value.unwrap();

    assert_that!(vals.len()).is_greater_than(0);
    assert_that!(vals.first()).is_some().is_equal_to(&23);

    // Argument with multiple values.
    let argv = ["--value", "23", "42", "72"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();
    let args = res.unwrap();

    assert_that!(args.value).is_some();
    let vals = args.value.unwrap();

    assert_that!(vals.len()).is_greater_than(0);
    let mut iter = vals.iter();

    assert_that!(iter.next()).is_some().is_equal_to(&23);
    assert_that!(iter.next()).is_some().is_equal_to(&42);
    assert_that!(iter.next()).is_some().is_equal_to(&72);
    assert_that!(iter.next()).is_none();
    assert_that!(iter.next()).is_none();
}

#[test]
#[should_panic]
fn it_should_panic_at_parsing_args_with_option_vec_type() {
    use heapless::Vec;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        #[allow(unused)]
        value: Option<Vec<u32, 4>>,
    }

    // Argument with too much values.
    let argv = ["--value", "1", "2", "3", "4", "5"].into_iter();
    let _ = MyArgs::try_parse_from(argv);
}

#[test]
fn it_should_parse_args_with_vec_type() {
    use heapless::Vec;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Vec<u32, 8>,
    }

    // No argument.
    let argv = [].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_err();

    // Argument without value.
    let argv = ["--value"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_err();

    // Argument with single value.
    let argv = ["--value", "23"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();
    let args = res.unwrap();

    assert_that!(args.value.len()).is_greater_than(0);
    assert_that!(args.value.first()).is_some().is_equal_to(&23);

    // Argument with multiple values.
    let argv = ["--value", "23", "42", "72"].into_iter();
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res).is_ok();
    let args = res.unwrap();

    assert_that!(args.value.len()).is_greater_than(0);
    let mut iter = args.value.iter();

    assert_that!(iter.next()).is_some().is_equal_to(&23);
    assert_that!(iter.next()).is_some().is_equal_to(&42);
    assert_that!(iter.next()).is_some().is_equal_to(&72);
    assert_that!(iter.next()).is_none();
    assert_that!(iter.next()).is_none();
}

#[test]
#[should_panic]
fn it_should_panic_at_parsing_args_with_vec_type() {
    use heapless::Vec;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        #[allow(unused)]
        value: Vec<u32, 4>,
    }

    // Argument with too much values.
    let argv = ["--value", "1", "2", "3", "4", "5"].into_iter();
    let _ = MyArgs::try_parse_from(argv);
}

use speculoos::prelude::*;

#[test]
fn it_should_parse_args_with_simple_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: u32,
    }

    let argv = &["--value", "42"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .is_equal_to(42);
}

#[test]
fn it_should_parse_args_with_option_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<u32>,
    }

    let argv = &[];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_ok().map(|x| &x.value).is_none();

    let argv = &["--value", "42"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .is_some()
        .is_equal_to(42);
}

#[test]
fn it_should_parse_args_with_option_option_type() {
    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<Option<u32>>,
    }

    let argv = &[];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_ok().map(|x| &x.value).is_none();

    let argv = &["--value"];
    let res = MyArgs::try_parse_from(argv);

    assert_that!(res)
        .is_ok()
        .map(|x| &x.value)
        .is_some()
        .is_none();
}

#[test]
fn it_should_parse_args_with_option_vec_type() {
    use heapless::Vec;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        value: Option<Vec<u32, 8>>,
    }

    // No argument.
    let argv = &[];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_ok().map(|x| &x.value).is_none();

    // Argument without value.
    let argv = &["--value"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_err();

    // Argument with single value.
    let argv = &["--value", "42"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .is_some()
        .equals_iterator(&[42].iter());

    // Argument with multiple values.
    let argv = &["--value", "23", "42", "72"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .is_some()
        .equals_iterator(&[23, 42, 72].iter());
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
    let argv = &["--value", "1", "2", "3", "4", "5"];
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
    let argv = &[];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_err();

    // Argument without value.
    let argv = &["--value"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output).is_err();

    // Argument with single value.
    let argv = &["--value", "42"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .equals_iterator(&[42].iter());

    // Argument with multiple values.
    let argv = &["--value", "23", "42", "72"];
    let output = MyArgs::try_parse_from(argv);

    assert_that!(output)
        .is_ok()
        .map(|x| &x.value)
        .equals_iterator(&[23, 42, 72].iter());
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
    let argv = &["--value", "1", "2", "3", "4", "5"];
    let _ = MyArgs::try_parse_from(argv);
}

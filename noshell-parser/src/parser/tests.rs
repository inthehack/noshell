use speculoos::prelude::*;

use super::*;

const PARSED_ARG_CAPACITY: usize = 32;
const PARSED_VALUES_CAPACITY: usize = 8;

#[test]
fn it_should_parse_arg_values_with_missing_value() {
    let argv = &["-f"];
    let (rest, arg) =
        ParsedArgs::<'_, PARSED_ARG_CAPACITY>::parse_arg_values(&[], "field", AtMost::One);

    assert_that!(rest).is_equal_to(Values::empty());
    assert_that!(arg).is_equal_to(Arg::Named("field", Values::new(&argv[1..])));
    assert_that!(arg)
        .matches(|x| matches!(x, Arg::Named("field", values) if values.iter().next().is_none()));
}

#[test]
fn it_should_parse_arg_values_with_missing_many_values() {
    let argv = &["-f"];
    let (rest, arg) =
        ParsedArgs::<'_, PARSED_ARG_CAPACITY>::parse_arg_values(&[], "field", AtMost::Many);

    assert_that!(rest).is_equal_to(Values::empty());
    assert_that!(arg).is_equal_to(Arg::Named("field", Values::new(&argv[1..])));
    assert_that!(arg)
        .matches(|x| matches!(x, Arg::Named("field", values) if values.iter().next().is_none()));
}

#[test]
fn it_should_parse_arg_values_with_value() {
    let argv = &["-f", "42"];
    let (rest, arg) =
        ParsedArgs::<'_, PARSED_ARG_CAPACITY>::parse_arg_values(&argv[1..], "field", AtMost::One);

    assert_that!(rest).is_equal_to(Values::new(&argv[2..]));
    assert_that!(arg).is_equal_to(Arg::Named("field", Values::new(&argv[1..])));
    assert_that!(arg)
        .matches(|x| matches!(x, Arg::Named("field", values) if values.iter().next().is_some()));
}

#[test]
fn it_should_parse_arg_values_with_single_many_values() {
    let argv = &["-f", "42"];
    let (rest, arg) =
        ParsedArgs::<'_, PARSED_ARG_CAPACITY>::parse_arg_values(&[], "field", AtMost::Many);

    assert_that!(rest).is_equal_to(Values::empty());
    assert_that!(arg).is_equal_to(Arg::Named("field", Values::new(&argv[2..])));
    assert_that!(arg)
        .matches(|x| matches!(x, Arg::Named("field", values) if values.iter().next().is_none()));
}

#[test]
fn it_should_parse_arg_values_with_many_value() {
    let argv = &["-f", "42", "24"];
    let (rest, arg) =
        ParsedArgs::<'_, PARSED_ARG_CAPACITY>::parse_arg_values(&argv[1..], "field", AtMost::Many);

    assert_that!(rest).is_equal_to(Values::new(&argv[3..]));
    assert_that!(arg).is_equal_to(Arg::Named("field", Values::new(&argv[1..])));
    assert_that!(arg)
        .matches(|x| matches!(x, Arg::Named("field", values) if values.iter().count() > 1));
}

#[test]
fn it_should_parse_missing_arg_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::One)]);

    let argv = &["-f"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_one::<u32>("field"))
        .is_ok()
        .is_some()
        .is_none();
}

#[test]
fn it_should_parse_missing_arg_many_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::Many)]);

    let argv = &["-f"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_many::<Vec<_, PARSED_VALUES_CAPACITY>, u32>("field"))
        .is_ok()
        .is_some()
        .matches(|x| x.is_empty());
}

#[test]
fn it_should_parse_invalid_arg_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::One)]);

    let argv = &["-f", "-42"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_one::<u32>("field")).is_err_containing(Error::InvalidArgument);
}

#[test]
fn it_should_parse_invalid_arg_many_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::Many)]);

    let argv = &["-f", "42", "-42"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_many::<Vec<_, PARSED_VALUES_CAPACITY>, u32>("field"))
        .is_err_containing(Error::InvalidArgument);
}

#[test]
fn it_should_parse_valid_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::One)]);

    let argv = &["-f", "42"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_one::<u32>("field"))
        .is_ok()
        .is_some()
        .is_some()
        .is_equal_to(42);
}

#[test]
fn it_should_parse_valid_many_value() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Short('f'), "field", AtMost::Many)]);

    let argv = &["-f", "42", "42"];
    let args: ParsedArgs<'_, PARSED_ARG_CAPACITY> = ParsedArgs::parse_from(argv, &LOOKUP);

    assert_that!(args.try_get_many::<Vec<_, PARSED_VALUES_CAPACITY>, u32>("field"))
        .is_ok()
        .is_some()
        .matches(|x| x.iter().sum::<u32>() == 84);
}

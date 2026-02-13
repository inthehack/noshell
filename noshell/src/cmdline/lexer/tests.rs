
use rstest::rstest;
use speculoos::prelude::*;

use super::*;

#[rstest]
#[case("", "")]
#[case("word", "word")]
fn it_should_parse_word(#[case] input: &str, #[case] expected: &str) {
    assert_that!(parse_single_word(input))
        .is_ok()
        .matches(|(_, word)| expected == *word);
}

#[rstest]
#[case("''")]
#[case("'word'")]
#[case("\"\"")]
#[case("\"word\"")]
fn it_should_parse_quoted_word(#[case] input: &str) {
    fn unquote(s: &str) -> &str {
        s.trim_matches('\'').trim_matches('"')
    }

    assert_that!(parse_single_word(input))
        .is_ok()
        .matches(|(_, word)| unquote(input) == *word);
}

#[rstest]
#[case("'word")]
#[case("'word\"")]
#[case("\"word")]
#[case("\"word'")]
fn it_should_fail_parse_invalid_quoted_word(#[case] input: &str) {
    assert_that!(parse_single_word(input)).is_err();
}

#[rstest]
#[case("\tword", &["word"])]
#[case("\rword", &["word"])]
#[case("\nword", &["word"])]
#[case("\r\nword", &["word"])]
#[case("\tword\t", &["word", "word"])]
#[case("\rword\rword", &["word", "word"])]
#[case("\nword\nword", &["word", "word"])]
#[case("\r\nword\r\nword", &["word", "word"])]
#[case(
        "-f value1 --flag2 value2",
        &["-f", "value1", "--flag2", "value2"]
    )]
#[case(
        "-f value1 --flag2 \"value2.1 value2.2\"",
        &["-f", "value1", "--flag2", "value2.1 value2.2"]
    )]
fn it_should_parse_multiple_words(#[case] input: &str, #[case] expected: &[&str]) {
    let words: Result<Vec<_>, _> = split(input).try_collect();

    assert_that!(words).is_ok().matches(|x| {
        x.iter().enumerate().fold(true, |state, (i, item)| {
            state && {
                let Some(expected_value) = expected.get(i) else {
                    return false;
                };

                expected_value == item
            }
        })
    });
}

#[rstest]
#[case("word 'word word")]
#[case("word \"word word")]
fn it_should_fail_parse_invalid_multiple_words(#[case] input: &str) {
    let words: Result<Vec<_>, _> = split(input).try_collect();

    assert_that!(words)
        .is_err()
        .matches(|x| matches!(x, Error::InvalidInput(_)));
}

//! Lexer.
//!
//! This lexer is in charge of lexing the command line in a POSIX-compliant way.

use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::{take_until, take_while};
use nom::character::complete::char;
use nom::combinator::cut;
use nom::sequence::{preceded, terminated};
use nom::{IResult, Parser};

/// Errors from the lexer.
#[derive(Debug, PartialEq, Eq, Hash, thiserror::Error)]
pub enum Error<'a> {
    /// Invalid input, no word can be found.
    #[error("invalid input")]
    InvalidInput(&'a str),

    /// Unknown error, for development only.
    #[error("unknown error")]
    Unknown,
}

/// Re-export of result type.
pub type Result<'a, T, E = Error<'a>> = core::result::Result<T, E>;

/// Lex the command line and split it into words in a POSIX-compliant way.
pub fn split<'a>(input: &'a str) -> Words<'a> {
    Words::new(input)
}

/// Successive words in the sense of POSIX input on a command line.
#[derive(Clone, Debug)]
pub struct Words<'a> {
    input: &'a str,
}

impl<'a> Words<'a> {
    /// Create a new iterator from the input string.
    fn new(input: &'a str) -> Self {
        Words { input }
    }
}

impl<'a> Words<'a> {
    /// Get the next word and consume the iterator.
    pub fn advance(&mut self) -> Result<'a, Option<&'a str>> {
        // Remove useless trailing whitespaces.
        self.input = trim_start_whitespaces(self.input);

        if self.input.is_empty() {
            return Ok(None);
        }

        // Parse the next word.
        match parse_single_word(self.input) {
            Ok((rest, word)) => {
                self.input = rest;
                Ok(Some(word))
            }

            Err(nom::Err::Error(_)) => Ok(None),
            Err(nom::Err::Incomplete(_)) => Err(Error::InvalidInput("unreachable")),
            Err(nom::Err::Failure(err)) => Err(Error::InvalidInput(err.input)),
        }
    }

    /// Consume the iterator into a collection.
    pub fn try_collect<OutputTy: FromIterator<&'a str>>(mut self) -> Result<'a, OutputTy> {
        let words = core::iter::from_fn(move || self.advance().transpose());
        words.process_results(|iter| OutputTy::from_iter(iter))
    }
}

fn parse_single_word(input: &str) -> IResult<&str, &str> {
    alt((
        parse_in_between_single_quotes,
        parse_in_between_double_quotes,
        take_while(|x: char| !x.is_ascii_whitespace()),
    ))
    .parse_complete(input)
}

fn trim_start_whitespaces(input: &str) -> &str {
    input.trim_start_matches(|x: char| x.is_ascii_whitespace())
}

fn parse_in_between_single_quotes(input: &str) -> IResult<&str, &str> {
    preceded(char('\''), cut(terminated(take_until("'"), char('\'')))).parse_complete(input)
}

fn parse_in_between_double_quotes(input: &str) -> IResult<&str, &str> {
    preceded(char('"'), cut(terminated(take_until("\""), char('"')))).parse_complete(input)
}

#[cfg(test)]
mod tests {
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
}

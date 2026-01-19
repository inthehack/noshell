//! Lexer.
//!
//! This lexer is in charge of lexing the command line in a POSIX-compliant way.

use nom::branch::alt;
use nom::bytes::complete::{take_until, take_while};
use nom::character::complete::char;
use nom::sequence::delimited;
use nom::{IResult, Parser};

use crate::cmdline::{Error, Result};

/// Lex the command line and split it into words in a POSIX-compliant way.
pub fn split<'a>(input: &'a str) -> impl Iterator<Item = Result<&'a str>> + 'a {
    WordIterator::new(input)
}

struct WordIterator<'a> {
    input: &'a str,
}

impl<'a> WordIterator<'a> {
    /// Create a new iterator from the input string.
    fn new(input: &'a str) -> Self {
        WordIterator { input }
    }
}

impl<'a> Iterator for WordIterator<'a> {
    type Item = Result<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        // Remove useless trailing whitespaces.
        self.input = trim_start_whitespaces(self.input);

        // Check if the input is empty.
        self.input.chars().next()?;

        // Parse the next word.
        match parse_single_word(self.input) {
            Ok((rest, word)) => {
                self.input = rest;
                Some(Ok(word))
            }

            Err(nom::Err::Error(_)) => None,
            Err(nom::Err::Incomplete(_)) => None,
            Err(nom::Err::Failure(_)) => Some(Err(Error::Unknown)),
        }
    }
}

fn is_whitespace(input: char) -> bool {
    [' ', '\t', '\n'].contains(&input)
}

fn parse_single_word(input: &str) -> IResult<&str, &str> {
    alt((
        parse_in_between_single_quotes,
        parse_in_between_double_quotes,
        take_while(|x| !is_whitespace(x)),
    ))
    .parse_complete(input)
}

#[inline(always)]
fn trim_start_whitespaces(input: &str) -> &str {
    input.trim_start_matches(is_whitespace)
}

fn parse_in_between_single_quotes(input: &str) -> IResult<&str, &str> {
    delimited(char('\''), take_until("'"), char('\'')).parse_complete(input)
}

fn parse_in_between_double_quotes(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), take_until("\""), char('"')).parse_complete(input)
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use speculoos::prelude::*;

    use super::*;

    #[rstest]
    #[case("")]
    #[case("word")]
    #[case("-f")]
    #[case("--flag")]
    fn it_should_parse_single_word(#[case] input: &str) {
        assert_that!(parse_single_word(input))
            .is_ok()
            .matches(|(_, word)| input == *word);
    }

    #[rstest]
    #[case("''")]
    #[case("'word'")]
    #[case("\"\"")]
    #[case("\"word\"")]
    fn it_should_parse_single_quoted_word(#[case] input: &str) {
        fn unquote(s: &str) -> &str {
            s.trim_matches('\'').trim_matches('"')
        }

        assert_that!(parse_single_word(input))
            .is_ok()
            .matches(|(_, word)| unquote(input) == *word);
    }

    #[rstest]
    #[case(
        "-f value1 --flag2 value2",
        &["-f", "value1", "--flag2", "value2"]
    )]
    #[case(
        "-f value1 --flag2 \"value2.1 value2.2\"",
        &["-f", "value1", "--flag2", "value2.1 value2.2"]
    )]
    fn it_should_parse_multiple_words(#[case] input: &str, #[case] expected: &[&str]) {
        let words: Result<Vec<_>, _> = split(input).collect();

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
}

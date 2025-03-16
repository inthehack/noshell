//! A parser for collecting arguments from a token stream.

use core::fmt::Debug;
use core::str::FromStr;

use heapless::Vec;

use crate::lexer::{Flag, IntoTokens, Token, Values};

/// Defines the possible errors that may occur during parsing of arguments.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[non_exhaustive]
pub enum Error {
    /// The argument value is invalid, meaning that it cannot be converted to the destination
    /// type. This could mean that there is a missing implementation for [`str::parse`] trait.
    #[error("invalid argument")]
    InvalidArgument,

    /// The argument value is missing, which occurs when the flag is not boolean and expect a
    /// value.
    #[error("missing argument")]
    MissingArgument,

    /// Insufficient space for parsing arguments.
    #[error("out of parser memory space")]
    OutOfMemory,
}

/// Defines the result of argument parsing. This is a simple key-value store that offers a look-up
/// over parsed arguments.
#[derive(Debug, Default)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct ParsedArgs<'a, const ARG_COUNT_MAX: usize = 8> {
    args: Vec<(Flag<'a>, Values<'a>), ARG_COUNT_MAX>,
}

impl<'a, const SIZE: usize> ParsedArgs<'a, SIZE> {
    /// Parse the command line input from a token stream. The result is the set of found arguments.
    pub fn parse(argv: impl IntoTokens<'a>) -> Self {
        Self::try_parse(argv).expect("cannot parse arguments")
    }

    /// Try to parse the input arguments.
    pub fn try_parse(argv: impl IntoTokens<'a>) -> Result<Self, Error> {
        let mut tokens = argv.into_tokens();

        let mut out = Self::default();

        while let Some(token) = tokens.next() {
            if let Token::Flag(x) = &token {
                if out.args.push((*x, tokens.values())).is_err() {
                    return Err(Error::OutOfMemory);
                }
            }
        }

        Ok(out)
    }

    /// Check if there exists an argument with the given key (i.e. short or long flag).
    #[inline(always)]
    pub fn contains(&self, id: &str) -> bool {
        self.args.iter().any(|x| x.0 == id)
    }

    /// Get one value for the given flag identifier.
    pub fn get_one<T>(&self, id: &str) -> Option<T>
    where
        T: FromStr,
    {
        self.try_get_one::<T>(id).expect("invalid argument")
    }

    /// Get many values for the given flag identifier.
    pub fn get_many<B, T>(&self, id: &str) -> Option<B>
    where
        B: FromIterator<T>,
        T: FromStr,
    {
        self.try_get_many::<B, T>(id).expect("invalid argument")
    }

    /// Try to get and parse the argument value if any.
    pub fn try_get_one<T>(&self, id: &str) -> Result<Option<T>, Error>
    where
        T: FromStr,
    {
        if let Some((_, values)) = self.args.iter().find(|x| x.0 == id) {
            let mut iter = values.clone();

            let value = if let Some(value) = iter.next() {
                value
            } else {
                // The argument has no value.
                return Err(Error::InvalidArgument);
            };

            if iter.next().is_some() {
                // The argument has more than one value.
                return Err(Error::InvalidArgument);
            }

            // The value cannot be parsed to the target type `T`.
            return value
                .parse::<T>()
                .map(Some)
                .map_err(|_| Error::InvalidArgument);
        }

        Ok(None)
    }

    /// Try to get and parse the argument value if any. The value can be constructed from
    /// an iterator.
    pub fn try_get_many<B, T>(&self, id: &str) -> Result<Option<B>, Error>
    where
        B: FromIterator<T>,
        T: FromStr,
    {
        if let Some((_, values)) = self.args.iter().find(|x| x.0 == id) {
            let iter = values.clone();

            // Collect on Seq<Result<T, _>> can be coerced to Result<Seq<T>, _>.
            let result: Result<B, _> = iter.map(|x| x.parse::<T>()).collect();

            // The value cannot be parsed to the target type `T`.
            return result.map(Some).map_err(|_| Error::InvalidArgument);
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use crate::lexer::Tokens;

    use super::*;

    #[test]
    fn it_should_parse_valid_value() {
        let tokens = Tokens::new(&["-v", "42"]);
        let args: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);
        assert_that!(
            args.try_get_one::<u32>("v"),
            matches_pattern!(&Ok(Some(42)))
        );
    }

    #[test]
    fn it_should_parse_invalid_arg() {
        let tokens = Tokens::new(&["-v", "-42"]);
        let args: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);
        assert_that!(
            args.try_get_one::<u32>("v"),
            eq(&Err(Error::InvalidArgument))
        );
    }

    #[test]
    fn it_should_parse_missing_arg() {
        let tokens = Tokens::new(&["-v"]);
        let args: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);
        assert_that!(
            args.try_get_one::<u32>("v"),
            eq(&Err(Error::InvalidArgument))
        );
    }

    #[test]
    fn it_should_parse_enabled_bool_arg() {
        let tokens = Tokens::new(&["-v"]);
        let args: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);
        assert_that!(args.contains("v"), eq(true));
        assert_that!(
            args.try_get_one::<bool>("v"),
            eq(&Err(Error::InvalidArgument))
        );
    }

    #[test]
    fn it_should_parse_enabled_bool_arg_with_value() {
        let tokens = Tokens::new(&["-v", "true"]);
        let args: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);
        assert_that!(args.contains("v"), eq(true));
        assert_that!(args.try_get_one::<bool>("v"), eq(&Ok(Some(true))));
    }
}

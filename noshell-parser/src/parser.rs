//! A parser for collecting arguments from a token stream.

use core::str::FromStr;

use heapless::Vec;

use crate::lexer;

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
}

/// Defines the result of argument parsing. This is a simple key-value store that offers a look-up
/// over parsed arguments.
#[derive(Debug, Default)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct ParsedArgs<'a, const ARG_COUNT_MAX: usize = 8> {
    args: Vec<lexer::Token<'a>, ARG_COUNT_MAX>,
}

impl<'a> ParsedArgs<'a> {
    /// Parse the command line input from a token stream. The result is the set of found arguments.
    pub fn parse<I>(tokens: I) -> Self
    where
        I: Iterator<Item = lexer::Token<'a>>,
    {
        let mut out = Self::default();

        for token in tokens {
            out.args.push(token).ok();
        }

        out
    }

    /// Try to get and parse the argument value if any.
    pub fn get<'k, T>(&self, key: &'k str) -> Result<Option<T>, Error>
    where
        'a: 'k,
        T: FromStr,
    {
        let mut wait_for_value = false;

        for token in &self.args {
            if !wait_for_value && match_key_with_token(key, token) {
                wait_for_value = true;
                continue;
            }

            if wait_for_value {
                if let lexer::Token::Value(value) = *token {
                    let typed = value.parse::<T>().map_err(|_| Error::InvalidArgument)?;
                    return Ok(Some(typed));
                }

                return Err(Error::MissingArgument);
            }
        }

        // Not enough tokens.
        if wait_for_value {
            return Err(Error::MissingArgument);
        }

        Ok(None)
    }

    /// Check if there exists an argument with the given key (i.e. short or long flag).
    pub fn is_enabled<'k>(&self, key: &'k str) -> bool
    where
        'a: 'k,
    {
        for token in &self.args {
            if match_key_with_token(key, token) {
                return true;
            }
        }

        false
    }
}

fn match_key_with_token<'a, 'k>(key: &'k str, token: &lexer::Token<'a>) -> bool
where
    'a: 'k,
{
    match token {
        lexer::Token::ShortFlag(name) => key.len() == 1 && *name == key.chars().next().unwrap(),
        lexer::Token::LongFlag(name) => key.len() > 1 && *name == key,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use crate::Lexer;

    use super::*;

    #[test]
    fn it_should_parse_valid_value() {
        let lexer = Lexer::new(&["-v", "42"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.get::<u32>("v"), matches_pattern!(&Ok(Some(42))));
    }

    #[test]
    fn it_should_parse_invalid_arg() {
        let lexer = Lexer::new(&["-v", "-42"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.get::<u32>("v"), eq(&Err(Error::InvalidArgument)));
    }

    #[test]
    fn it_should_parse_missing_arg() {
        let lexer = Lexer::new(&["-v"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.get::<u32>("v"), eq(&Err(Error::MissingArgument)));
    }

    #[test]
    fn it_should_parse_enabled_bool_arg() {
        let lexer = Lexer::new(&["-v"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.is_enabled("v"), eq(true));
        assert_that!(args.get::<bool>("v"), eq(&Err(Error::MissingArgument)));
    }

    #[test]
    fn it_should_parse_enabled_bool_arg_with_value() {
        let lexer = Lexer::new(&["-v", "true"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.is_enabled("v"), eq(true));
        assert_that!(args.get::<bool>("v"), eq(&Ok(Some(true))));
    }
}

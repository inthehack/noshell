use core::str::FromStr;

use heapless::Vec;

use crate::lexer;

#[derive(Debug, Default, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Error {
    #[default]
    #[error("undefined parsing error")]
    Undefined,

    #[error(transparent)]
    Lexer(#[from] lexer::Error),
}

pub const ARG_MATCHES_SIZE_MAX: usize = 16;

#[derive(Debug, Default)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct ParsedArgs<'a> {
    pub args: Vec<lexer::Token<'a>, ARG_MATCHES_SIZE_MAX>,
}

impl<'a> ParsedArgs<'a> {
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

    pub fn get<'k, T>(&self, key: &'k str) -> Result<Option<T>, Error>
    where
        'a: 'k,
        T: FromStr,
    {
        let mut pending = false;

        for token in &self.args {
            if match_key_with_token(key, token) {
                pending = true;
                continue;
            }

            if let lexer::Token::Value(value) = token {
                // Parse error.
                return Ok(Some(value.parse::<T>().map_err(|_| Error::Undefined)?));
            }
        }

        // Missing value.
        if pending {
            return Err(Error::Undefined);
        }

        // Not found.
        Ok(None)
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
    fn it_should_parse_expected_value() {
        let lexer = Lexer::new(&["-v", "42"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.get::<u32>("v"), matches_pattern!(&Ok(Some(42))));
    }

    #[test]
    fn it_should_parse_unexpected_value() {
        let lexer = Lexer::new(&["-v", "-42"]);
        let args = ParsedArgs::parse(lexer);
        assert_that!(args.get::<u32>("v"), eq(&Err(Error::Undefined)));
    }
}

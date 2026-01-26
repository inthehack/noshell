//! A lexer for generating tokens from a command line.

use core::marker::PhantomData;
use core::ops::Deref;

/// Variant of `Flag` token. Only store the identifier, not the hyphens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Flag<'a> {
    /// A short flag (e.g. -v).
    Short(char),

    /// A long flag (e.g. --verbose).
    Long(&'a str),
}

/// Defines a `Token` that has been read from the command line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Token<'a> {
    /// Flag.
    Flag(Flag<'a>),

    /// Value (i.e. everything that is not a short or long flag).
    Value(&'a str),
}

impl<'a> Token<'a> {
    /// Evaluate if the token string is a short flag.
    #[inline(always)]
    pub fn is_short_flag(input: &str) -> bool {
        input.starts_with('-') && input.len() == 2 && !Self::is_number(input)
    }

    /// Evaluate if the token string is a short flag, and then return it.
    pub fn as_short_flag(input: &str) -> Option<Self> {
        if Self::is_short_flag(input) {
            let (_, name) = input.split_at(1);
            let first = name.chars().nth(0).unwrap_or_default();
            Some(Token::Flag(Flag::Short(first)))
        } else {
            None
        }
    }

    /// Evaluate if the token string is a long flag.
    #[inline(always)]
    pub fn is_long_flag(input: &str) -> bool {
        input.starts_with("--") && input.len() > 2
    }

    /// Evaluate if the token string is a long flag, and then return it.
    pub fn as_long_flag(input: &'a str) -> Option<Self> {
        if Self::is_long_flag(input) {
            let (_, name) = input.split_at(2);
            Some(Token::Flag(Flag::Long(name)))
        } else {
            None
        }
    }

    /// Evaluate if the token string is a flag.
    #[inline(always)]
    pub fn is_flag(input: &str) -> bool {
        Self::is_short_flag(input) || Self::is_long_flag(input)
    }

    /// Evaluate if the token string represents a number.
    pub fn is_number(input: &str) -> bool {
        let mut position_of_e = None;
        let mut have_seen_dot = false;

        // Remove the front sign is any.
        let input = input.trim_start_matches('-');

        for (i, c) in input.as_bytes().iter().enumerate() {
            match c {
                // Digits, OK.
                b'0'..=b'9' => {}

                // Exponential, OK if not the first character.
                b'e' | b'E' if position_of_e.is_none() && i > 0 => {
                    position_of_e = Some(i);
                }

                // Dot is valid if unique, not the first character and before any exponential.
                b'.' if !have_seen_dot && position_of_e.is_none() && i > 0 => {
                    have_seen_dot = true;
                }

                _ => return false,
            }
        }

        if let Some(pos) = position_of_e {
            pos != input.len() - 1
        } else {
            true
        }
    }

    /// Convert a input string into a token.
    pub fn tokenize(input: &'a str) -> Self {
        if let Some(flag) = Self::as_short_flag(input) {
            return flag;
        }

        if let Some(flag) = Self::as_long_flag(input) {
            return flag;
        }

        Token::Value(input)
    }
}

/// Defines a `Lexer` that is responsible for streaming tokens from the command line input.
///
/// A lexer acts like an forward iterator.
#[derive(Debug)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct TokenIterator<'a, InnerTy> {
    inner: InnerTy,
    _marker: PhantomData<&'a ()>,
}

impl<'a, InnerTy> TokenIterator<'a, InnerTy> {
    /// Create a new lexer from the command line input.
    pub fn new(inner: InnerTy) -> Self {
        TokenIterator {
            inner,
            _marker: PhantomData,
        }
    }
}

impl<'a, InnerTy> Iterator for TokenIterator<'a, InnerTy>
where
    InnerTy: Iterator,
    <InnerTy as Iterator>::Item: Deref<Target = &'a str>,
{
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.inner.next()?;
        Some(Token::tokenize(item.deref()))
    }
}

impl<'a> From<&'a [&'a str]> for TokenIterator<'a, core::slice::Iter<'a, &'a str>> {
    fn from(value: &'a [&'a str]) -> Self {
        TokenIterator::new(value.iter())
    }
}

#[cfg(test)]
mod tests {
    use speculoos::prelude::*;

    use super::*;

    #[test]
    fn it_should_match_short_flag() {
        let mut lexer = TokenIterator::new(["-f"].iter());

        assert_that!(lexer.next())
            .is_some()
            .is_equal_to(Token::Flag(Flag::Short('f')));
    }

    #[test]
    fn it_should_match_value_starting_with_dash() {
        let mut lexer = TokenIterator::new(["-flag"].iter());

        assert_that!(lexer.next())
            .is_some()
            .is_equal_to(Token::Value("-flag"));
    }

    #[test]
    fn it_should_match_long_flag() {
        let mut lexer = TokenIterator::new(["--flag"].iter());

        assert_that!(lexer.next())
            .is_some()
            .is_equal_to(Token::Flag(Flag::Long("flag")));
    }

    #[test]
    fn it_should_match_numbers() {
        let lexer =
            TokenIterator::new(["-2", "2", "-2.", "2.", "-2.e1", "2.e1", "-2e1", "2e1"].iter());

        for token in lexer {
            assert_that!(token).matches(|x| matches!(x, &Token::Value(_)));
        }
    }
}

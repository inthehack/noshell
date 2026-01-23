//! A parser for collecting arguments from a token stream.

use core::fmt::Debug;
use core::str::FromStr;

use heapless::Vec;

use crate::lexer::{Flag, Token};

mod values;

pub use values::{AtMost, Values};

#[cfg(test)]
mod tests;

/// Defines the possible errors that may occur during parsing of arguments.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[non_exhaustive]
pub enum Error {
    /// The argument is not defined.
    #[error("undefined argument")]
    UndefinedArgument,

    /// The argument value is invalid, meaning that it cannot be converted to the destination
    /// type. This could mean that there is a missing implementation for [`str::parse`] trait.
    #[error("invalid argument")]
    InvalidArgument,

    /// The argument has no expected value on the command line.
    #[error("no value expected")]
    NoValueArgument,

    /// The argument value is missing, which occurs when the flag is not boolean and expect a
    /// value.
    #[error("missing argument")]
    MissingArgument,

    /// Insufficient space for parsing arguments.
    #[error("out of parser memory space")]
    OutOfMemory,
}

/// Re-export of result type with module [`Error`].
pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Defines an argument on the command line.
#[derive(Clone, Debug, PartialEq)]
pub enum Arg<'a> {
    /// A named argument, which is defined by a flag, and zero or more values.
    Named(&'a str, Values<'a>),

    /// A positional argument, which is defined by its value.
    Positional(&'a str),
}

/// Argument id to metadata look-up table.
#[derive(Debug)]
pub struct ArgLookupTable<'a> {
    table: &'a [(Flag<'a>, &'a str, AtMost)],
}

impl<'a> ArgLookupTable<'a> {
    /// Create a new look-up table.
    pub const fn new(table: &'a [(Flag<'a>, &'a str, AtMost)]) -> Self {
        ArgLookupTable { table }
    }

    /// Look up for a flag.
    pub fn metadata_of(&self, flag: &Flag<'_>) -> Option<(&'a str, AtMost)> {
        let (_, id, expected) = self.table.iter().find(|&x| x.0 == *flag)?;
        Some((*id, *expected))
    }
}

/// Defines the result of argument parsing. This is a simple key-value store that offers a look-up
/// over parsed arguments.
#[derive(Default, Debug)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct ParsedArgs<'a, const CAPACITY: usize = 1> {
    args: Vec<Arg<'a>, CAPACITY>,
}

impl<'a, const CAPACITY: usize> ParsedArgs<'a, CAPACITY> {
    /// Parse the command line input from a token stream. The result is the set of found arguments.
    pub fn parse_from(argv: &'a [&'a str], ids: &ArgLookupTable<'static>) -> Self {
        Self::try_parse_from(argv, ids).expect("cannot parse arguments")
    }

    /// Try to parse the input arguments.
    pub fn try_parse_from(
        argv: &'a [&'a str],
        table: &ArgLookupTable<'static>,
    ) -> Result<Self, Error> {
        // Some initial checks before start parsing.
        Self::check_capacity(argv)?;
        Self::check_undefined_argument(argv, table)?;

        let mut parsed = ParsedArgs::default();

        let lookup = |flag: &Flag<'a>| {
            // SAFETY: the validation above guarantees that the lookup found an entry.
            unsafe { table.metadata_of(flag).unwrap_unchecked() }
        };

        let named = |args: &mut Vec<_, _>, name, expected, (start, end)| {
            let (rest, arg) = Self::parse_arg_values(&argv[start..end], name, expected);

            // SAFETY: the validation above guarantees that the capacity of the resulting
            // parsed args is sufficient.
            unsafe { args.push(arg).unwrap_unchecked() };

            for value in rest.iter() {
                // SAFETY: the validation above guarantees that the capacity of the resulting
                // parsed args is sufficient.
                unsafe { args.push(Arg::Positional(value)).unwrap_unchecked() };
            }
        };

        let positional = |args: &mut Vec<_, _>, value| {
            // SAFETY: the validation above guarantees that the capacity of the resulting
            // parsed args is sufficient.
            unsafe { args.push(Arg::Positional(value)).unwrap_unchecked() }
        };

        let parse_then_push =
            |state, (index, arg): (usize, &&'a str)| match (state, Token::tokenize(arg)) {
                // A flag has been met, while this new flag occurs, then save the previous one and
                // keep going on the new flag values.
                (Some((flag, start)), Token::Flag(next)) => {
                    let (name, expected) = lookup(&flag);
                    named(&mut parsed.args, name, expected, (start, index));
                    Some((next, index + 1))
                }

                // A flag has been met and this value belong to it, then keep going.
                (Some(_), Token::Value(_)) => state,

                // No flag has been met and a new one occurs, then keep going on the new flag
                // values.
                (None, Token::Flag(flag)) => Some((flag, index + 1)),

                // No flag has been met, then this value is a positional argument.
                (None, Token::Value(value)) => {
                    positional(&mut parsed.args, value);
                    None
                }
            };

        let last_flag = argv.iter().enumerate().fold(None, parse_then_push);

        if let Some((flag, start)) = last_flag {
            let (name, expected) = lookup(&flag);
            named(&mut parsed.args, name, expected, (start, argv.len()));
        }

        Ok(parsed)
    }

    /// Check if there exists an argument with the given key (i.e. short or long flag).
    #[inline(always)]
    pub fn contains(&self, id: &str) -> bool {
        self.args
            .iter()
            .any(|arg| matches!(arg, Arg::Named(name, _) if id == *name))
    }

    /// Get one value for the given flag identifier.
    pub fn get_one<T>(&self, id: &str) -> Option<Option<T>>
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
    pub fn try_get_one<T>(&self, id: &str) -> Result<Option<Option<T>>, Error>
    where
        T: FromStr,
    {
        if let Some(Arg::Named(_, values)) = self
            .args
            .iter()
            .find(|&x| matches!(x, Arg::Named(name, _) if *name == id))
        {
            let mut iter = values.iter();

            let value = if let Some(value) = iter.next() {
                value
            } else {
                // The argument has no value.
                return Ok(Some(None));
            };

            if iter.next().is_some() {
                // The argument has more than one value.
                return Err(Error::InvalidArgument);
            }

            return value
                .parse::<T>()
                // The argument is present and has a value (i.e. Some(Some(_))).
                .map(Some)
                .map(Some)
                // The value cannot be parsed to the target type `T`.
                .map_err(|_| Error::InvalidArgument);
        }

        // The argument has not been found.
        Ok(None)
    }

    /// Try to get and parse the argument value if any. The value can be constructed from
    /// an iterator.
    pub fn try_get_many<B, T>(&self, id: &str) -> Result<Option<B>, Error>
    where
        B: FromIterator<T>,
        T: FromStr,
    {
        if let Some(Arg::Named(_, values)) = self
            .args
            .iter()
            .find(|&x| matches!(x, Arg::Named(name, _) if *name == id))
        {
            return Ok(Some(
                values
                    .iter()
                    .map(|x| x.parse::<T>())
                    .collect::<Result<B, _>>()
                    .map_err(|_| Error::InvalidArgument)?,
            ));
        }

        Ok(None)
    }

    fn check_capacity(argv: &[&str]) -> Result<()> {
        if CAPACITY < argv.len() {
            return Err(Error::OutOfMemory);
        }
        Ok(())
    }

    fn check_undefined_argument(argv: &[&str], table: &ArgLookupTable<'_>) -> Result<()> {
        let undefined = argv
            .iter()
            .map(|&x| Token::tokenize(x))
            .any(|x| matches!(x, Token::Flag(flag) if table.metadata_of(&flag).is_none()));

        if undefined {
            return Err(Error::UndefinedArgument);
        }

        Ok(())
    }

    fn parse_arg_values<'b>(
        argv: &'b [&'b str],
        name: &'b str,
        expected: AtMost,
    ) -> (Values<'b>, Arg<'b>) {
        match expected {
            AtMost::Zero => (Values::new(argv), Arg::Named(name, Values::empty())),
            AtMost::One => {
                let rest = if argv.len() <= 1 { &[] } else { &argv[1..] };
                let arg = if argv.is_empty() { &[] } else { &argv[..1] };
                (Values::new(rest), Arg::Named(name, Values::new(arg)))
            }
            AtMost::Many => (Values::empty(), Arg::Named(name, Values::new(argv))),
        }
    }
}

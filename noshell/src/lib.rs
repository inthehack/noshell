//! noshell, a `no_std` argument parser and a shell for constrained systems.
#![no_std]
#![deny(missing_docs)]

pub use noshell_macros::Parser;
pub use noshell_parser::{Error as ParserError, Lexer, ParsedArgs};

/// Defines the possible errors that may occur during usage of the crate.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[non_exhaustive]
pub enum Error {
    /// An error comes from the parsing of arguments.
    #[error(transparent)]
    Parser(#[from] ParserError),
}

#[cfg(test)]
mod tests {
    use googletest::prelude::{assert_that, eq, matches_pattern};

    use crate as noshell;

    #[derive(Debug, noshell::Parser)]
    struct MyArgs {
        id: u32,
        age: Option<u8>,
    }

    #[test]
    fn it_should_parse_args_with_empty_option() {
        let argv = &["--id", "233"];
        let res = MyArgs::parse(argv);

        assert_that!(res, matches_pattern!(Ok(_)));

        let args = res.unwrap();
        assert_that!(args.id, eq(233));
        assert_that!(args.age, eq(None));
    }

    #[test]
    fn it_should_parse_args_with_non_empty_option() {
        let argv = &["--id", "233", "--age", "32"];
        let res = MyArgs::parse(argv);

        assert_that!(res, matches_pattern!(Ok(_)));

        let args = res.unwrap();
        assert_that!(args.id, eq(233));
        assert_that!(args.age, eq(Some(32)));
    }
}

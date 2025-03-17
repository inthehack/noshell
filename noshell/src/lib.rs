//! noshell, a `no_std` argument parser and a shell for constrained systems.
#![no_std]
#![deny(missing_docs)]

pub use noshell_macros as macros;
pub use noshell_parser as parser;

pub use macros::Parser;

/// Defines the possible errors that may occur during usage of the crate.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[non_exhaustive]
pub enum Error {
    /// An error comes from the parsing of arguments.
    #[error(transparent)]
    Parser(#[from] parser::Error),
}

#[cfg(test)]
mod tests {
    use googletest::prelude::{assert_that, eq};

    use crate as noshell;

    #[test]
    fn it_should_parse_args_with_simple_type() {
        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            value: u32,
        }

        let argv = &["--value", "233"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value, eq(233));
    }

    #[test]
    fn it_should_parse_args_with_option_type() {
        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            value: Option<u32>,
        }

        let argv = &[];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value, eq(None));

        let argv = &["--value", "233"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value, eq(Some(233)));
    }

    #[test]
    fn it_should_parse_args_with_option_option_type() {
        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            value: Option<Option<u32>>,
        }

        let argv = &[];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value, eq(None));

        let argv = &["--value"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value, eq(Some(None)));
    }

    #[test]
    fn it_should_parse_args_with_option_vec_type() {
        use heapless::Vec;

        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            value: Option<Vec<u32, 8>>,
        }

        // No argument.
        let argv = &[];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));

        let args = res.unwrap();
        assert_that!(args.value.is_none(), eq(true));

        // Argument without value.
        let argv = &["--value"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(false));

        // Argument with single value.
        let argv = &["--value", "23"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));
    }
}

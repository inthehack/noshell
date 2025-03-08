#![no_std]

pub use noshell_macros::Parser;
pub use noshell_parser::{Lexer, ParsedArgs};

#[derive(Debug, Default, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Error {
    #[default]
    #[error("undefined error")]
    Undefined,

    #[error(transparent)]
    Parser(#[from] noshell_parser::Error),
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

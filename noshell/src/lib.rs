//! noshell, a `no_std` argument parser and a shell for constrained systems.
#![no_std]
#![allow(async_fn_in_trait)]
#![deny(missing_docs)]

use heapless::Vec;
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

    /// Command not found.
    #[error("command not found")]
    CommandNotFound,

    /// Invalid utf8 string.
    #[error("invalid utf8 string")]
    Utf8,
}

/// Command.
pub struct Command {
    runner: fn(&[&str], &mut [u8]) -> usize,
}

impl Command {
    /// Run the command.
    pub fn run(&self, args: &[&str], output: &mut [u8]) -> usize {
        (self.runner)(args, output)
    }
}

/// Parse top-level commands.
pub fn lookup(name: &str) -> Result<Command, Error> {
    let entries = unsafe {
        let start = (&NOSHELL_COMMANDS_START as *const u32).cast::<CommandEntry>();
        let end = (&NOSHELL_COMMANDS_END as *const u32).cast::<CommandEntry>();
        let len = (end as usize) - (start as usize);

        core::slice::from_raw_parts(start, len)
    };

    entries
        .iter()
        .find(|x| name == x.name)
        .map(|x| Command { runner: x.runner })
        .ok_or(Error::CommandNotFound)
}

#[repr(C)]
struct CommandEntry {
    name: &'static str,
    runner: fn(&[&str], &mut [u8]) -> usize,
}

unsafe extern "C" {
    static NOSHELL_COMMANDS_START: u32;
    static NOSHELL_COMMANDS_END: u32;
}

/// Character write trait.
pub trait Write {
    /// Error type.
    type Error;

    /// Write the given data to the underlying byte stream.
    async fn write(&mut self, data: &[u8]) -> Result<usize, Self::Error>;
}

/// Character read trait.
pub trait Read {
    /// Error type;
    type Error;

    /// Read some data from the underlying byte stream.
    async fn read(&self, data: &mut [u8]) -> Result<usize, Self::Error>;
}

/// Run the shell.
pub async fn start<IO: Read + Write>(mut io: IO) -> Result<(), Error> {
    let mut input = [0u8; 1024];
    let mut output = [0u8; 1024];

    let mut cursor = 0;

    loop {
        let args: Vec<&str, 32> = loop {
            match io.read(&mut input[cursor..]).await {
                Ok(len) => {
                    if let Some(eol) = input[cursor..cursor + len]
                        .iter()
                        .position(|&x| x as char == '\n')
                    {
                        let end = cursor + eol;
                        cursor = 0;

                        let line = str::from_utf8(&input[..end]).map_err(|_| Error::Utf8)?;
                        let args = line.split(" ").collect();

                        break args;
                    } else {
                        cursor += len;
                    }
                }

                Err(_) => {
                    cursor = 0;
                }
            }
        };

        let Some(name) = args.first() else {
            continue;
        };

        let Ok(cmd) = lookup(name) else {
            continue;
        };

        let len = cmd.run(&args[1..], &mut output);
        io.write(&output[..len]).await.ok();
    }
    // Ok(())
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
        let args = res.unwrap();

        assert_that!(args.value.is_some(), eq(true));
        let vals = args.value.unwrap();

        assert_that!(vals.is_empty(), eq(false));
        assert_that!(vals.first().unwrap(), eq(&23));

        // Argument with multiple values.
        let argv = &["--value", "23", "42", "72"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));
        let args = res.unwrap();

        assert_that!(args.value.is_some(), eq(true));
        let vals = args.value.unwrap();

        assert_that!(vals.is_empty(), eq(false));
        let mut iter = vals.iter();

        assert_that!(iter.next().unwrap(), eq(&23));
        assert_that!(iter.next().unwrap(), eq(&42));
        assert_that!(iter.next().unwrap(), eq(&72));
        assert_that!(iter.next(), eq(None));
    }

    #[test]
    #[should_panic]
    fn it_should_panic_at_parsing_args_with_option_vec_type() {
        use heapless::Vec;

        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            #[allow(unused)]
            value: Option<Vec<u32, 4>>,
        }

        // Argument with too much values.
        let argv = &["--value", "1", "2", "3", "4", "5"];
        let _ = MyArgs::parse(argv);
    }

    #[test]
    fn it_should_parse_args_with_vec_type() {
        use heapless::Vec;

        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            value: Vec<u32, 8>,
        }

        // No argument.
        let argv = &[];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(false));

        // Argument without value.
        let argv = &["--value"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(false));

        // Argument with single value.
        let argv = &["--value", "23"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));
        let args = res.unwrap();

        assert_that!(args.value.is_empty(), eq(false));
        assert_that!(args.value.first().unwrap(), eq(&23));

        // Argument with multiple values.
        let argv = &["--value", "23", "42", "72"];
        let res = MyArgs::parse(argv);

        assert_that!(res.is_ok(), eq(true));
        let args = res.unwrap();

        assert_that!(args.value.is_empty(), eq(false));
        let mut iter = args.value.iter();

        assert_that!(iter.next().unwrap(), eq(&23));
        assert_that!(iter.next().unwrap(), eq(&42));
        assert_that!(iter.next().unwrap(), eq(&72));
        assert_that!(iter.next(), eq(None));
    }

    #[test]
    #[should_panic]
    fn it_should_panic_at_parsing_args_with_vec_type() {
        use heapless::Vec;

        #[derive(Debug, noshell::Parser)]
        struct MyArgs {
            #[allow(unused)]
            value: Vec<u32, 4>,
        }

        // Argument with too much values.
        let argv = &["--value", "1", "2", "3", "4", "5"];
        let _ = MyArgs::parse(argv);
    }
}

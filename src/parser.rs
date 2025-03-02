use crate::lexer::{self, Token};

#[derive(Debug, Default, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[default]
    #[error("undefined parsing error")]
    Undefined,

    #[error(transparent)]
    Lexer(#[from] lexer::Error),
}

#[derive(Debug)]
pub struct MyArgs {
    // -v, --value
    pub value: i32,
}

impl MyArgs {
    pub fn parse<'a, I>(tokens: &mut I) -> Result<MyArgs, Error>
    where
        I: Iterator<Item = Token<'a>>,
    {
        let mut args: MyArgs = unsafe { ::core::mem::zeroed() };

        while let Some(token) = tokens.next() {
            match token {
                // New short arg.
                Token::Short(key) => {
                    if key == 'v' {
                        if let Some(Token::Value(value)) = tokens.next() {
                            args.value = value.try_into()?;
                        } else {
                            // Missing value.
                            return Err(Error::Undefined);
                        }
                    }
                }

                // New long arg.
                Token::Long(key) => {
                    if key == "value" {
                        if let Some(Token::Value(value)) = tokens.next() {
                            args.value = value.try_into()?;
                        } else {
                            // Missing value.
                            return Err(Error::Undefined);
                        }
                    }
                }

                Token::Value(_) => {}
            }
        }

        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use crate::Lexer;

    use super::*;

    #[test]
    fn it_should_parse_i32_short_arg() {
        let cmdline = &["-v", "2"];

        let mut lexer = Lexer::new(cmdline);
        let args = MyArgs::parse(&mut lexer);

        assert_that!(args.is_ok(), eq(true));
        assert_that!(args.unwrap().value, eq(2));
    }

    #[test]
    fn it_should_parse_i32_long_arg() {
        let cmdline = &["--value", "2"];

        let mut lexer = Lexer::new(cmdline);
        let args = MyArgs::parse(&mut lexer);

        assert_that!(args.is_ok(), eq(true));
        assert_that!(args.unwrap().value, eq(2));
    }

    #[test]
    fn it_should_fail_i32_short_arg_with_missing_value() {
        let cmdline = &["-v"];

        let mut lexer = Lexer::new(cmdline);
        let args = MyArgs::parse(&mut lexer);

        assert_that!(args.is_err(), eq(true));
        assert_that!(args.err().unwrap(), eq(&Error::Undefined));
    }
}

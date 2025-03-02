#[derive(Debug, Default, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[default]
    #[error("undefined error")]
    Undefined,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Token<'a> {
    Short(char),
    Long(&'a str),
    Value(Value<'a>),
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Value<'a> {
    Str(&'a str),
    Int32(i32),
    Uint32(u32),
    Float32(f32),
}

impl<'a> Value<'a> {
    pub fn parse<'b>(input: &'b str) -> Self
    where
        'b: 'a,
    {
        if let Ok(value) = input.parse::<i32>() {
            return Value::Int32(value);
        }

        if let Ok(value) = input.parse::<u32>() {
            return Value::Uint32(value);
        }

        if let Ok(value) = input.parse::<f32>() {
            return Value::Float32(value);
        }

        Value::Str(input)
    }
}

macro_rules! make_from_value_impl {
    ($variant:ident, $target:ty) => {
        impl TryFrom<Value<'_>> for $target {
            type Error = Error;

            fn try_from(value: Value<'_>) -> Result<Self, Self::Error> {
                match value {
                    Value::$variant(x) => Ok(x),
                    _ => Err(Error::Undefined),
                }
            }
        }
    };
}

make_from_value_impl!(Int32, i32);
make_from_value_impl!(Uint32, u32);
make_from_value_impl!(Float32, f32);

impl<'a, 'b> TryFrom<Value<'a>> for &'b str
where
    'a: 'b,
{
    type Error = Error;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Str(x) => Ok(x),
            _ => Err(Error::Undefined),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    args: &'a [&'a str],
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(args: &'a [&'a str]) -> Self {
        Lexer { args, cursor: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if self.cursor >= self.args.len() {
            return None;
        }

        let arg = self.args[self.cursor];
        self.cursor += 1;

        // Long flag.
        if arg.starts_with("--") && arg.len() >= 3 {
            let (_, name) = arg.split_at(2);
            return Some(Token::Long(name));
        }

        // Look up for signed numerical value (e.g. -3, -2.5).
        let value = Value::parse(arg);

        // Short argument.
        if arg.starts_with('-') && arg.len() == 2 && matches!(value, Value::Str(_)) {
            let (_, name) = arg.split_at(1);
            return Some(Token::Short(name.chars().nth(0).unwrap_or_default()));
        }

        Some(Token::Value(value))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use super::*;

    #[test]
    fn it_should_count_tokens() {
        let mut lexer = Lexer::new(&["-f", "--flag", "value", "-2", "-2.5", "2"]);

        assert_that!(lexer.next_token(), eq(&Some(Token::Short('f'))));
        assert_that!(lexer.next_token(), eq(&Some(Token::Long("flag"))));
        assert_that!(
            lexer.next_token(),
            eq(&Some(Token::Value(Value::Str("value"))))
        );
        assert_that!(
            lexer.next_token(),
            eq(&Some(Token::Value(Value::Int32(-2))))
        );
        assert_that!(
            lexer.next_token(),
            eq(&Some(Token::Value(Value::Float32(-2.5))))
        );
        assert_that!(lexer.next_token(), eq(&Some(Token::Value(Value::Int32(2)))));
        assert_that!(lexer.next_token(), eq(&None));
    }
}

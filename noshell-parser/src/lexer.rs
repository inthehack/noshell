//! A lexer for generating tokens from a command line.

/// Defines a `Token` that has been read from the command line.
#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Token<'a> {
    /// Short flag (e.g. -f).
    ShortFlag(char),

    /// Long flag (e.g. --flag).
    LongFlag(&'a str),

    /// Value (i.e. everything that is not a short or long flag).
    Value(&'a str),
}

/// Defines a `Lexer` that is responsible for streaming tokens from the command line input.
///
/// A lexer acts like an forward iterator.
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    args: &'a [&'a str],
    cursor: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from the command line input.
    pub fn new(args: &'a [&'a str]) -> Self {
        Lexer { args, cursor: 0 }
    }

    /// Retrieve the next token on the command line if any.
    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if self.cursor >= self.args.len() {
            return None;
        }

        let arg = self.args[self.cursor];
        self.cursor += 1;

        // Long flag.
        if arg.starts_with("--") && arg.len() >= 3 {
            let (_, name) = arg.split_at(2);
            return Some(Token::LongFlag(name));
        }

        // Numbers.
        if arg.starts_with('-') && is_number(arg) {
            return Some(Token::Value(arg));
        }

        // Short flag.
        if arg.starts_with('-') && arg.len() == 2 {
            let (_, name) = arg.split_at(1);
            return Some(Token::ShortFlag(name.chars().nth(0).unwrap_or_default()));
        }

        Some(Token::Value(arg))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

fn is_number(input: &str) -> bool {
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

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use super::*;

    #[test]
    fn it_should_match_short_flag() {
        let mut lexer = Lexer::new(&["-f"]);

        let token = lexer.next_token();
        assert_that!(token.is_some(), eq(true));
        assert_that!(token.unwrap(), eq(Token::ShortFlag('f')));
    }

    #[test]
    fn it_should_match_value_starting_with_dash() {
        let mut lexer = Lexer::new(&["-flag"]);

        let token = lexer.next_token();
        assert_that!(token.is_some(), eq(true));
        assert_that!(token.unwrap(), eq(Token::Value("-flag")));
    }

    #[test]
    fn it_should_match_long_flag() {
        let mut lexer = Lexer::new(&["--flag"]);

        let token = lexer.next_token();
        assert_that!(token.is_some(), eq(true));
        assert_that!(token.unwrap(), eq(Token::LongFlag("flag")));
    }

    #[test]
    fn it_should_match_numbers() {
        let lexer = Lexer::new(&["-2", "2", "-2.", "2.", "-2.e1", "2.e1", "-2e1", "2e1"]);

        for token in lexer {
            assert_that!(token, matches_pattern!(&Token::Value(_)));
        }
    }
}

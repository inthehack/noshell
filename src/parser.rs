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
    pub fn get_one<'k>(&self, key: &'k str) -> Option<lexer::Token<'a>>
    where
        'a: 'k,
    {
        self.args
            .iter()
            .find(|x| match_key_with_token(key, x))
            .copied()
    }

    pub fn get_many<'k>(&self, key: &'k str) -> Option<Vec<lexer::Token<'a>, ARG_MATCHES_SIZE_MAX>>
    where
        'a: 'k,
    {
        let iter = self.args.iter().filter(|x| match_key_with_token(key, x));
        debug_assert!(iter.clone().count() <= ARG_MATCHES_SIZE_MAX);

        let tokens = iter.copied().collect();
        Some(tokens)
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
}

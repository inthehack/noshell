pub use noshell_macros::Parser;
pub use noshell_parser::{Lexer, ParsedArgs};

#[derive(Debug, Default, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub enum Error {
    #[default]
    #[error("undefined error")]
    Undefined,
}

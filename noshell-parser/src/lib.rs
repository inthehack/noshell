//! noshell parser provides an minimal API for parsing arguments from a byte stream.

#![no_std]
#![deny(missing_docs)]

/// Lexer for generating tokens from the command line.
pub mod lexer;

/// Parser for collecting argument flags and values from a token stream.
pub mod parser;

pub use lexer::Lexer;
pub use parser::{Error, ParsedArgs};

//! Prompt.

use noterm::Queuable;
use noterm::cursor::MoveToNextLine;
use noterm::style::Print;

use super::Result;

/// A Prompt is composed of several styled string parts.
#[derive(Debug)]
pub struct Prompt<'a> {
    parts: &'a str,
}

impl<'a> Prompt<'a> {
    /// Create a new prompt from contents.
    pub fn new(parts: &'a str) -> Self {
        Prompt { parts }
    }
}

impl Prompt<'_> {
    /// Reset the prompt and print it to the output.
    pub fn reset<OutputTy>(&self, output: &mut OutputTy) -> Result<()>
    where
        OutputTy: noterm::io::blocking::Write,
    {
        output.queue(MoveToNextLine(1))?;
        output.queue(Print(format_args!("{} ", self.parts)))?;
        output.flush()?;
        Ok(())
    }
}

impl Default for Prompt<'_> {
    fn default() -> Self {
        Prompt::new("shell $")
    }
}

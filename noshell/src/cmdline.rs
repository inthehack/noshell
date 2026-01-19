//! Line parsing.

use core::fmt;

use futures::{Stream, StreamExt, pin_mut};
use heapless::String;
use noterm::cursor::{MoveRight, MoveToNextLine};
use noterm::events::{Event, KeyCode, KeyEvent};
use noterm::io::blocking::Write;
use noterm::style::Print;
use noterm::{Executable, Queuable};

pub mod lexer;
pub mod prompt;

pub use prompt::Prompt;

#[cfg(test)]
mod tests;

#[cfg(test)]
extern crate std;

/// Error.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Input/ouput error.
    #[error(transparent)]
    Io(#[from] noterm::io::Error),

    /// Unknown error.
    #[error("unknown error")]
    Unknown,
}

/// Re-export result type.
pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Read a line.
pub async fn readline<OutputTy, EventsTy, ContentTy, const SIZE: usize>(
    output: &mut OutputTy,
    events: EventsTy,
    prompt: Prompt<ContentTy>,
) -> Result<String<SIZE>>
where
    OutputTy: Write,
    EventsTy: Stream<Item = noterm::io::Result<Event>>,
    ContentTy: Iterator + Clone,
    <ContentTy as Iterator>::Item: fmt::Display,
{
    // Prepare the output of the line.
    let mut line: String<SIZE> = String::new();

    // Write the prompt, then read for input events.
    prompt.reset(output)?;

    // Pin the events, so that it stays on the stack while calling async/await.
    pin_mut!(events);

    // Create the escaped state.
    let mut escaped = false;

    loop {
        match events.next().await {
            Some(Ok(event)) => match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    modifiers: _,
                    kind: _,
                }) if !escaped => break,

                Event::Key(KeyEvent {
                    code,
                    modifiers: _,
                    kind: _,
                }) => match code {
                    KeyCode::Enter if escaped => {
                        let _ = line.push('\n');
                        output.queue(MoveToNextLine(1))?;
                        output.queue(MoveRight(4))?;
                        output.flush()?;
                        escaped = false;
                    }

                    KeyCode::Char(c) => {
                        let _ = line.push(c);
                        output.execute(Print(c))?;
                        escaped = c == '\\';
                    }

                    _ => {}
                },

                _ => {}
            },

            Some(Err(err)) => return Err(Error::from(err)),
            None => break,
        }
    }

    Ok(unescape::<SIZE>(&line))
}

fn unescape<const SIZE: usize>(input: &str) -> heapless::String<SIZE> {
    let (acc, _) =
        input.chars().fold(
            (heapless::String::new(), false),
            |(mut acc, escaped), c| match escaped {
                // If the character is escaped and is special, consume it as unescaped.
                true if ['$', '"', '\\'].contains(&c) => {
                    let _ = acc.push(c);
                    (acc, false)
                }

                // If the character is a newline, preceded by a backslash, discard both.
                true if '\n' == c => (acc, false),

                // If the character is escaped but not special, consume it as escaped.
                true => {
                    let _ = acc.push('\\');
                    let _ = acc.push(c);
                    (acc, false)
                }

                // If character is not a backslash, then consume it.
                false if c != '\\' => {
                    let _ = acc.push(c);
                    (acc, false)
                }

                // If the character is a backslash, discard it but keep memory of it.
                false => (acc, true),
            },
        );

    acc
}

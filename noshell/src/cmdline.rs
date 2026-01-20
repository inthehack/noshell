//! Line parsing.

use core::fmt;

use futures::{Stream, StreamExt, pin_mut};
use heapless::String;
use noterm::cursor::{Home, MoveLeft, MoveRight, MoveToNextLine};
use noterm::events::{Event, KeyCode, KeyEvent, KeyModifiers};
use noterm::io;
use noterm::style::Print;
use noterm::terminal::{Clear, ClearType};
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

    /// End of events.
    #[error("no more events")]
    NoMoreEvents,

    /// Unknown error.
    #[error("unknown error")]
    Unknown,
}

/// Re-export result type.
pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Read a line.
pub async fn readline<OutputTy, EventsTy, ContentTy, const SIZE: usize>(
    prompt: &Prompt<ContentTy>,
    events: EventsTy,
    output: &mut OutputTy,
) -> Result<String<SIZE>>
where
    OutputTy: io::blocking::Write,
    EventsTy: Stream<Item = io::Result<Event>>,
    ContentTy: Iterator + Clone,
    <ContentTy as Iterator>::Item: fmt::Display,
{
    // Prepare the output of the line.
    let mut line: Line<SIZE> = Line::default();

    // Write the prompt, then read for input events.
    prompt.reset(output)?;

    // Pin the events, so that it stays on the stack while calling async/await.
    pin_mut!(events);

    loop {
        match events.next().await {
            Some(Ok(event)) => match event {
                Event::Key(key_event) => {
                    if let Some(contents) = line.on_key_event(key_event, prompt, output)? {
                        return Ok(unescape::<SIZE>(contents));
                    };
                }
                Event::Cursor(_) => {}
                Event::Screen(_) => {}
            },

            Some(Err(err)) => return Err(Error::from(err)),
            None => return Err(Error::NoMoreEvents),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum LineStatus {
    Done,
    Pending,
}

#[derive(Debug, Default)]
struct Line<const SIZE: usize = 256> {
    escaped: bool,
    buffer: String<SIZE>,
}

impl<const SIZE: usize> Line<SIZE> {
    fn contents(&self) -> &str {
        self.buffer.as_str()
    }

    fn on_key_event<ContentTy, WriterTy>(
        &mut self,
        event: KeyEvent,
        prompt: &Prompt<ContentTy>,
        output: &mut WriterTy,
    ) -> Result<Option<&str>>
    where
        ContentTy: Iterator + Clone,
        <ContentTy as Iterator>::Item: fmt::Display,
        WriterTy: io::blocking::Write,
    {
        let KeyEvent {
            code,
            modifiers,
            kind: _,
        } = event;

        let is_ctrl_modified = modifiers.contains(KeyModifiers::CONTROL);
        let is_shift_modified = modifiers.contains(KeyModifiers::SHIFT);

        if is_ctrl_modified && on_ctrl_key_event(code, prompt, output)? == LineStatus::Done {
            return Ok(None);
        }

        if KeyCode::Enter == code && !self.escaped {
            return Ok(Some(self.contents()));
        }

        if KeyCode::Enter == code && self.escaped {
            let _ = self.buffer.push('\n');
            output.queue(MoveToNextLine(1))?;
            output.queue(MoveRight(4))?;
            output.flush()?;
            self.escaped = false;
            return Ok(None);
        }

        if KeyCode::Backspace == code {
            output
                .queue(MoveLeft(self.buffer.len() as u16))?
                .queue(Clear(ClearType::LineFromCursor))?
                .flush()?;

            self.buffer.pop();
            output.execute(Print(self.contents()))?;
            return Ok(None);
        }

        if let KeyCode::Char(c) = code {
            let cased = if c.is_alphabetic() && is_shift_modified {
                c.to_ascii_uppercase()
            } else {
                c
            };

            let _ = self.buffer.push(cased);
            output.execute(Print(cased))?;

            self.escaped = c == '\\';
            return Ok(None);
        }

        Ok(None)
    }
}

fn on_ctrl_key_event<ContentTy, WriterTy>(
    key: KeyCode,
    prompt: &Prompt<ContentTy>,
    output: &mut WriterTy,
) -> Result<LineStatus>
where
    ContentTy: Iterator + Clone,
    <ContentTy as Iterator>::Item: fmt::Display,
    WriterTy: io::blocking::Write,
{
    let status = match key {
        KeyCode::Char('l') => {
            output.queue(Clear(ClearType::All))?.queue(Home)?.flush()?;
            prompt.reset(output)?;
            LineStatus::Done
        }

        _ => LineStatus::Pending,
    };

    Ok(status)
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

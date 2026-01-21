//! Line parsing.

use core::fmt;

use futures::{Stream, StreamExt, pin_mut};
use heapless::{CapacityError, String};
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

    /// No more events from the stream.
    #[error("no more events")]
    NoMoreEvents,

    /// No space left in line buffer.
    #[error("no space left")]
    NoSpaceLeft,

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
    cursor: usize,
}

impl<const SIZE: usize> Line<SIZE> {
    const _ASSERT_SIZE_IS_U16_CONVERTIBLE: () =
        assert!(SIZE <= u16::MAX as usize, "SIZE must be less than 65535");

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
            return Ok(Some(self.buffer.as_str()));
        }

        if KeyCode::Enter == code && self.escaped {
            self.buffer.push('\n')?;

            output
                .queue(MoveToNextLine(1))?
                .queue(MoveRight(4))?
                .flush()?;

            self.cursor += 1;
            self.escaped = false;
            return Ok(None);
        }

        if KeyCode::Backspace == code {
            if self.cursor == 0 {
                return Ok(None);
            }

            self.buffer.remove(self.cursor - 1);
            let (_, updated) = self.buffer.split_at(self.cursor - 1);

            output
                .queue(MoveLeft(1))?
                .queue(Clear(ClearType::LineFromCursor))?
                .flush()?;

            if !updated.is_empty() {
                output
                    .queue(Print(updated))?
                    .queue(MoveLeft(updated.len() as u16))?
                    .flush()?;
            }

            self.cursor -= 1;
            return Ok(None);
        }

        if let KeyCode::Char(c) = code {
            let cased = if c.is_alphabetic() && is_shift_modified {
                c.to_ascii_uppercase()
            } else {
                c
            };

            let is_cursor_eol = self.cursor == self.buffer.len();

            if is_cursor_eol {
                self.buffer.push(cased)?;
                output.execute(Print(cased))?;
            } else {
                self.buffer.insert(self.cursor, cased)?;
                let (_, updated) = self.buffer.split_at(self.cursor);

                output
                    .queue(Clear(ClearType::LineFromCursor))?
                    .queue(Print(updated))?
                    .queue(MoveLeft((updated.len() - 1) as u16))?
                    .flush()?;
            };

            self.cursor += 1;
            self.escaped = c == '\\';
            return Ok(None);
        }

        match code {
            KeyCode::Left => {
                let old = self.cursor;
                self.cursor = self.cursor.saturating_sub(1);

                if old != self.cursor {
                    output.execute(MoveLeft(1))?;
                }
            }

            KeyCode::Right => {
                let old = self.cursor;
                self.cursor = self.cursor.saturating_add(1).min(self.buffer.len());

                if old != self.cursor {
                    output.execute(MoveRight(1))?;
                }
            }

            _ => {}
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

impl From<CapacityError> for Error {
    fn from(_: CapacityError) -> Self {
        Error::NoSpaceLeft
    }
}

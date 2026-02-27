//! Console.

use core::cell::UnsafeCell;

use futures::Stream;

use heapless::{String, Vec};
use noterm::cursor::Home;
use noterm::events::Event;
use noterm::terminal::{Clear, ClearType};
use noterm::{Queuable, io};

use crate::line;
use crate::line::Prompt;

/// Error.
#[derive(Debug, PartialEq, Eq, Hash, thiserror::Error)]
pub enum Error {
    /// Current command line is cancelled by the user.
    #[error("command line cancelled")]
    Cancelled,

    /// Console had been terminated by user.
    #[error("console terminated")]
    Terminated,

    /// Invalid
    #[error("user input unexpected")]
    Unexpected,

    /// No space left in internal buffers.
    #[error("no space left")]
    NoSpaceLeft,

    /// Other error.
    #[error("other")]
    Other,

    /// Error from io.
    #[error(transparent)]
    Io(#[from] io::Error),
}

/// Re-export of result type.
pub type Result<T, E = Error> = core::result::Result<T, E>;

impl From<line::lexer::Error<'_>> for Error {
    fn from(value: line::lexer::Error<'_>) -> Self {
        match value {
            line::lexer::Error::Unexpected(_) => Error::Unexpected,
        }
    }
}

impl From<line::Error> for Error {
    fn from(value: line::Error) -> Self {
        match value {
            line::Error::Io(err) => Error::Io(err),
            line::Error::Cancelled => Error::Cancelled,
            line::Error::NoMoreEvents => Error::Terminated,
            line::Error::NoSpaceLeft => Error::NoSpaceLeft,
        }
    }
}

/// Capacity constants.
const LINE_BUFFER_CAPACITY: usize = 256;
const ARGV_BUFFER_CAPACITY: usize = 32;

/// A console.
pub struct Console<'a, EventsTy, WriterTy> {
    prompt: Prompt<'a>,
    events: UnsafeCell<&'a mut EventsTy>,
    writer: UnsafeCell<&'a mut WriterTy>,
}

impl<'a, EventsTy, WriterTy> Console<'a, EventsTy, WriterTy>
where
    EventsTy: Stream<Item = io::Result<Event>> + Unpin + 'a,
    WriterTy: io::blocking::Write + 'a,
{
    /// Create a new console.
    pub fn new(events: &'a mut EventsTy, output: &'a mut WriterTy) -> Self {
        Console {
            prompt: Prompt::new("shell $"),
            events: UnsafeCell::new(events),
            writer: UnsafeCell::new(output),
        }
    }

    /// Clear the console.
    pub fn clear(&self) -> Result<()> {
        let output = unsafe { &mut **self.writer.get() };
        output.queue(Clear(ClearType::All))?;
        output.queue(Home)?;
        output.flush()?;
        Ok(())
    }

    /// Get writer.
    pub fn writer(&mut self) -> &mut WriterTy
    where
        WriterTy: io::blocking::Write + 'a,
    {
        unsafe { &mut **self.writer.get() }
    }

    /// Process user input.
    pub async fn process<'p, HandlerTy, FutureTy>(&self, handler: HandlerTy) -> Result<()>
    where
        HandlerTy: FnOnce(&'p [&'p str], &'p mut WriterTy) -> FutureTy + 'p,
        FutureTy: Future<Output = ()>,
        'a: 'p,
    {
        let stream = unsafe { &mut **self.events.get() };
        let writer = unsafe { &mut **self.writer.get() };

        let line: String<LINE_BUFFER_CAPACITY> =
            match line::readline(&self.prompt, stream, writer).await {
                Ok(line) => line,

                Err(line::Error::Io(err)) => return Err(Error::Io(err)),
                Err(line::Error::Cancelled) => {
                    noterm::print!(writer, "\r\n");
                    return Err(Error::Cancelled);
                }
                Err(line::Error::NoMoreEvents) => return Err(Error::Terminated),
                Err(line::Error::NoSpaceLeft) => return Err(Error::NoSpaceLeft),
            };

        noterm::print!(writer, "\r\n");

        // Skip comment.
        if line.starts_with('#') {
            return Ok(());
        }

        // Prepare arguments.
        let argv: Vec<_, ARGV_BUFFER_CAPACITY> =
            match line::lexer::split(line.as_str()).try_collect() {
                Ok(argv) => argv,

                Err(line::lexer::Error::Unexpected(tokens)) => {
                    noterm::println!(writer, "unexpected tokens `{}`", tokens);
                    return Err(Error::Cancelled);
                }
            };

        // SAFETY: it is safe to consider that argv still lives after the `await`.
        let argv = unsafe { core::mem::transmute::<&[&str], &[&str]>(argv.as_slice()) };

        handler(argv, writer).await;
        Ok(())
    }
}

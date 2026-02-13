//! Console.

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
    /// Error from io.
    #[error(transparent)]
    Io(#[from] io::Error),

    /// Console had been terminated by user.
    #[error("terminated")]
    Terminated,

    /// No space left in internal buffers.
    #[error("no space left")]
    NoSpaceLeft,

    /// Unknown error, for development only.
    #[error("unknown error")]
    Unknown,
}

/// Re-export of result type.
pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Capacity constants.
const LINE_BUFFER_CAPACITY: usize = 256;
const ARGV_BUFFER_CAPACITY: usize = 32;

/// A console.
pub struct Console<'a, EventsTy, WriterTy> {
    prompt: Prompt<'a>,
    events: EventsTy,
    writer: WriterTy,
}

impl<'a, EventsTy, WriterTy> Console<'a, EventsTy, WriterTy>
where
    EventsTy: Stream<Item = io::Result<Event>> + Unpin,
    WriterTy: io::blocking::Write,
{
    /// Create a new console.
    pub fn new(events: EventsTy, output: WriterTy) -> Self {
        Console {
            prompt: Prompt::new("shell $"),
            events,
            writer: output,
        }
    }

    /// Clear the console.
    pub fn clear(&mut self) -> Result<()> {
        self.writer.queue(Clear(ClearType::All))?;
        self.writer.queue(Home)?;
        self.writer.flush()?;
        Ok(())
    }

    /// Get output writer.
    pub fn writer<'b>(&'b mut self) -> ConsoleWriter<'b> {
        ConsoleWriter(&mut self.writer)
    }

    /// Process user input.
    pub async fn process(&mut self) -> Result<()> {
        let line: String<LINE_BUFFER_CAPACITY> =
            match line::readline(&self.prompt, &mut self.events, &mut self.writer).await {
                Ok(line) => line,

                Err(line::Error::Io(err)) => return Err(Error::Io(err)),
                Err(line::Error::NoMoreEvents) => return Err(Error::Terminated),
                Err(line::Error::NoSpaceLeft) => return Err(Error::NoSpaceLeft),
                Err(line::Error::Unknown) => return Err(Error::Unknown),
            };

        noterm::print!(&mut self.writer, "\r\n");

        // Skip comment.
        if line.starts_with('#') {
            return Ok(());
        }

        // Prepare arguments.
        let argv: Vec<_, ARGV_BUFFER_CAPACITY> = match line::lexer::split(&line).try_collect() {
            Ok(argv) => argv,

            Err(line::lexer::Error::InvalidInput(failed)) => {
                noterm::println!(&mut self.writer, "error: unexpected token `{}`", failed);
                return Ok(());
            }
            Err(line::lexer::Error::Unknown) => return Err(Error::Unknown),
        };

        self.execute(&argv).await?;
        Ok(())
    }

    /// Execute the command.
    pub(crate) async fn execute(&mut self, argv: &[&str]) -> Result<()> {
        handler(argv, &mut self.writer());
        Ok(())
    }
}

fn handler(argv: &[&str], writer: &mut ConsoleWriter<'_>) {
    noterm::println!(writer, "execute `{:?}`!", argv);
}

/// Console writer.
pub struct ConsoleWriter<'a>(&'a mut dyn io::blocking::Write);

impl ConsoleWriter<'_> {
    /// Clear the console output.
    pub fn clear(&mut self) -> Result<()> {
        use io::blocking::Write;

        self.queue(Clear(ClearType::All))?;
        self.queue(Home)?;
        self.flush()?;
        Ok(())
    }
}

impl io::blocking::Write for ConsoleWriter<'_> {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.0.write(data)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

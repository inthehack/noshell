//! Console.

use futures::Stream;

use heapless::{String, Vec};
use noterm::cursor::Home;
use noterm::events::Event;
use noterm::terminal::{Clear, ClearType};
use noterm::{Queuable, io};

use crate::command::Command;
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

    /// Command not found.
    #[error("command not found")]
    CommandNotFound,

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

/// Capacity constants.
const COMMAND_DYN_CAPACITY: usize = 32;
const LINE_BUFFER_CAPACITY: usize = 256;
const ARGV_BUFFER_CAPACITY: usize = 32;

/// A console.
pub struct Console<'a, EventsTy, WriterTy> {
    prompt: Prompt<'a>,
    events: EventsTy,
    writer: WriterTy,
    commands: Vec<Command, COMMAND_DYN_CAPACITY>,
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
            commands: Vec::new(),
        }
    }

    /// Add a command.
    pub fn register(&mut self, command: Command) -> Result<&mut Self, Command> {
        self.commands.push(command)?;
        Ok(self)
    }

    /// Add a command, without check.
    ///
    /// # Safety
    ///
    /// This function may panic if the capacity is full.
    pub unsafe fn register_unchecked(&mut self, command: Command) -> &mut Self {
        unsafe { self.register(command).unwrap_unchecked() };
        self
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
            };

        noterm::print!(&mut self.writer, "\r\n");

        // Skip comment.
        if line.starts_with('#') {
            return Ok(());
        }

        // Prepare arguments.
        let argv: Vec<_, ARGV_BUFFER_CAPACITY> = line::lexer::split(&line).try_collect()?;

        self.execute(&argv).await?;
        Ok(())
    }

    /// Execute the command.
    pub(crate) async fn execute(&mut self, argv: &[&str]) -> Result<()> {
        let Some(arg0) = argv.first().copied() else {
            return Ok(());
        };

        let Some(command) = self.commands.iter().copied().find(|x| arg0 == x.name()) else {
            noterm::println!(&mut self.writer, "error: command `{}` not found", arg0);
            return Err(Error::CommandNotFound);
        };

        command.execute(&argv[1..], self.writer());
        Ok(())
    }
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

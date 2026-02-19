//! Console.

use core::cell::UnsafeCell;

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
const COMMAND_DYN_CAPACITY: usize = 32;
const LINE_BUFFER_CAPACITY: usize = 256;
const ARGV_BUFFER_CAPACITY: usize = 32;

/// A console.
pub struct Console<'a, EventsTy, OutputTy> {
    prompt: Prompt<'a>,
    events: UnsafeCell<&'a mut EventsTy>,
    output: UnsafeCell<&'a mut OutputTy>,
    commands: Vec<Command, COMMAND_DYN_CAPACITY>,
}

impl<'a, EventsTy, OutputTy> Console<'a, EventsTy, OutputTy>
where
    EventsTy: Stream<Item = io::Result<Event>> + Unpin,
    OutputTy: io::blocking::Write,
{
    /// Create a new console.
    pub fn new(events: &'a mut EventsTy, output: &'a mut OutputTy) -> Self {
        Console {
            prompt: Prompt::new("shell $"),
            events: UnsafeCell::new(events),
            output: UnsafeCell::new(output),
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
    pub fn clear(&self) -> Result<()> {
        let output = unsafe { &mut **self.output.get() };
        output.queue(Clear(ClearType::All))?;
        output.queue(Home)?;
        output.flush()?;
        Ok(())
    }

    /// Get output writer.
    pub fn writer<'b>(&'b self) -> ConsoleWriter<'b> {
        ConsoleWriter(unsafe { &mut **self.output.get() })
    }

    /// Process user input.
    pub async fn process(&mut self) -> Result<()> {
        let stream = unsafe { &mut **self.events.get() };
        let output = unsafe { &mut **self.output.get() };

        let line: String<LINE_BUFFER_CAPACITY> =
            match line::readline(&self.prompt, stream, output).await {
                Ok(line) => line,

                Err(line::Error::Io(err)) => return Err(Error::Io(err)),
                Err(line::Error::Cancelled) => {
                    noterm::print!(output, "\r\n");
                    return Err(Error::Cancelled);
                }
                Err(line::Error::NoMoreEvents) => return Err(Error::Terminated),
                Err(line::Error::NoSpaceLeft) => return Err(Error::NoSpaceLeft),
            };

        noterm::print!(output, "\r\n");

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
            let output = unsafe { &mut **self.output.get() };
            noterm::println!(output, "error: command `{}` not found", arg0);
            return Err(Error::Cancelled);
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

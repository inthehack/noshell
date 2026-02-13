//! Console.

use futures::Stream;

use heapless::{String, Vec};
use noterm::events::Event;
use noterm::io;

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
pub struct Console<'a, EventsTy, OutputTy> {
    prompt: Prompt<'a>,
    events: EventsTy,
    output: OutputTy,
}

impl<'a, EventsTy, OutputTy> Console<'a, EventsTy, OutputTy>
where
    EventsTy: Stream<Item = io::Result<Event>> + Unpin,
    OutputTy: io::blocking::Write,
{
    /// Create a new console.
    pub fn new(events: EventsTy, output: OutputTy) -> Self {
        Console {
            prompt: Prompt::new("shell $"),
            events,
            output,
        }
    }

    /// Process user input.
    pub async fn process(&mut self) -> Result<()> {
        let line: String<LINE_BUFFER_CAPACITY> =
            match line::readline(&self.prompt, &mut self.events, &mut self.output).await {
                Ok(line) => line,

                Err(line::Error::Io(err)) => return Err(Error::Io(err)),
                Err(line::Error::NoMoreEvents) => return Err(Error::Terminated),
                Err(line::Error::NoSpaceLeft) => return Err(Error::NoSpaceLeft),
                Err(line::Error::Unknown) => return Err(Error::Unknown),
            };

        // Skip comment.
        if line.starts_with('#') {
            return Ok(());
        }

        // Prepare arguments.
        let argv: Vec<_, ARGV_BUFFER_CAPACITY> = match line::lexer::split(&line).try_collect() {
            Ok(argv) => argv,

            Err(line::lexer::Error::InvalidInput(failed)) => {
                noterm::println!(&mut self.output, "error: unexpected token `{}`", failed);
                return Ok(());
            }
            Err(line::lexer::Error::Unknown) => return Err(Error::Unknown),
        };

        self.execute(&argv).await?;
        Ok(())
    }

    /// Execute the command.
    pub(crate) async fn execute(&mut self, argv: &[&str]) -> Result<()> {
        noterm::println!(&mut self.output, "execute `{}`!", argv.join(" "));
        Ok(())
    }
}

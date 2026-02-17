//! Command.

use crate::console::ConsoleWriter;

/// Handler type.
pub type HandlerFn = fn(&[&str], &mut ConsoleWriter<'_>);

/// Command.
#[derive(Clone, Copy, Debug)]
pub struct Command {
    name: &'static str,
    handler: HandlerFn,
}

impl Command {
    /// Create new command.
    pub const fn new(name: &'static str, handler: HandlerFn) -> Self {
        Command { name, handler }
    }

    /// Get name.
    pub const fn name(&self) -> &'static str {
        self.name
    }

    /// Execute the command.
    pub fn execute(&self, argv: &[&str], mut writer: ConsoleWriter<'_>) {
        (self.handler)(argv, &mut writer);
    }
}

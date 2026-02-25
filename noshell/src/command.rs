//! Command.

use core::pin::Pin;
use core::task::{Context, Poll};

use crate::console::ConsoleWriter;

/// Handler type.
pub type HandlerFn = for<'a> fn(&'a [&'a str], &mut ConsoleWriter<'a>) -> UnitFuture<'a>;

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
    pub async fn execute<'a>(&self, argv: &'a [&'a str], mut writer: ConsoleWriter<'a>) {
        (self.handler)(argv, &mut writer).await;
    }
}

/// Command future.
pub struct UnitFuture<'a> {
    inner: &'a mut (dyn Future<Output = ()> + Unpin),
}

impl<'a> UnitFuture<'a> {
    /// Create a new command future.
    pub fn new(fut: &'a mut (impl Future<Output = ()> + Unpin)) -> Self {
        UnitFuture { inner: fut }
    }
}

impl Future for UnitFuture<'_> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut *self.inner).poll(cx)
    }
}

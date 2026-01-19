//! noshell, a `no_std` argument parser and a shell for constrained systems.
#![cfg_attr(not(test), no_std)]
#![allow(async_fn_in_trait)]
#![deny(missing_docs)]

#[cfg(feature = "parser")]
pub use {macros::Parser, noshell_macros as macros, noshell_parser as parser};

#[cfg(feature = "events")]
pub use noterm::events;

pub mod cmdline;

#[cfg(test)]
mod tests;

/// Defines the possible errors that may occur during usage of the crate.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
#[non_exhaustive]
pub enum Error {
    /// An error comes from the parsing of arguments.
    #[cfg(feature = "parser")]
    #[error(transparent)]
    Parser(#[from] parser::Error),

    /// Command not found.
    #[error("command not found")]
    CommandNotFound,

    /// Invalid utf8 string.
    #[error("invalid utf8 string")]
    Utf8,

    /// Unknown error, for development only.
    #[error("unknown error")]
    Unknown,
}

// /// Command trait.
// pub trait Callback {
//     /// Execute the callback.
//     fn call(&mut self, input: &str);
// }

// /// Command.
// pub struct Command<'a, OutputTy: Write>(pub(crate) TypedCommand<'a, dyn Callback + 'a, OutputTy>);

// pub(crate) struct TypedCommand<'a, CalleeTy: Callback + ?Sized, OutputTy: Write> {
//     callee: &'a CalleeTy,
// }

// /// Command.
// pub struct Command(pub(crate) Call)

// /// Callback inner function type.
// pub struct CallbackImpl<'a, CalleeTy, OutputTy>
// where
//     CalleeTy: FnMut(&str, &mut OutputTy),
//     OutputTy: Write,
// {
//     inner: CalleeTy,
//     output: &'a mut OutputTy,
// }

// impl<'a, CalleeTy, OutputTy> CallbackImpl<'a, CalleeTy, OutputTy>
// where
//     CalleeTy: FnMut(&str, &mut OutputTy),
//     OutputTy: Write,
// {
//     /// Create a new callback.
//     pub fn new(inner: CalleeTy, output: &'a mut OutputTy) -> Self {
//         CallbackImpl { inner, output }
//     }
// }

// impl<CalleeTy, OutputTy> Callback for CallbackImpl<'_, CalleeTy, OutputTy>
// where
//     CalleeTy: FnMut(&str, &mut OutputTy),
//     OutputTy: Write,
// {
//     fn execute(&mut self, input: &str) {
//         (self.inner)(input, self.output)
//     }
// }

// /// Parse top-level commands.
// pub fn lookup_in_static_entries<'a>(name: &str) -> Result<&'a mut Command<'static>, Error> {
//     let entries: &'static mut [Command<'static>] = unsafe {
//         let start = (&NOSHELL_COMMANDS_START as *const u32)
//             .cast::<Command<'static>>()
//             .cast_mut();

//         let end = (&NOSHELL_COMMANDS_END as *const u32)
//             .cast::<Command<'static>>()
//             .cast_mut();

//         let len = (end as usize) - (start as usize);

//         core::slice::from_raw_parts_mut(start, len)
//     };

//     entries
//         .iter_mut()
//         .find(|entry| name == entry.name)
//         .ok_or(Error::CommandNotFound)
// }

// unsafe extern "C" {
//     static NOSHELL_COMMANDS_START: u32;
//     static NOSHELL_COMMANDS_END: u32;
// }

// /// Character write trait.
// pub trait Write {
//     /// Error type.
//     type Error;

//     /// Write the given data to the underlying byte stream.
//     async fn write(&mut self, data: &[u8]) -> Result<usize, Self::Error>;
// }

// /// Character read trait.
// pub trait Read {
//     /// Error type;
//     type Error;

//     /// Read some data from the underlying byte stream.
//     async fn read(&self, data: &mut [u8]) -> Result<usize, Self::Error>;
// }

// /// Run the shell.
// pub async fn run<IO: Read + Write>(mut io: IO) -> Result<(), Error> {
//     let mut input = [0u8; 1024];
//     let mut output = [0u8; 1024];

//     let mut cursor = 0;

//     loop {
//         'restart: {
//             let cmdline = loop {
//                 match io.read(&mut input[cursor..]).await {
//                     Ok(len) => {
//                         if let Some(eol) = input[cursor..cursor + len]
//                             .iter()
//                             .position(|&x| x as char == '\n')
//                         {
//                             let end = cursor + eol;
//                             cursor = 0;

//                             let cmdline = str::from_utf8(&input[..end]).map_err(|_| Error::Utf8)?;

//                             break cmdline;
//                         } else {
//                             cursor += len;

//                             if cursor >= input.len() {
//                                 cursor = 0;
//                                 break 'restart;
//                             }
//                         }
//                     }

//                     Err(_) => {
//                         cursor = 0;
//                         break 'restart;
//                     }
//                 }
//             };

//             let Some(name) = cmdline.split(" ").next() else {
//                 break 'restart;
//             };

//             let Ok(cmd) = lookup_in_static_entries(name) else {
//                 break 'restart;
//             };

//             let len = cmd.run(cmdline, &mut output);
//             io.write(&output[..len]).await.ok();
//         }
//     }
// }

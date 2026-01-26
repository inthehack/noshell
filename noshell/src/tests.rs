// use speculoos::prelude::*;

// use crate as noshell;

#[cfg(not(feature = "parser"))]
compile_error!("missing `parser` feature for running tests");

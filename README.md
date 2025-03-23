# noshell

[![crates.io](https://img.shields.io/crates/d/noshell.svg)](https://crates.io/crates/noshell)
[![crates.io](https://img.shields.io/crates/v/noshell.svg)](https://crates.io/crates/noshell)
[![Documentation](https://docs.rs/noshell/badge.svg)](https://docs.rs/noshell)

`noshell`, a `no_std` argument parser and a shell for constrained systems.

# Rationale

This crate provides a working but yet minimal implementation of a argument parser. It could be used
for parsing command line arguments in applications that require a shell for instance.

This crate does not rely on crate `alloc` but `heapless` for the result of argument parsing. This
could be mitigated and improved in a near future. As such, it could be used in very constrained and
critical embedded developments.

# Example

```rust
#[derive(Debug, noshell::Parser)]
struct MyArgs {
    retries: Option<u32>,
}

fn main() -> {
    let cmdline = &["--retries", "3"];

    let args = MyArgs::parser(cmdline).unwrap();

    if let Some(retries) = args.retries {
        println!("You have {} retries left!", retries);
    }
}
```

# Status

This crate is still a work in progress and is subject to huge changes in its API.

# Roadmap

- \[ \] Add support for `noline` crate, waiting for a custom handling of terminal escape codes.
- \[x\] Add more parsers for destination value like `Option<Vec<_>>` or `Vec<_>` for instance.
- \[ \] Add automatic generation of help output.
- \[ \] Add completion thanks to escape codes (i.e. a press on Tab for instance).
- \[ \] Add support for subcommands and related global and local arguments

Please feel free to email me with suggestions or directly propose a Pull Request with some valuable
contribution. As it is the beginning of the project, I will take time to studi every contribution.

# License

This work is licensed under either

- APACHE License, version 2.0
- MIT License

at your option.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in
the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.

# Credits

I would like to give a big thank to the creator and contributors of the crate
[`clap`](https://github.com/clap-rs/clap), which I draw a lot of inspiration from.

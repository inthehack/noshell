noshell, a `no_std` argument parser and a shell for constrained systems.

# Rationale

This crate provides a working but yet minimal implementation of a argument parser. It could be used
for parsing command line arguments in applications that require a shell for instance.

This crate does not rely on crate `alloc` but `heapless` for the result of argument parsing. This
could be mitigated and improved in a near future. As such, it could be used in very constrained and
critical embedded developments.

# Status

This crate is still a work in progress and is subject to huge changes in its API.

# Roadmap

[ ] Add support for `noline` crate, waiting for a custom handling of terminal escape codes.
[ ] Add more parsers for destination value like `Option<Vec<_>>` or `Vec<_>` for instance.
[ ] Add automatic generation of help output.
[ ] Add completion thanks to escape codes (i.e. a press on Tab for instance).
[ ] Add support for subcommands and related global and local arguments

Please feel free to email me with suggestions or directly propose a Pull Request with some valuable
contribution. As it is the beginning of the project, I will take time to studi every contribution.

# License

This work is licensed under either

- APACHE License, version 2.0
- MIT License

at your option.

# Credits

I would like to give a big thank to the creator and contributors of the crate
`[clap](https://github.com/clap-rs/clap)`, which I draw a lot of inspiration from.

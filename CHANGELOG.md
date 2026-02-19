# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0](https://github.com/inthehack/noshell/compare/v0.3.0...v0.4.0) - 2026-02-19

### Added

- display error message on unexpected tokens
- refine error handling and enumeration
- make console I/O internally mutable
- clearer handling of error in console
- better segmentation of error in console
- add command handler in console
- expose console as top-level type
- expose console writer as erased type
- add console basic implementation
- make line error comparable
- capture an error when input is invalid
- [**breaking**] return error in case of words splitting fails
- [**breaking**] simplify prompt api

### Fixed

- prevent incorrect backtracking in quoted-string parsers

### Other

- fix missing version in local dependencies
- clean up dependencies
- enable integration tests only if required feature is set
- update version constraint on noterm dependency
- update noterm dependency
- [**breaking**] change cmdline module name to line
- move tests to independent file
- remove inlining of function
- use whitespace check from core lib
- update noterm dependency
- rename word iterator

## [0.3.0](https://github.com/inthehack/noshell/compare/v0.2.0...v0.3.0) - 2026-01-26

### Added

- handling of one and many argument values
- add prompt and readline handling
- add posix word lexer
- use iterators while parsing
- add shell runner (wip)
- add static command definitions
- better error handling in proc-macros

### Fixed

- handling of control events in readline ([#4](https://github.com/inthehack/noshell/pull/4))
- update to follow new parsed arg impl

### Other

- update cargo deps
- add integration tests with parser
- fix noterm dependency
- move library tests to separate module file
- rename module line by cmdline
- lint
- add test for readline
- add feature request in readme
- remove redundant pattern matching

## [0.2.0](https://github.com/inthehack/noshell/compare/noshell-v0.1.1...noshell-v0.2.0) - 2025-03-30

### Added

- add support for flag to id lookup
- *(parser)* add get_one and get_many helpers on ParsedArgs
- *(parser)* add tokens and values iterators to lexer
- *(macros)* add check for short and long flags
- *(macros)* add limit arg to noshell attribute
- *(macros)* add attribute parser
- *(macros)* add multiple option and vec variants of parsers

### Fixed

- *(macros)* use correct limit for parsed args and arg parsers
- use idiomatic parser implementations
- use parser error instead of undefined
- *(parser)* improve robustess of token distinction
- *(parser)* don't forget to dereference sometimes

### Other

- *(macros)* clean up noshell limit arg parsing
- apply code formatting
- *(macros)* remove span from attribute

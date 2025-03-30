# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

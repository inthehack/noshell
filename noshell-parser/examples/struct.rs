use core::panic;

use noshell_parser::{Lexer, ParsedArgs};

struct MyArgs {
    field1: u32,
    field2: Option<u32>,
}

fn main() {
    let argv = &["--field1", "42"];

    let tokens = Lexer::new(argv);
    let parsed = ParsedArgs::parse(tokens);

    let args = MyArgs {
        field1: parsed
            .get("field1")
            .expect("must be parsed")
            .expect("must be present"),
        field2: parsed.get("field2").expect("must be parsed"),
    };

    if 42 != args.field1 {
        panic!("invalid field1");
    }

    if args.field2.is_none() {
        panic!("invalid field2");
    }
}

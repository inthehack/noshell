use core::panic;

use noshell_parser::{ParsedArgs, Tokens};

struct MyArgs {
    field1: u32,
    field2: Option<u32>,
}

fn main() {
    let argv = &["--field1", "42"];

    let tokens = Tokens::new(argv);
    let parsed: ParsedArgs<'_, 1> = ParsedArgs::parse(tokens);

    let args = MyArgs {
        field1: parsed
            .try_get_one("field1")
            .expect("must be parsed")
            .expect("must be present"),
        field2: parsed.try_get_one("field2").expect("must be parsed"),
    };

    if 42 != args.field1 {
        panic!("invalid field1");
    }

    if args.field2.is_none() {
        panic!("invalid field2");
    }
}

use core::panic;

use noshell_parser::{
    ParsedArgs,
    lexer::Flag,
    parser::{ArgLookupTable, AtMost},
};

struct MyArgs {
    field1: u32,
    field2: Option<u32>,
}

fn main() {
    static LOOKUP: ArgLookupTable<'_> =
        ArgLookupTable::new(&[(Flag::Long("field1"), "field1", AtMost::One)]);

    let argv = &["--field1", "42"];
    let parsed: ParsedArgs<'_, 1> = ParsedArgs::parse_from(argv, &LOOKUP);

    let args = MyArgs {
        field1: parsed
            .try_get_one("field1")
            .expect("must be parsed")
            .expect("must be present")
            .expect("must have a value"),

        field2: parsed
            .try_get_one::<u32>("field2")
            .expect("must be parsed")
            .map(|v| {
                v.ok_or(noshell_parser::Error::MissingArgument)
                    .expect("must have a value")
            }),
    };

    if 42 != args.field1 {
        panic!("invalid field1");
    }

    if args.field2.is_none() {
        panic!("invalid field2");
    }
}

use futures::pin_mut;
use rstest::{Context, rstest};
use speculoos::prelude::*;

use noterm::{events, io};

use super::{Prompt, readline, unescape};

#[rstest]
#[case::empty(r#""#, "")]
#[case::quote(r#"'"#, "'")]
#[case::double_quote(r#"''"#, "''")]
#[case::single_quoted(r#"word"#, "word")]
#[case::special_dollar(r#"\$word"#, "$word")]
#[case::special_backslash(r#"\\word"#, "\\word")]
#[case::special_double_quote(r#"\"word"#, "\"word")]
#[case::hex(r#"\x33word"#, "\\x33word")]
#[case::multiline("word0 \\\nword1", "word0 word1")]
fn it_should_unescape_string(#[case] input: &str, #[case] expected: &str) {
    assert_that!(unescape::<256>(input).as_str()).is_equal_to(expected);
}

struct StringBuf {
    inner: String,
    cursor: usize,
}

impl StringBuf {
    fn new(inner: String) -> Self {
        StringBuf { inner, cursor: 0 }
    }
}

impl io::Read for StringBuf {
    async fn read(&mut self, data: &mut [u8]) -> io::Result<usize> {
        let n = (self.inner.len() - self.cursor).min(data.len());

        let (input, _) = self.inner.as_bytes().split_at(self.cursor + n);
        let (output, _) = data.split_at_mut(n);

        output.copy_from_slice(&input[self.cursor..]);
        self.cursor += n;

        Ok(n)
    }
}

impl io::blocking::Write for StringBuf {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.inner.push_str(str::from_utf8(data).unwrap());
        Ok(data.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[tokio::test]
async fn it_should_print_prompt() {
    let cmdline = String::from("\x0d");
    let mut input = StringBuf::new(cmdline.clone());
    let mut output = StringBuf::new(String::default());

    let stream = events::stream(&mut input);
    let prompt = Prompt::new(["prompt>"].iter());

    pin_mut!(stream);

    let line: Result<heapless::String<256>, _> = readline(&prompt, stream, &mut output).await;
    assert_that!(line).is_ok();

    let result = output.inner.as_str();
    insta::with_settings!({
        description => format!("cmdline: {}", cmdline),
        omit_expression => true,
    }, {
        insta::assert_snapshot!(result);
    });
}

#[rstest]
#[case::empty("\x0d")]
#[case::single("word\x0d")]
#[case::multiple("word0 word1\x0d")]
#[case::newline("word0 \\\nword1\x0d")]
#[tokio::test]
async fn it_should_read_line(#[context] ctx: Context, #[case] input: &str) {
    let cmdline = String::from(input);
    let mut input = StringBuf::new(cmdline.clone());
    let mut output = StringBuf::new(String::default());

    let stream = events::stream(&mut input);
    let prompt = Prompt::new(["prompt>"].iter());

    pin_mut!(stream);

    let line: Result<heapless::String<256>, _> = readline(&prompt, stream, &mut output).await;
    let result = assert_that!(line).is_ok().subject;

    insta::with_settings!({
        description => format!("cmdline: {}", cmdline),
        omit_expression => true,
        snapshot_suffix => ctx.description.unwrap_or_default(),
    }, {
        insta::assert_snapshot!(result);
    });
}

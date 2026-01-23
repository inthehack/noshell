//! Values.

/// Iterator over argument values.
#[derive(Clone, Debug, PartialEq)]
pub struct Values<'a> {
    slice: &'a [&'a str],
}

impl<'a> Values<'a> {
    /// Create a new value iterator.
    pub fn new(slice: &'a [&'a str]) -> Self {
        Values { slice }
    }

    /// Get an iterator.
    pub fn iter(&self) -> impl Iterator<Item = &'a str> {
        self.slice.iter().copied()
    }

    /// Create an empty value iterator.
    pub fn empty() -> Self {
        Self::new(&[])
    }
}

/// The number of expected values on a given argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AtMost {
    /// No value expected.
    Zero,

    /// Exactly one value expected.
    One,

    /// One or more values expected.
    Many,
}

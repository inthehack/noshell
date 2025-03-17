//! Parser utilities.

use heapless::Vec;

use crate::Error;

/// Check if the argument `Option` is `None` and return an error `Error::InvalidArgument` if true
#[inline(always)]
pub fn check_arg_is_missing<T>(v: Option<T>) -> Result<Option<T>, Error> {
    v.map(Some).ok_or(Error::MissingArgument)
}

/// Check if the value `Option` is `None` and return an error `Error::InvalidArgument` if true
#[inline(always)]
pub fn check_value_is_missing<T>(v: Option<T>) -> Result<Option<T>, Error> {
    v.map(Some).ok_or(Error::InvalidArgument)
}

/// Check if the `Vec` is empty and return an error `Error::InvalidArgument` if true.
#[inline(always)]
pub fn check_vec_is_missing<T, const SIZE: usize>(v: Vec<T, SIZE>) -> Result<Vec<T, SIZE>, Error> {
    if v.is_empty() {
        return Err(Error::InvalidArgument);
    }

    Ok(v)
}

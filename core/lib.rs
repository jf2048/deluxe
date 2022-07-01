mod parse_attributes;
pub mod parse_helpers;
mod parse_meta;
mod util;

pub use parse_attributes::*;
pub use parse_meta::*;
pub use util::*;

pub mod from_str {
    use crate::{Error, Result};
    use std::str::FromStr;
    use syn::parse::ParseStream;

    #[inline]
    pub fn parse_meta_item<T: FromStr>(input: ParseStream) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        let s = input.parse::<syn::LitStr>()?;
        Ok(T::from_str(&s.value()).map_err(|e| Error::new_spanned(s, e.to_string()))?)
    }
    #[inline]
    pub fn parse_named_meta_item<T: FromStr>(input: ParseStream) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        let s = crate::parse_helpers::parse_named_meta_item::<syn::LitStr>(input)?;
        Ok(T::from_str(&s.value()).map_err(|e| Error::new_spanned(s, e.to_string()))?)
    }
}

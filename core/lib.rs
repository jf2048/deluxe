mod parse_attributes;
pub mod parse_helpers;
mod parse_meta;
mod util;
pub mod validations;

pub use parse_attributes::*;
pub use parse_meta::*;
pub use util::*;

pub mod from_str {
    use crate::{Error, ParseMode, Result};
    use std::str::FromStr;
    use syn::parse::ParseStream;

    #[inline]
    pub fn parse_meta_item<T: FromStr>(input: ParseStream, _mode: ParseMode) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        let s = input.parse::<syn::LitStr>()?;
        T::from_str(&s.value()).map_err(|e| Error::new_spanned(s, e.to_string()))
    }
    #[inline]
    pub fn parse_meta_item_inline<T: FromStr>(input: ParseStream, _mode: ParseMode) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        parse_meta_item(input, _mode)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: Default>(_span: proc_macro2::Span) -> Result<T> {
        Ok(Default::default())
    }
}

pub mod mod_path {
    use crate::{ParseMode, Result};
    use syn::parse::ParseStream;

    #[inline]
    pub fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<syn::Path> {
        Ok(input.call(syn::Path::parse_mod_style)?)
    }
    #[inline]
    pub fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<syn::Path> {
        parse_meta_item(input, _mode)
    }
    #[inline]
    pub fn parse_meta_item_flag(span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::flag_disallowed_error(span))
    }
}

pub mod mod_path_vec {
    use crate::{ParseMode, Result};
    use syn::parse::ParseStream;

    #[repr(transparent)]
    struct ModPath(syn::Path);

    impl crate::ParseMetaItem for ModPath {
        #[inline]
        fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
            Ok(Self(super::mod_path::parse_meta_item(input, _mode)?))
        }
    }

    #[inline]
    pub fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Vec<syn::Path>> {
        Ok(
            <Vec<ModPath> as crate::ParseMetaItem>::parse_meta_item(input, _mode)?
                .into_iter()
                .map(|p| p.0)
                .collect(),
        )
    }
    #[inline]
    pub fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Vec<syn::Path>> {
        Ok(
            <Vec<ModPath> as crate::ParseMetaItem>::parse_meta_item_inline(input, _mode)?
                .into_iter()
                .map(|p| p.0)
                .collect(),
        )
    }
    #[inline]
    pub fn parse_meta_item_flag(_span: proc_macro2::Span) -> Result<Vec<syn::Path>> {
        Ok(Vec::new())
    }
}

//! Custom parsing helpers for `#[deluxe(with = "...")]`.

/// Helpers for parsing any type that implements [`std::str::FromStr`].
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = "deluxe::with::from_str")]`
pub mod from_str {
    #![allow(missing_docs)]
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
    pub fn parse_meta_item_inline<T: FromStr>(inputs: &[ParseStream], mode: ParseMode) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        crate::parse_helpers::parse_first(inputs, mode, |input| parse_meta_item(input, mode))
    }
    #[inline]
    pub fn parse_meta_item_flag<T: Default>(_span: proc_macro2::Span) -> Result<T> {
        Ok(Default::default())
    }
}

/// Helpers for parsing a module path using [`syn::Path::parse_mod_style`].
///
/// The field should be a `syn::Path`. Can be used on a field by specifying the module, like
/// `#[deluxe(with = "deluxe::with::mod_path")]`
pub mod mod_path {
    #![allow(missing_docs)]
    use crate::{ParseMode, Result};
    use syn::parse::ParseStream;

    #[inline]
    pub fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<syn::Path> {
        input.call(syn::Path::parse_mod_style)
    }
    #[inline]
    pub fn parse_meta_item_inline(inputs: &[ParseStream], mode: ParseMode) -> Result<syn::Path> {
        crate::parse_helpers::parse_first(inputs, mode, |input| parse_meta_item(input, mode))
    }
    #[inline]
    pub fn parse_meta_item_flag(span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::flag_disallowed_error(span))
    }
}

/// Helpers for parsing a `Vec` of module paths using [`syn::Path::parse_mod_style`].
///
/// The field should be a `Vec<syn::Path>`. Can be used on a field by specifying the module, like
/// `#[deluxe(with = "deluxe::with::mod_path_vec")]`
pub mod mod_path_vec {
    #![allow(missing_docs)]
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
    pub fn parse_meta_item_inline(
        inputs: &[ParseStream],
        _mode: ParseMode,
    ) -> Result<Vec<syn::Path>> {
        Ok(
            <Vec<ModPath> as crate::ParseMetaItem>::parse_meta_item_inline(inputs, _mode)?
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

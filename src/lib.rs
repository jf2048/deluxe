#[doc(hidden)]
pub mod ____private {
    pub use deluxe_core::parse_helpers;
    pub use once_cell::sync::OnceCell as SyncOnceCell;
    pub use proc_macro2::Span;
    pub use std::{
        borrow::Cow,
        clone::Clone,
        default::Default,
        format_args,
        option::Option,
        primitive::{bool, str, usize},
        string::ToString,
        unreachable,
        vec::Vec,
    };
    pub use syn::{parse::ParseStream, Error, Ident, Path};
}

pub use deluxe_core::{
    with, Error, ExtractAttributes, HasAttributes, ParseAttributes, ParseMetaItem, Result,
};
#[doc(hidden)]
pub use deluxe_core::{
    Errors, ParseMetaAppend, ParseMetaFlatNamed, ParseMetaFlatUnnamed, ParseMetaRest, ParseMode,
};
pub use deluxe_macros::*;

#[inline]
pub fn parse_attributes<'t, T, R>(obj: &'t T) -> Result<R>
where
    T: HasAttributes,
    R: ParseAttributes<'t, T>,
{
    R::parse_attributes(obj)
}

#[inline]
pub fn extract_attributes<T, R>(obj: &mut T) -> Result<R>
where
    T: HasAttributes,
    R: ExtractAttributes<T>,
{
    R::extract_attributes(obj)
}

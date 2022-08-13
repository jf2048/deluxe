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
    from_str, Error, Errors, ExtractAttributes, HasAttributes, ParseAttributes, ParseMetaFlatNamed,
    ParseMetaFlatPrefixed, ParseMetaFlatUnnamed, ParseMetaItem, ParseMode, Result,
};
pub use deluxe_macros::*;

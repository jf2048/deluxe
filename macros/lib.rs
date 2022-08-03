#[macro_use]
mod util;
mod field;
mod parse_attributes;
mod parse_meta_item;
use util::*;

use deluxe_core::Errors;
use proc_macro::TokenStream;

#[proc_macro_derive(ExtractAttributes, attributes(deluxe))]
pub fn derive_extract_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
        .map(|input| parse_attributes::impl_parse_attributes(input, &errors, true))
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

#[proc_macro_derive(ParseAttributes, attributes(deluxe))]
pub fn derive_parse_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
        .map(|input| parse_attributes::impl_parse_attributes(input, &errors, false))
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

#[proc_macro_derive(ParseMetaItem, attributes(deluxe))]
pub fn derive_parse_meta_item(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
        .map(|input| parse_meta_item::impl_parse_meta_item(input, &errors))
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

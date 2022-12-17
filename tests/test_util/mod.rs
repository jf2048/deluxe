#![allow(dead_code)]

use ::proc_macro2::TokenStream;
use ::std::prelude::v1::*;

pub trait SynResultExt {
    fn unwrap_err_string(self) -> String;
}

impl<T: ::std::fmt::Debug> SynResultExt for ::syn::Result<T> {
    fn unwrap_err_string(self) -> String {
        self.unwrap_err()
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[inline]
pub fn parse_meta<T: ::deluxe::ParseMetaItem>(s: TokenStream) -> ::deluxe::Result<T> {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <T as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };
    parser.parse2(s)
}

#[inline]
pub fn parse_flag<T: ::deluxe::ParseMetaItem>() -> ::deluxe::Result<T> {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <T as ::deluxe::ParseMetaItem>::parse_meta_item_flag(stream.span())
    };
    parser.parse2(Default::default())
}

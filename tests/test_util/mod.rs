use ::std::prelude::v1::*;

pub trait SynErrorExt {
    fn to_multi_string(&self) -> String;
}

impl SynErrorExt for ::syn::Error {
    fn to_multi_string(&self) -> String {
        self.into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[inline]
pub fn parse_str<T: ::deluxe::ParseMetaItem>(s: &str) -> ::deluxe::Result<T> {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <T as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };
    parser.parse_str(s)
}

#[inline]
pub fn parse_flag<T: ::deluxe::ParseMetaItem>() -> ::deluxe::Result<T> {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <T as ::deluxe::ParseMetaItem>::parse_meta_item_flag(stream.span())
    };
    parser.parse2(Default::default())
}

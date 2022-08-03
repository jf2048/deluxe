use std::borrow::Cow;

use crate::{Error, Errors, ParseMetaItem, ParseMode, Result};
use proc_macro2::{Span, TokenStream, TokenTree};
pub use syn::{
    parse::Nothing,
    token::{Brace, Bracket, Paren},
};
use syn::{
    parse::{ParseBuffer, ParseStream},
    spanned::Spanned,
    Token,
};

mod sealed {
    pub trait Sealed {}
    impl Sealed for syn::token::Paren {}
    impl Sealed for syn::token::Brace {}
    impl Sealed for syn::token::Bracket {}
}

/// Helpers for parsing tokens inside a delimiter.
///
/// This trait is only implemeted for the `syn` delimiter token types.
pub trait ParseDelimited: sealed::Sealed {
    /// Parses a pair of delimiter tokens, and returns a [`syn::ParseBuffer`] for the tokens inside
    /// the delimiter.
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer>;
    /// Parse a [`ParseMetaItem`] surrounded by a delimiter. The inner group is allowed to contain
    /// a trailing comma.
    #[inline]
    fn parse_delimited_meta_item<T: ParseMetaItem>(
        input: ParseStream,
        mode: ParseMode,
    ) -> Result<T> {
        let content = Self::parse_delimited(input)?;
        let result = T::parse_meta_item_inline(&content, mode)?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(result)
    }
}

impl ParseDelimited for Paren {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::parenthesized!(content in input);
        Ok(content)
    }
}

impl ParseDelimited for Brace {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::braced!(content in input);
        Ok(content)
    }
}

impl ParseDelimited for Bracket {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::bracketed!(content in input);
        Ok(content)
    }
}

#[inline]
pub fn peek_eof_or_trailing_comma(input: ParseStream) -> bool {
    parse_eof_or_trailing_comma(&input.fork()).is_ok()
}

#[inline]
pub fn parse_eof_or_trailing_comma(input: ParseStream) -> Result<()> {
    if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
    }
    input.parse::<Nothing>()?;
    Ok(())
}

#[inline]
pub fn parse_empty_meta_item<T: ParseMetaItem>(
    span: proc_macro2::Span,
    mode: ParseMode,
) -> Result<T> {
    let stream = quote::quote_spanned! { span => };
    syn::parse::Parser::parse2(
        |stream: ParseStream<'_>| T::parse_meta_item_inline(stream, mode),
        stream,
    )
}

#[inline]
pub fn parse_named_meta_item<T: ParseMetaItem>(input: ParseStream) -> Result<T> {
    if input.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        T::parse_meta_item(input, ParseMode::Named)
    } else if input.peek(Paren) {
        Paren::parse_delimited_meta_item(input, ParseMode::Named)
    } else {
        T::parse_meta_item_flag(input.span())
    }
}

#[inline]
pub fn parse_named_meta_item_with<T, I, II, IF>(
    input: ParseStream,
    parse: I,
    parse_inline: II,
    parse_flag: IF,
) -> Result<T>
where
    I: FnOnce(ParseStream, ParseMode) -> Result<T>,
    II: FnOnce(ParseStream, ParseMode) -> Result<T>,
    IF: FnOnce(proc_macro2::Span) -> Result<T>,
{
    if input.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        parse(input, ParseMode::Named)
    } else if input.peek(Paren) {
        let content = Paren::parse_delimited(input)?;
        let result = parse_inline(&content, ParseMode::Named)?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(result)
    } else {
        parse_flag(input.span())
    }
}

#[macro_export]
macro_rules! parse_named_meta_item_with {
    ($input:ident, $p:ident $(::$ps:ident)*) => {
        $crate::parse_helpers::parse_named_meta_item_with(
            $input,
            $p $(::$ps)* :: parse_meta_item,
            $p $(::$ps)* :: parse_meta_item_inline,
            $p $(::$ps)* :: parse_meta_item_flag,
        )
    };
}

#[inline]
pub fn parse_tuple_struct<F: FnMut(ParseStream, &[ParseStream], usize) -> Result<()>>(
    mut inputs: &[ParseStream],
    len: usize,
    mut func: F,
) -> Result<usize> {
    let mut counter = 0;
    while let Some(input) = inputs.first().cloned() {
        loop {
            if input.is_empty() {
                break;
            }
            if counter >= len {
                return Ok(counter);
            }
            func(input, inputs, counter)?;
            counter += 1;
            if !input.is_empty() && counter < len {
                input.parse::<Token![,]>()?;
            }
        }
        inputs = &inputs[1..];
    }
    Ok(counter)
}

#[inline]
pub fn parse_struct<F: FnMut(ParseStream, &str, Span) -> Result<()>>(
    inputs: &[ParseStream],
    mut func: F,
) -> Result<()> {
    for input in inputs {
        loop {
            if input.is_empty() {
                break;
            }
            let path = input.call(syn::Path::parse_mod_style)?;
            func(input, path_to_string(&path).as_str(), path.span())?;
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
    }
    Ok(())
}

#[inline]
pub fn skip_named_meta_item(input: ParseStream) {
    input
        .step(|cursor| {
            let mut cur = *cursor;
            while let Some((tt, next)) = cur.token_tree() {
                cur = next;
                if let TokenTree::Punct(punct) = tt {
                    if punct.as_char() == ',' {
                        break;
                    }
                }
            }
            Ok(((), cur))
        })
        .unwrap();
}

#[inline]
pub fn parse_tokens<T, F: FnOnce(ParseStream) -> Result<T>>(
    input: TokenStream,
    func: F,
) -> Result<T> {
    let errors = Errors::new();
    let mut ret = None;
    syn::parse::Parser::parse2(
        |stream: ParseStream<'_>| {
            ret = match func(stream) {
                Ok(ret) => Some(ret),
                Err(err) => {
                    errors.push_syn(err);
                    None
                }
            };
            if let Err(err) = parse_eof_or_trailing_comma(stream) {
                errors.push_syn(err);
            }
            Ok(())
        },
        input,
    )?;
    errors.check()?;
    Ok(ret.unwrap())
}

pub fn parse_struct_attr_tokens<T, I, F>(
    inputs: I,
    func: F,
) -> Result<()>
where
    T: quote::ToTokens,
    I: IntoIterator<Item = T>,
    F: FnMut(&[ParseStream]) -> Result<()>,
{
    fn parse_next<T, I, F>(
        mut iter: I,
        buffers: Vec<ParseBuffer>,
        mut func: F,
    ) -> Result<()>
    where
        T: quote::ToTokens,
        I: Iterator<Item = T>,
        F: FnMut(&[ParseStream]) -> Result<()>,
    {
        if let Some(tokens) = iter.next() {
            let tokens = tokens.into_token_stream();
            parse_tokens(tokens, |stream| {
                let content = Paren::parse_delimited(stream)?;
                let mut buffers: Vec<ParseBuffer> = buffers.into();
                buffers.push(content);
                parse_next(iter, buffers, func)
            })
        } else {
            let streams = buffers.iter().map(|b| b).collect::<Vec<_>>();
            func(&streams)?;
            for buffer in buffers {
                parse_eof_or_trailing_comma(&buffer)?;
            }
            Ok(())
        }
    }
    parse_next(inputs.into_iter(), Vec::new(), func)
}

pub fn path_to_string(path: &syn::Path) -> String {
    let mut s = String::new();
    for seg in &path.segments {
        if !s.is_empty() {
            s.push_str("::");
        }
        s.push_str(&seg.ident.to_string());
    }
    s
}

#[inline]
pub fn join_path<'path>(prefix: &str, path: &'path str) -> Cow<'path, str> {
    if prefix.is_empty() {
        Cow::Borrowed(path)
    } else {
        format!("{}::{}", prefix, path).into()
    }
}

#[inline]
pub fn join_paths(prefix: &str, paths: &[&'static str]) -> Vec<&'static str> {
    paths
        .iter()
        .map(|p| match join_path(prefix, p) {
            Cow::Borrowed(p) => p,
            Cow::Owned(p) => &*Box::leak(p.into_boxed_str()),
        })
        .collect()
}

#[inline]
pub fn inputs_span<'a>(inputs: impl IntoIterator<Item = &'a ParseStream<'a>>) -> Span {
    let mut iter = inputs.into_iter();
    let first = iter.next();
    if let Some(input) = first {
        let mut span = input.span();
        while let Some(next) = iter.next() {
            if let Some(joined) = span.join(next.span()) {
                span = joined;
            }
        }
        span
    } else {
        Span::call_site()
    }
}

#[inline]
pub fn flag_disallowed_error(span: Span) -> Error {
    syn::Error::new(span, "unexpected flag, expected `=` or parentheses")
}

pub fn unknown_error(path: &str, span: Span, fields: &[&str]) -> Error {
    let mut closest = None;
    for field in fields {
        let sim = strsim::jaro_winkler(path, field);
        if sim > 0.8 && closest.as_ref().map(|(s, _)| sim > *s).unwrap_or(true) {
            closest = Some((sim, *field));
        }
    }
    if let Some((_, closest)) = closest {
        Error::new(
            span,
            format!("unknown field `{}`, did you mean `{}`?", path, closest),
        )
    } else {
        Error::new(span, format!("unknown field `{}`", path))
    }
}

#[inline]
pub fn check_unknown_attribute(path: &str, span: Span, fields: Option<&[&str]>, errors: &Errors) {
    if let Some(fields) = fields {
        if !fields.contains(&path) {
            errors.push_syn(unknown_error(path, span, fields));
        }
    }
}

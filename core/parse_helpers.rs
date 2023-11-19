//! Utility functions for parse trait implementations.

pub use crate::small_string::SmallString;
use crate::{Error, Errors, ParseMetaItem, ParseMode, Result, ToKeyString};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::ToTokens;
use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
};
pub use syn::{
    parse::Nothing,
    token::{Brace, Bracket, Paren},
};
use syn::{
    parse::{ParseBuffer, ParseStream},
    spanned::Spanned,
    Token,
};

/// Temporary container for storing parsing state for a single field.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum FieldStatus<T> {
    /// Field is not present or not parsed yet.
    None,
    /// Field was present, but had an error during parsing.
    ParseError,
    /// Field was found and successfully parsed.
    Some(T),
}

impl<T> FieldStatus<T> {
    /// Converts from `&FieldStatus<T>` to `FieldStatus<&T>`.
    #[inline]
    pub const fn as_ref(&self) -> FieldStatus<&T> {
        match *self {
            Self::Some(ref x) => FieldStatus::Some(x),
            Self::ParseError => FieldStatus::ParseError,
            Self::None => FieldStatus::None,
        }
    }
    /// Returns `b` if the status is `Some`.
    #[inline]
    pub fn and<U>(self, b: FieldStatus<U>) -> FieldStatus<U> {
        match self {
            Self::Some(_) => b,
            Self::ParseError => FieldStatus::ParseError,
            Self::None => FieldStatus::None,
        }
    }

    /// If the status is `Some`, calls `f` with the wrapped value and returns the result.
    #[inline]
    pub fn and_then<U, F: FnOnce(T) -> FieldStatus<U>>(self, f: F) -> FieldStatus<U> {
        match self {
            Self::Some(x) => f(x),
            Self::ParseError => FieldStatus::ParseError,
            Self::None => FieldStatus::None,
        }
    }
    /// Maps a `FieldStatus<T>` to `FieldStatus<U>` by applying a function to a contained value.
    #[inline]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> FieldStatus<U> {
        match self {
            Self::Some(x) => FieldStatus::Some(f(x)),
            Self::ParseError => FieldStatus::ParseError,
            Self::None => FieldStatus::None,
        }
    }
    /// Returns `true` if the field is a [`FieldStatus::None`] value.
    #[inline]
    pub const fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
    /// Returns `true` if the field is a [`FieldStatus::ParseError`] value.
    #[inline]
    pub const fn is_parse_error(&self) -> bool {
        matches!(self, Self::ParseError)
    }
    /// Returns `true` if the field is a [`FieldStatus::Some`] value.
    #[inline]
    pub const fn is_some(&self) -> bool {
        matches!(self, Self::Some(_))
    }
    /// Returns the status if it contains a value, or if it is `None` then returns `b`.
    #[inline]
    pub fn or(self, b: Self) -> Self {
        match self {
            Self::Some(x) => Self::Some(x),
            Self::ParseError => Self::ParseError,
            Self::None => b,
        }
    }
    /// Returns the status if it contains a value, or if it is `None` then calls `f` and returns
    /// the result.
    #[inline]
    pub fn or_else<F: FnOnce() -> Self>(self, f: F) -> Self {
        match self {
            Self::Some(x) => Self::Some(x),
            Self::ParseError => Self::ParseError,
            Self::None => f(),
        }
    }
    /// Returns the contained [`FieldStatus::Some`] value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the self value equals [`FieldStatus::None`] or [`FieldStatus::ParseError`].
    #[inline]
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            Self::Some(val) => val,
            _ => panic!("called `FieldStatus::unwrap()` on a `None` value"),
        }
    }
    /// Returns the contained [`FieldStatus::Some`] value or a provided default.
    #[inline]
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Self::Some(x) => x,
            _ => default,
        }
    }
    /// Returns the contained [`FieldStatus::Some`] value or computes it from a closure.
    #[inline]
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, f: F) -> T {
        match self {
            Self::Some(x) => x,
            _ => f(),
        }
    }
    /// Converts from `FieldStatus<impl Into<FieldStatus<T>>>` to `FieldStatus<T>`.
    #[inline]
    pub fn flatten<U>(self) -> FieldStatus<U>
    where
        T: Into<FieldStatus<U>>,
    {
        match self {
            Self::Some(inner) => inner.into(),
            Self::ParseError => FieldStatus::ParseError,
            Self::None => FieldStatus::None,
        }
    }
    /// Parses a named item into this status.
    #[inline]
    pub fn parse_named_item(&mut self, name: &str, input: ParseStream, span: Span, errors: &Errors)
    where
        T: ParseMetaItem,
    {
        self.parse_named_item_with(name, input, span, errors, T::parse_meta_item_named)
    }
    /// Parses a named item into this status, using a custom parsing function.
    #[inline]
    pub fn parse_named_item_with<F>(
        &mut self,
        name: &str,
        input: ParseStream,
        span: Span,
        errors: &Errors,
        func: F,
    ) where
        F: FnOnce(ParseStream, &str, Span) -> Result<T>,
    {
        if !self.is_none() {
            errors.push(span, format_args!("duplicate attribute for `{name}`"));
        }
        match errors.push_result(func(input, name, span)) {
            Some(v) => {
                if self.is_none() {
                    *self = Self::Some(v)
                }
            }
            None => {
                if self.is_none() {
                    *self = Self::ParseError;
                }
                skip_meta_item(input);
            }
        }
    }
    /// Parses an unnamed item into this status.
    #[inline]
    pub fn parse_unnamed_item(&mut self, input: ParseStream, errors: &Errors)
    where
        T: ParseMetaItem,
    {
        self.parse_unnamed_item_with(input, errors, T::parse_meta_item)
    }
    /// Parses an unnamed item into this status, using a custom parsing function.
    #[inline]
    pub fn parse_unnamed_item_with<F>(&mut self, input: ParseStream, errors: &Errors, func: F)
    where
        F: FnOnce(ParseStream, ParseMode) -> Result<T>,
    {
        match errors.push_result(func(input, ParseMode::Unnamed)) {
            Some(v) => *self = Self::Some(v),
            None => {
                *self = Self::ParseError;
                skip_meta_item(input);
            }
        }
    }
    /// Converts the status to an [`Option`].
    #[inline]
    pub fn into_option(self) -> Option<T> {
        match self {
            Self::Some(v) => Some(v),
            _ => None,
        }
    }
}

impl<T> Default for FieldStatus<T> {
    #[inline]
    fn default() -> Self {
        Self::None
    }
}

impl<T> From<FieldStatus<T>> for Option<T> {
    #[inline]
    fn from(value: FieldStatus<T>) -> Self {
        value.into_option()
    }
}

impl<T> From<Option<T>> for FieldStatus<T> {
    #[inline]
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Self::Some(v),
            None => Self::None,
        }
    }
}

mod sealed {
    pub trait Sealed {}
    impl Sealed for syn::token::Paren {}
    impl Sealed for syn::token::Brace {}
    impl Sealed for syn::token::Bracket {}
}

/// Helpers for parsing tokens inside a delimiter.
///
/// This trait is only implemeted for the [`syn`] delimiter token types.
pub trait ParseDelimited: sealed::Sealed {
    /// Parses a pair of delimiter tokens, and returns a [`ParseBuffer`](syn::parse::ParseBuffer)
    /// for the tokens inside the delimiter.
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer>;
    /// Parse a stream surrounded by a delimiter. The inner stream is allowed to contain
    /// a trailing comma.
    #[inline]
    fn parse_delimited_with<T, F: FnOnce(ParseStream) -> Result<T>>(
        input: ParseStream,
        func: F,
    ) -> Result<T> {
        let content = Self::parse_delimited(input)?;
        let result = func(&content)?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(result)
    }
    /// Parse a [`ParseMetaItem`] surrounded by a delimiter. The inner stream is allowed to contain
    /// a trailing comma.
    #[inline]
    fn parse_delimited_meta_item<T: ParseMetaItem>(
        input: ParseStream,
        mode: ParseMode,
    ) -> Result<T> {
        Self::parse_delimited_with(input, |inner| T::parse_meta_item_inline(&[inner], mode))
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

/// Checks if a stream is empty or contains only a comma.
///
/// Returns `false` if any other tokens are present besides a single comma.
#[inline]
pub fn peek_eof_or_trailing_comma(input: ParseStream) -> bool {
    parse_eof_or_trailing_comma(&input.fork()).is_ok()
}

/// Parses a stream for end-of-stream, or a single comma.
///
/// Returns a parse error if any other tokens are present besides a single comma.
#[inline]
pub fn parse_eof_or_trailing_comma(input: ParseStream) -> Result<()> {
    if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
    }
    if let Some((tree, _)) = input.cursor().token_tree() {
        Err(syn::Error::new(
            tree.span(),
            format_args!("unexpected token `{tree}`"),
        ))
    } else {
        Ok(())
    }
}

/// Runs a parsing function from an empty token stream.
///
/// Creates an empty token stream and then calls `func` on it. Any errors returned from `func` will
/// be changed to make them spanned with `span`.
#[inline]
pub fn parse_empty<T, F>(span: proc_macro2::Span, func: F) -> Result<T>
where
    F: FnOnce(ParseStream) -> Result<T>,
{
    syn::parse::Parser::parse2(func, TokenStream::new()).map_err(|e| {
        let mut iter = e.into_iter();
        let mut err = Error::new(span, iter.next().unwrap());
        for e in iter {
            err.combine(e);
        }
        err
    })
}

/// Parses a [`ParseMetaItem`](crate::ParseMetaItem) inline from an empty token stream.
///
/// Creates an empty token stream and then calls
/// [`ParseMetaItem::parse_meta_item_inline`](crate::ParseMetaItem::parse_meta_item_inline) on it,
/// passing through `mode`. The empty token stream will be created with `span`, so that any
/// resulting error messages can be spanned to it.
#[inline]
pub fn parse_empty_meta_item<T: ParseMetaItem>(
    span: proc_macro2::Span,
    mode: ParseMode,
) -> Result<T> {
    parse_empty(span, |input| T::parse_meta_item_inline(&[input], mode))
}

/// Runs a parsing function on the first stream in a stream list, then ensures the rest of the
/// streams are empty.
pub fn parse_first<'s, T, F, S>(inputs: &[S], mode: ParseMode, func: F) -> Result<T>
where
    F: FnOnce(ParseStream, ParseMode) -> Result<T>,
    S: Borrow<ParseBuffer<'s>>,
{
    let mut iter = inputs.iter();
    if let Some(input) = iter.next() {
        let input = input.borrow();
        let ret = func(input, mode)?;
        parse_eof_or_trailing_comma(input)?;
        for next in iter {
            next.borrow().parse::<Nothing>()?;
        }
        Ok(ret)
    } else {
        let span = match mode {
            ParseMode::Named(span) => span,
            _ => Span::call_site(),
        };
        parse_empty(span, |input| func(input, mode))
    }
}

/// Parses a [`ParseMetaItem`](crate::ParseMetaItem) following a name.
///
/// To be used by any parse implementations that expect named attributes.
///
/// If the first token is `=`, then calls
/// [`ParseMetaItem::parse_meta_item`](crate::ParseMetaItem::parse_meta_item). If the first token
/// is a parenthesized group, calls
/// [`ParseMetaItem::parse_meta_item_inline`](crate::ParseMetaItem::parse_meta_item_inline) on the
/// contents.
///
/// If neither a `=` or `(` is parsed, then no additional tokens are consumed, and
/// [`ParseMetaItem::parse_meta_item_flag`](crate::ParseMetaItem::parse_meta_item_flag) is called
/// with the current stream span.
#[inline]
pub fn parse_named_meta_item<T: ParseMetaItem>(input: ParseStream, name_span: Span) -> Result<T> {
    if input.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        T::parse_meta_item(input, ParseMode::Named(name_span))
    } else if input.peek(Paren) {
        Paren::parse_delimited_meta_item(input, ParseMode::Named(name_span))
    } else {
        T::parse_meta_item_flag(name_span)
    }
}

/// The type of a named meta field.
///
/// See [`try_parse_named_meta_item`].
#[derive(Debug)]
pub enum NamedParse<'p> {
    /// The field was specified with an equals, .e.g `my_arg = 123`.
    Equals,
    /// The field was specified with surrounding parenthesis, .e.g `my_arg(123)`.
    Paren(ParseBuffer<'p>),
    /// The field was specified as a flag without a value, .e.g `my_arg`.
    Flag,
}

/// Attempts to begin parsing a meta item following a name, returning a value describing what type
/// of named meta item this is.
///
/// Can be used as an alternative to [`parse_named_meta_item_with`] for custom parsing logic.
#[inline]
pub fn try_parse_named_meta_item(input: ParseStream) -> Result<NamedParse> {
    if input.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        Ok(NamedParse::Equals)
    } else if input.peek(Paren) {
        let content = Paren::parse_delimited(input)?;
        Ok(NamedParse::Paren(content))
    } else {
        Ok(NamedParse::Flag)
    }
}

/// Parses a [`ParseMetaItem`](crate::ParseMetaItem) following a name, using custom parse
/// functions.
///
/// Equivalent to [`parse_named_meta_item`], but the functions are passed directly instead of using
/// the implementations from a trait. Used for parsing enum variants. For parsing with a custom
/// module path, use [`parse_named_meta_item_with!`](crate::parse_named_meta_item_with!).
#[inline]
pub fn parse_named_meta_item_with<T, I, II, IF>(
    input: ParseStream,
    name_span: Span,
    parse: I,
    parse_inline: II,
    parse_flag: IF,
) -> Result<T>
where
    I: FnOnce(ParseStream, ParseMode) -> Result<T>,
    II: FnOnce(&[ParseStream], ParseMode) -> Result<T>,
    IF: FnOnce(proc_macro2::Span) -> Result<T>,
{
    if input.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        parse(input, ParseMode::Named(name_span))
    } else if input.peek(Paren) {
        let content = Paren::parse_delimited(input)?;
        let result = parse_inline(&[&content], ParseMode::Named(name_span))?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(result)
    } else {
        parse_flag(name_span)
    }
}

/// Parses a [`ParseMetaItem`](crate::ParseMetaItem) following a name, using a custom parse
/// module.
///
/// Equivalent to [`parse_named_meta_item_with`](fn@parse_named_meta_item_with), but the functions
/// are taken from a module path. Can be used to implement additional parsing strategies for types
/// that already implement [`ParseMetaItem`](crate::ParseMetaItem), or for generic types.
/// Some common parsers are available in the [`with`](crate::with) module.
///
/// The first argument is a [`syn::parse::ParseStream`], the second argument is the
/// [`Span`](proc_macro2::Span) of the name tokens, and the third argument is a module path.
/// Modules are required to implement the functions `parse_meta_item`, `parse_meta_item_inline`,
/// and `parse_meta_item_flag`, even if just to return an error. The signatures of these functions
/// should match the equivalent functions in [`ParseMetaItem`](crate::ParseMetaItem).
///
/// `parse_meta_item_flag` implementations can call [`flag_disallowed_error`] for a standard error
/// if flags are not supported by the target type.
#[macro_export]
macro_rules! parse_named_meta_item_with {
    ($input:expr, $name_span:expr, $($path:tt)* $(,)?) => {
        $crate::parse_helpers::parse_named_meta_item_with(
            $input,
            $name_span,
            $($path)* :: parse_meta_item,
            |inputs, mode| $($path)* :: parse_meta_item_inline(inputs, mode),
            $($path)* :: parse_meta_item_flag,
        )
    };
}

/// Parses an inline fixed-length tuple struct from a stream.
///
/// `len` is the maximum number of items to parse. Each stream in `inputs` will be parsed in order,
/// calling `func` for every field with three arguments: the current stream, a slice containing the
/// current stream and all following streams, and the index of the current field starting from `0`.
/// Commas will be parsed between each field.
///
/// Returns the number of items that were successfully parsed, or [`Err`] on any parsing errors or
/// the first failure of `func`. A trailing comma will be consumed only if the return value is less
/// than `len`.
#[inline]
pub fn parse_tuple_struct<'s, F, S>(mut inputs: &[S], len: usize, mut func: F) -> Result<usize>
where
    S: Borrow<ParseBuffer<'s>>,
    F: FnMut(ParseStream, &[S], usize) -> Result<()>,
{
    let mut counter = 0;
    while let Some(input) = inputs.first() {
        let input = input.borrow();
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

/// Parses a path allowing any keywords as identifiers, and containing no path arguments.
pub fn parse_any_path(input: ParseStream) -> Result<syn::Path> {
    Ok(syn::Path {
        leading_colon: input.parse()?,
        segments: {
            let mut segments = syn::punctuated::Punctuated::new();
            loop {
                if input.cursor().ident().is_none() {
                    break;
                }
                let ident = <syn::Ident as syn::ext::IdentExt>::parse_any(input)?;
                segments.push_value(syn::PathSegment::from(ident));
                if !input.peek(syn::Token![::]) {
                    break;
                }
                let punct = input.parse()?;
                segments.push_punct(punct);
            }
            if segments.is_empty() {
                return Err(input.error("expected path"));
            } else if segments.trailing_punct() {
                return Err(input.error("expected path segment"));
            }
            segments
        },
    })
}

/// Parses an inline struct with named fields from a stream.
///
/// Each stream in `inputs` will be parsed in order, first parsing a path by calling
/// [`syn::Path::parse_mod_style`]. Then, `func` will be called for every field with three
/// arguments: the current stream, the name of the field as a string, and the span of the field
/// name. Commas will be parsed between each field. A trailing comma will always be consumed.
/// Callers will typically check the field name for a match, and then call
/// [`parse_named_meta_item`] or [`parse_named_meta_item_with`](fn@parse_named_meta_item_with).
///
/// Callers will usually want to use [`check_unknown_attribute`] and [`skip_meta_item`] when
/// encountering any unknown fields.
///
/// Returns [`Err`] on any parsing errors or the first failure of `func`.
#[inline]
pub fn parse_struct<'s, F, S>(inputs: &[S], mut func: F) -> Result<()>
where
    S: Borrow<ParseBuffer<'s>>,
    F: FnMut(ParseStream, &str, Span) -> Result<()>,
{
    for input in inputs {
        let input = input.borrow();
        loop {
            if input.is_empty() {
                break;
            }
            let path = input.call(parse_any_path)?;
            func(input, key_to_string(&path).as_str(), path.span())?;
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
    }
    Ok(())
}

/// Skips over items in a [`ParseStream`](syn::parse::ParseStream) until a comma is reached.
///
/// Consumes all tokens up until the comma. Does not consume the comma.
#[inline]
pub fn skip_meta_item(input: ParseStream) {
    input
        .step(|cursor| {
            let mut cur = *cursor;
            while let Some((tt, next)) = cur.token_tree() {
                if let TokenTree::Punct(punct) = tt {
                    if punct.as_char() == ',' {
                        break;
                    }
                }
                cur = next;
            }
            Ok(((), cur))
        })
        .ok();
}

/// Consumes all tokens left in a list of streams.
#[inline]
pub fn skip_all<'s, S>(inputs: &[S])
where
    S: Borrow<ParseBuffer<'s>>,
{
    for input in inputs {
        input
            .borrow()
            .step(|cursor| {
                let mut cur = *cursor;
                while let Some((_, next)) = cur.token_tree() {
                    cur = next;
                }
                Ok(((), cur))
            })
            .ok();
    }
}

#[inline]
fn parse_tokens<T, F: FnOnce(ParseStream) -> Result<T>>(input: TokenStream, func: F) -> Result<T> {
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

/// Parses a collection of [`ToTokens`](quote::ToTokens) values into a struct with named fields.
///
/// Should be used from [`ParseAttributes`](crate::ParseAttributes) or
/// [`ExtractAttributes`](crate::ExtractAttributes) implementations, which should respectively call
/// [`ref_tokens`] or [`take_tokens`] before passing any token streams into this function.
///
/// Each stream will be parsed for a parenthesized group. Then, `func` will be called with a list
/// of every [`ParseBuffer`](syn::parse::ParseBuffer) inside these groups, and the corresponding
/// [`Span`](proc_macro2::Span) for the path of that group. An end-of-stream (or trailing comma
/// followed by end-of-stream) will be consumed from each buffer after `func` returns. Callers
/// should ensure the success path consumes all other tokens in each buffer.
///
/// Structs with named fields or tuple structs should be parsed by respectively calling
/// [`parse_struct`] or [`parse_tuple_struct`].
///
/// Returns a parse error if any of the streams failed to parse. Otherwise, passes through the
/// return value from `func`.
pub fn parse_struct_attr_tokens<T, I, F, R>(inputs: I, func: F) -> Result<R>
where
    T: quote::ToTokens,
    I: IntoIterator<Item = (T, SmallString<'static>, Span)>,
    F: FnOnce(&[ParseBuffer], &[(SmallString<'static>, Span)]) -> Result<R>,
{
    fn parse_next<T, I, F, R>(
        mut iter: I,
        buffers: Vec<ParseBuffer>,
        mut paths: Vec<(SmallString<'static>, Span)>,
        func: F,
    ) -> Result<R>
    where
        T: quote::ToTokens,
        I: Iterator<Item = (T, SmallString<'static>, Span)>,
        F: FnOnce(&[ParseBuffer], &[(SmallString<'static>, Span)]) -> Result<R>,
    {
        if let Some((tokens, name, span)) = iter.next() {
            let tokens = tokens.into_token_stream();
            parse_tokens(tokens, |stream| {
                let mut buffers: Vec<ParseBuffer> = buffers; // move to shrink the lifetime
                if stream.is_empty() {
                    buffers.push(stream.fork());
                } else {
                    let content = Paren::parse_delimited(stream)?;
                    buffers.push(content);
                }
                paths.push((name, span));
                parse_next(iter, buffers, paths, func)
            })
        } else {
            let r = func(&buffers, &paths)?;
            for buffer in buffers {
                parse_eof_or_trailing_comma(&buffer)?;
            }
            Ok(r)
        }
    }
    parse_next(inputs.into_iter(), Vec::new(), Vec::new(), func)
}

/// Converts a [`ToKeyString`] to a [`SmallString`].
pub fn key_to_string<T: ToKeyString>(t: &T) -> SmallString<'static> {
    struct FormatKey<'a, A>(&'a A);

    impl<'a, A: ToKeyString> std::fmt::Display for FormatKey<'a, A> {
        #[inline]
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            self.0.fmt_key_string(f)
        }
    }

    let mut ss = SmallString::new();
    std::fmt::write(&mut ss, format_args!("{}", FormatKey(t))).unwrap();
    ss
}

/// Concatenates two path strings.
///
/// If `prefix` is empty, returns `path`. Otherwise, joins `prefix` and `path` together with `::`
/// as a separator. If `prefix` ends with `::` then no additional `::` will be added.
#[inline]
pub fn join_path<'path>(prefix: &str, path: &'path str) -> Cow<'path, str> {
    if prefix.is_empty() {
        Cow::Borrowed(path)
    } else if prefix.ends_with("::") {
        format!("{prefix}{path}").into()
    } else {
        format!("{prefix}::{path}").into()
    }
}

/// Concatenates two path prefix strings.
///
/// Similar to [`join_path`], but all returned strings will have `::` appended to the end of them.
/// The returned string will be suitable to pass to [`str::strip_prefix`] after calling
/// [`key_to_string`].
#[inline]
pub fn join_prefix(prefix: &str, path: &str) -> String {
    if prefix.is_empty() {
        let mut out = path.to_string();
        out.push_str("::");
        out
    } else {
        let mut out = prefix.to_string();
        if !prefix.ends_with("::") {
            out.push_str("::");
        }
        out.push_str(path);
        out.push_str("::");
        out
    }
}

/// Calls [`join_path`] on all paths in a slice.
#[inline]
pub fn join_paths<'s>(prefix: &str, paths: &[&'s str]) -> Vec<Cow<'s, str>> {
    paths.iter().map(|p| join_path(prefix, p)).collect()
}

#[inline]
#[doc(hidden)]
pub fn extend_from_owned<'s>(vec: &mut Vec<&'s str>, owned: &'s [Cow<'s, str>]) {
    vec.extend(owned.iter().map(|p| p.as_ref()));
}

/// Checks if a path matches a list of names.
///
/// Each string in `segs` is checked against `path.segments[..].ident`.
///
/// Returns `false` if the paths are different, or if any segments have generic arguments.
pub fn path_matches(path: &syn::Path, segs: &[&str]) -> bool {
    if path.segments.len() != segs.len() {
        return false;
    }
    for (i, seg) in path.segments.iter().enumerate() {
        if !matches!(seg.arguments, syn::PathArguments::None) {
            return false;
        }
        if seg.ident != segs[i] {
            return false;
        }
    }
    true
}

/// Checks if all keys are contained in a set of paths.
///
/// A key is a list of names that could be used for a field. Returns `true` if each key has at
/// least one name present in `paths`.
#[inline]
pub fn has_paths(paths: &HashMap<SmallString<'static>, Span>, keys: &[&[&str]]) -> bool {
    keys.iter()
        .all(|ks| ks.iter().any(|i| paths.contains_key(*i)))
}

/// Removes some paths from a path hashmap.
///
/// `keys` is an array of `(prefix, keys)` tuples. The prefix will be be prepended to the key and
/// then the resulting string will be removed from `paths`.
pub fn remove_paths(paths: &mut HashMap<SmallString<'static>, Span>, keys: &[(&str, &[&str])]) {
    for (prefix, ks) in keys {
        for path in *ks {
            let path = join_path(prefix, path);
            paths.remove(path.as_ref());
        }
    }
}

/// Appends errors if any keys are found in a list of paths.
///
/// `keys` is an array of `(prefix, keys)` tuples. The prefix will be be prepended to the key and
/// then checked against `paths`. For every key found, a "disallowed" error will be appended to
/// `errors`.
pub fn disallow_paths(
    paths: &HashMap<SmallString<'static>, Span>,
    keys: &[(&str, &[&str])],
    cur: &str,
    errors: &Errors,
) {
    for (prefix, ks) in keys {
        for path in *ks {
            let path = join_path(prefix, path);
            if let Some(span) = paths.get(path.as_ref()) {
                errors.push(
                    *span,
                    format_args!("`{path}` not allowed with variant `{cur}`"),
                );
            }
        }
    }
}

/// Creates a span encompassing a collection of [`ParseStream`](syn::parse::ParseStream)s.
///
/// Returns a span joined togther from each item in the slice, using
/// [`Span::join`](proc_macro2::Span::join).
#[inline]
pub fn inputs_span<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S]) -> Span {
    let mut iter = inputs.iter();
    let first = iter.next();
    if let Some(input) = first {
        let mut span = input.borrow().span();
        for next in iter {
            if let Some(joined) = span.join(next.borrow().span()) {
                span = joined;
            }
        }
        span
    } else {
        Span::call_site()
    }
}

/// Returns an error with a "missing required field" message.
///
/// The error will be spanned to `span`.
#[inline]
pub fn missing_field_error(name: &str, span: Span) -> Error {
    syn::Error::new(span, format_args!("missing required field {name}"))
}

/// Returns an error with a "unexpected flag" message.
///
/// The error will be spanned to `span`.
#[inline]
pub fn flag_disallowed_error(span: Span) -> Error {
    syn::Error::new(span, "unexpected flag, expected `=` or parentheses")
}

/// Returns an error with an "unknown field" message.
///
/// `path` is the name of the unknown field that will be included in the error message. The names
/// of allowed fields can be passed in `fields`. If any fields are a close enough match to `path`,
/// then a "did you mean" message will be appended to the end of the error message. The error will
/// be spanned to `span`.
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
            format!("unknown field `{path}`, did you mean `{closest}`?"),
        )
    } else {
        Error::new(span, format!("unknown field `{path}`"))
    }
}

/// Appends an error if a field is not included in a list of fields.
///
/// If `path` is not present in `fields` then an error will be appended to `errors`. The error will
/// be spanned to `span`. Returns `true` if an error was appended.
#[inline]
pub fn check_unknown_attribute(path: &str, span: Span, fields: &[&str], errors: &Errors) -> bool {
    if !fields.contains(&path) {
        errors.push_syn(unknown_error(path, span, fields));
        return true;
    }
    false
}

/// Returns an iterator of pairs of [`TokenStream`](proc_macro2::TokenStream)s and the
/// corresponding path [`Span`](proc_macro2::Span) from a matched
/// [`ParseAttributes`](crate::ParseAttributes).
#[inline]
pub fn ref_tokens<'t, P, T>(
    input: &'t T,
) -> impl Iterator<Item = (TokenStream, SmallString<'static>, Span)> + 't
where
    P: crate::ParseAttributes<'t, T>,
    T: crate::HasAttributes,
{
    T::attrs(input)
        .iter()
        .filter(|&a| P::path_matches(a.path()))
        .map(|a| {
            let value = match &a.meta {
                syn::Meta::Path(_) => Default::default(),
                syn::Meta::List(list) => proc_macro2::TokenTree::Group(proc_macro2::Group::new(
                    match list.delimiter {
                        syn::MacroDelimiter::Paren(_) => proc_macro2::Delimiter::Parenthesis,
                        syn::MacroDelimiter::Brace(_) => proc_macro2::Delimiter::Brace,
                        syn::MacroDelimiter::Bracket(_) => proc_macro2::Delimiter::Bracket,
                    },
                    list.tokens.clone(),
                ))
                .into(),
                syn::Meta::NameValue(nv) => nv.value.to_token_stream(),
            };
            (value, key_to_string(a.path()), a.path().span())
        })
}

/// Returns an iterator of [`TokenStream`](proc_macro2::TokenStream)s and the corresponding path
/// [`Span`](proc_macro2::Span) from a matched [`ExtractAttributes`](crate::ExtractAttributes).
///
/// All matching attributes will be removed from `input`. The resulting
/// [`TokenStream`](proc_macro2::TokenStream)s are moved into the iterator.
#[inline]
pub fn take_tokens<E, T>(
    input: &mut T,
) -> Result<impl Iterator<Item = (TokenStream, SmallString<'static>, Span)>>
where
    E: crate::ExtractAttributes<T>,
    T: crate::HasAttributes,
{
    let attrs = T::attrs_mut(input)?;
    let mut tokens = Vec::new();
    let mut index = 0;
    while index < attrs.len() {
        if E::path_matches(attrs[index].path()) {
            let attr = attrs.remove(index);
            let span = attr.path().span();
            let key = key_to_string(attr.path());
            let value = match attr.meta {
                syn::Meta::Path(_) => Default::default(),
                syn::Meta::List(list) => proc_macro2::TokenTree::Group(proc_macro2::Group::new(
                    match list.delimiter {
                        syn::MacroDelimiter::Paren(_) => proc_macro2::Delimiter::Parenthesis,
                        syn::MacroDelimiter::Brace(_) => proc_macro2::Delimiter::Brace,
                        syn::MacroDelimiter::Bracket(_) => proc_macro2::Delimiter::Bracket,
                    },
                    list.tokens,
                ))
                .into(),
                syn::Meta::NameValue(nv) => nv.value.into_token_stream(),
            };
            tokens.push((value, key, span));
        } else {
            index += 1;
        }
    }
    Ok(tokens.into_iter())
}

/// Gets the first [`Span`](proc_macro2::Span) in a list of path names and spans.
///
/// Returns [`Span::call_site`](proc_macro2::Span::call_site) if the slice is empty.
#[inline]
pub fn first_span(spans: &[(SmallString<'static>, Span)]) -> Span {
    spans.first().map(|s| s.1).unwrap_or_else(Span::call_site)
}

/// Gets the first string in a list of path names and spans.
///
/// Returns [`None`] if the slice is empty.
#[inline]
pub fn first_path_name<'a>(spans: &'a [(SmallString<'static>, Span)]) -> Option<&'a str> {
    spans.first().map(|s| s.0.as_str())
}

/// Forks all streams in a [`ParseStream`](syn::parse::ParseStream) slice into a new [`Vec`] of
/// owned [`ParseBuffer`](syn::parse::ParseBuffer)s.
///
/// Used to copy the initial states in a list of streams before calling [`parse_struct`].
#[inline]
pub fn fork_inputs<'s, S>(inputs: &[S]) -> Vec<ParseBuffer<'s>>
where
    S: Borrow<ParseBuffer<'s>>,
{
    inputs.iter().map(|s| s.borrow().fork()).collect()
}

/// Appends a "too many enum variants" error.
///
/// The error will be appended to `errors` and spanned to `span`. `prefix` will be appended to the
/// variant names.
#[inline]
pub fn only_one_variant(span: Span, prefix: &str, (va, vb): (&str, &str), errors: &Errors) {
    errors.push(
        span,
        std::format_args!(
            "expected only one enum variant, got `{}` and `{}`",
            join_path(prefix, va),
            join_path(prefix, vb)
        ),
    );
}

/// Appends a "enum variant expected" error message.
///
/// `variants` is a list of variants, where each variant is a list of fields, where each field is a
/// list of names that can be used for that field.
///
/// The error will be appended to `errors` and spanned to `span`. `prefix` will be appended to the
/// variant names.
#[inline]
pub fn variant_required(span: Span, prefix: &str, variants: &[&[&[&str]]], errors: &Errors) {
    errors.push(
        span,
        std::format_args!(
            "expected one of {}",
            variants
                .iter()
                .map(|keys| {
                    let mut iter = keys.iter().map(|idents| {
                        idents
                            .iter()
                            .map(|i| join_path(prefix, i).into_owned())
                            .collect::<Vec<_>>()
                            .join("|")
                    });
                    if keys.len() == 1 {
                        iter.next().unwrap()
                    } else {
                        format!("({})", iter.collect::<Vec<_>>().join(", "))
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
    );
}

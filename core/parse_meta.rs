use crate::{parse_helpers::*, Errors, Result};
use proc_macro2::Span;
use quote::ToTokens;
use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    fmt::{self, Write},
    hash::Hash,
};
use syn::{
    parse::{Nothing, Parse, ParseBuffer, ParseStream, Peek},
    punctuated::Punctuated,
    Token,
};

/// The context a meta item parser is operating in.
#[derive(Debug, Clone, Copy)]
pub enum ParseMode {
    /// Named context, corresponding to [`syn::FieldsNamed`].
    ///
    /// Contains the [`Span`](proc_macro2::Span) of the name tokens.
    Named(Span),
    /// Unnamed context, corresponding to [`syn::FieldsUnnamed`].
    Unnamed,
}

impl ParseMode {
    /// Converts `self` to a [`Self::Named`].
    ///
    /// If `self` is [`Unnamed`](Self::Unnamed), uses the [`Span`](proc_macro2::Span) from `input`.
    #[inline]
    pub fn to_named(&self, input: ParseStream) -> Self {
        match self {
            Self::Unnamed => Self::Named(input.span()),
            m => *m,
        }
    }
    /// Gets the stored [`Span`](proc_macro2::Span).
    ///
    /// If `self` is [`Unnamed`](Self::Unnamed), returns [`None`].
    #[inline]
    pub fn named_span(&self) -> Option<Span> {
        match self {
            Self::Named(s) => Some(*s),
            _ => None,
        }
    }
    /// Gets the stored [`Span`](proc_macro2::Span).
    ///
    /// If `self` is [`Unnamed`](Self::Unnamed), returns the [`Span`](proc_macro2::Span) from
    /// `input`.
    #[inline]
    pub fn to_span(&self, input: ParseStream) -> Span {
        self.named_span().unwrap_or_else(|| input.span())
    }
    /// Gets the stored [`Span`](proc_macro2::Span).
    ///
    /// If `self` is [`Unnamed`](Self::Unnamed), returns <code>[inputs_span]\(inputs)</code>.
    #[inline]
    pub fn to_full_span<'s, S: Borrow<ParseBuffer<'s>>>(&self, inputs: &[S]) -> Span {
        self.named_span().unwrap_or_else(|| inputs_span(inputs))
    }
}

/// Base trait for parsing a single field out of [`syn::parse::ParseStream`].
///
/// This trait is conceptually similar to [`syn::parse::Parse`], but with a few key differences:
/// - It can tell when an item is being parsed from a tuple struct or a struct with named fields.
/// - It can be parsed "inline" by containers that consume the surrounding delimiters.
/// - It can be parsed as a "flag" by containers that allow empty fields.
/// - It can provide custom parsing when it is used with a named attribute.
///
/// Implementations are provided for all [primitives](std::primitive) and [standard
/// collections](std::collections), as well as for tuples, arrays, and for most parseable
/// node structures in [`syn`].
pub trait ParseMetaItem: Sized {
    /// Parse the item from the tokens in `input`.
    ///
    /// If the item can contain commas, the whole item should be wrapped in a pair of delimiters.
    /// The stream should should not parse any trailing commas or additional tokens past the end of
    /// the item.
    ///
    /// The `_mode` argument describes whether this item is being parsed in a named or an unnamed
    /// context. This argument will rarely be used. It only should be checked when an item uses
    /// [`Self::parse_meta_item_flag`] to provide shorthand for a default enum value, and wants to
    /// avoid parsing it in a named context. In an unnamed context, both values will still need to
    /// be parsed. See the source to the
    /// [implementation](ParseMetaItem#impl-ParseMetaItem-for-Option<T>) of this trait on
    /// <code>[Option]&lt;T></code> for an example of when to check `_mode`.
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self>;
    /// Parse the item in an inline context.
    ///
    /// The stream can consume any number of commas. Items with a finite length should not consume
    /// a trailing comma. See [`Self::parse_meta_item`] for a description of the `_mode` argument.
    ///
    /// This function should be implemented for custom data structures, but is not intended to be
    /// called by a parser of a structure containing it as a field. Instead, the custom data
    /// structure should implement [`ParseMetaFlatUnnamed::parse_meta_flat_unnamed`],
    /// [`ParseMetaFlatNamed::parse_meta_flat_named`], or
    /// [`ParseMetaRest::parse_meta_rest`]. Then, the implementation of this
    /// function should delegate to one of those methods.
    ///
    /// The default implementation directly calls [`Self::parse_meta_item`].
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        parse_first(inputs, _mode, Self::parse_meta_item)
    }
    /// Parses an empty flag value.
    ///
    /// Only called when parsing a struct with named fields, with no value given after the field
    /// name. `_span` is the span of the token following the name.
    ///
    /// The default implementation returns an error.
    #[inline]
    fn parse_meta_item_flag(_span: Span) -> Result<Self> {
        Err(flag_disallowed_error(_span))
    }
    /// Parses the item following a name.
    ///
    /// Only called when parsing a struct with named fields. Normally implementations will not need
    /// to override this method.
    ///
    /// The default implementation simply calls
    /// [`parse_helpers::parse_named_meta_item`](crate::parse_helpers::parse_named_meta_item).
    #[inline]
    fn parse_meta_item_named(input: ParseStream, _name: &str, span: Span) -> Result<Self> {
        parse_named_meta_item(input, span)
    }
    /// Fallback for when a required item is missing.
    ///
    /// Only called when a required (non-default) field was omitted from a parsed attribute.
    /// Implementations on types that implement [`Default`] will most likely want to return
    /// <code>[Ok]\([Default::default]\(\)\)</code> here.
    ///
    /// The default implementation returns an error.
    #[inline]
    fn missing_meta_item(name: &str, span: Span) -> Result<Self> {
        Err(missing_field_error(name, span))
    }
}

/// Parses a meta item for a structure with unnamed fields.
///
/// Automatically implemented by `derive(ParseMetaItem)` for tuple
/// structures, but can also be manually implemented for array-like types.
///
/// A parent structure that wants to flatten items of this type into its own fields should call
/// [`Self::parse_meta_flat_unnamed`].
pub trait ParseMetaFlatUnnamed: Sized {
    /// Returns the number of fields in this structure.
    ///
    /// Returns [`None`] if it can parse any number of fields.
    ///
    /// Currently, this is only useful when generating error messages.
    fn field_count() -> Option<usize>;
    /// Parse the item from a group of inline contexts.
    ///
    /// The streams can consume any number of commas. The streams in `inputs` should not be
    /// considered contiguous, but should imply a trailing comma if one is not present.
    /// Implementations should parse each stream in order, continuing to the next stream when the
    /// current one is exhausted.
    ///
    /// If the structure can take any number of fields, it should parse all streams to the end and
    /// ensure that a traiing comma is parsed.
    ///
    /// If the structure contains a finite number of fields, then trailing comma should not be
    /// consumed. Once all fields are parsed, the function should return, without any further
    /// modifications to the current stream or any following streams.
    ///
    /// `index` is the starting offset into the currently parsing tuple. It should be used as a
    /// base when generating error messages.
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
        index: usize,
    ) -> Result<Self>;
}

/// Parses a meta item for a structure with named fields.
///
/// Automatically implemented by `derive(ParseMetaItem)` for structures with named fields, and for
/// enums.
///
/// A parent structure that wants to flatten items of this type into its own fields should call
/// [`Self::parse_meta_flat_named`].
pub trait ParseMetaFlatNamed: Sized {
    /// Returns an array specifying all optional and required fields accepted by this structure.
    fn field_names() -> &'static [&'static str];
    /// Parse the item from a group of inline named contexts.
    ///
    /// The streams can consume any number of commas. The streams in `inputs` should not be
    /// considered contiguous, but should imply a trailing comma if one is not present.
    /// Implementations should parse each stream in order, continuing to the next stream when the
    /// current one is exhausted.
    ///
    /// All streams should be parsed to the end, skipping over unknown fields, and consuming a
    /// trailing comma if present. The [`crate::parse_helpers::parse_struct`] helper should be used
    /// for convenience. `prefix` should be stripped off the names of all fields before pattern
    /// matching on them.
    ///
    /// If `validate` is true, then an error should be generated upon encountering any
    /// fields not in [`Self::field_names`].
    fn parse_meta_flat_named<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
        prefix: &str,
        validate: bool,
    ) -> Result<Self>;
    /// A flag noting if this parser will consume all unknown fields.
    ///
    /// Should be set to `true` only for structures containing a `#[deluxe(rest)]` field.
    const ACCEPTS_ALL: bool = false;
}

/// Parses a meta item for a structure with named fields that concatenates all matching items.
///
/// Should be implemented on collection types. Implementations are provided for the common
/// collection types in [`std`].
pub trait ParseMetaAppend: Sized {
    /// Parse the item from a group of inline named contexts.
    ///
    /// Fields with names matching any path in `paths` will be appended. Non-matching fields should
    /// be skipped with [`crate::parse_helpers::skip_meta_item`].
    fn parse_meta_append<'s, S, I, P>(inputs: &[S], paths: I) -> Result<Self>
    where
        S: Borrow<ParseBuffer<'s>>,
        I: IntoIterator<Item = P>,
        I::IntoIter: Clone,
        P: AsRef<str>;
}

/// Parses a meta item for a structure with named fields that consumes all fields.
///
/// Should be implemented on map types. Implementations are provided for the common map types in
/// [`std`].
pub trait ParseMetaRest: Sized {
    /// Parse the item from a group of inline named contexts.
    ///
    /// Fields with names in `exclude` should be should be skipped with
    /// [`crate::parse_helpers::skip_meta_item`].
    fn parse_meta_rest<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        exclude: &[&str],
    ) -> Result<Self>;
}

/// A trait for converting an attribute key to a string.
///
/// Used for performing path comparisons and for constructing error messages when using arbitrary
/// types as attribute keys. Currently only used by the [`ParseMetaItem`] implementations for
/// [`BTreeMap`] and [`HashMap`].
pub trait ToKeyString: Sized {
    /// Formats the given value as a key string.
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result;
    /// Runs function `f` with the key converted to a `&str`.
    ///
    /// Can be specialized by values that can pass a cheaply borrowed `&str`. The default
    /// implementation calls `f` with the result from [`key_to_string`].
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f(&key_to_string(self))
    }
}

macro_rules! impl_parse_meta_item_primitive {
    ($ty:ty, $lit:ty, $conv:ident) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                impl_parse_meta_item_primitive!(@conv input, _mode, $lit, $conv)
            }
        }
        impl ToKeyString for $ty {
            #[inline]
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(self, f)
            }
        }
    };
    (@conv $input:ident, $mode:ident, $lit:ty, base10_parse) => {
        $input.parse::<$lit>()?.base10_parse()
    };
    (@conv $input:ident, $mode:ident, $lit:ty, value) => {
        Ok($input.parse::<$lit>()?.value())
    };
    (@conv $input:ident, $mode:ident, $lit:ty, from_str) => {
        $crate::with::from_str::parse_meta_item($input, $mode)
    };
}

impl_parse_meta_item_primitive!(i8, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i128, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(isize, syn::LitInt, base10_parse);

impl ParseMetaItem for u8 {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitByte) {
            Ok(input.parse::<syn::LitByte>()?.value())
        } else if lookahead.peek(syn::LitInt) {
            Ok(input.parse::<syn::LitInt>()?.base10_parse()?)
        } else {
            Err(lookahead.error())
        }
    }
}

impl_parse_meta_item_primitive!(u16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u128, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(usize, syn::LitInt, base10_parse);

impl_parse_meta_item_primitive!(f32, syn::LitFloat, base10_parse);
impl_parse_meta_item_primitive!(f64, syn::LitFloat, base10_parse);

impl ParseMetaItem for bool {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(input.parse::<syn::LitBool>()?.value())
    }
    #[inline]
    fn parse_meta_item_flag(_: Span) -> Result<Self> {
        Ok(true)
    }
}

impl_parse_meta_item_primitive!(std::num::NonZeroI8, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroI16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroI32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroI64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroI128, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroIsize, syn::LitInt, base10_parse);

impl_parse_meta_item_primitive!(std::num::NonZeroU8, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroU16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroU32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroU64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroU128, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(std::num::NonZeroUsize, syn::LitInt, base10_parse);

impl_parse_meta_item_primitive!(char, syn::LitChar, value);

impl ParseMetaItem for String {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(input.parse::<syn::LitStr>()?.value())
    }
}

impl ToKeyString for String {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f(self)
    }
}

impl ParseMetaItem for std::path::PathBuf {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(Self::from(input.parse::<syn::LitStr>()?.value()))
    }
}

impl ToKeyString for std::path::PathBuf {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.to_string_lossy())
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f(&self.to_string_lossy())
    }
}

impl ParseMetaItem for std::ffi::OsString {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(Self::from(input.parse::<syn::LitStr>()?.value()))
    }
}

impl ToKeyString for std::ffi::OsString {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.to_string_lossy())
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f(&self.to_string_lossy())
    }
}

impl_parse_meta_item_primitive!(std::net::IpAddr, syn::LitStr, from_str);
impl_parse_meta_item_primitive!(std::net::Ipv4Addr, syn::LitStr, from_str);
impl_parse_meta_item_primitive!(std::net::Ipv6Addr, syn::LitStr, from_str);
impl_parse_meta_item_primitive!(std::net::SocketAddr, syn::LitStr, from_str);
impl_parse_meta_item_primitive!(std::net::SocketAddrV4, syn::LitStr, from_str);
impl_parse_meta_item_primitive!(std::net::SocketAddrV6, syn::LitStr, from_str);

impl<T: ParseMetaItem> ParseMetaItem for Option<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        match mode {
            ParseMode::Named(_) => T::parse_meta_item(input, mode).map(Some),
            ParseMode::Unnamed => {
                mod keywords {
                    syn::custom_keyword!(Some);
                    syn::custom_keyword!(None);
                }
                let lookahead = input.lookahead1();
                if lookahead.peek(keywords::Some) {
                    input.parse::<keywords::Some>()?;
                    Paren::parse_delimited_meta_item(input, mode).map(Some)
                } else if lookahead.peek(keywords::None) {
                    input.parse::<keywords::None>()?;
                    Ok(None)
                } else {
                    Err(lookahead.error())
                }
            }
        }
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        T::parse_meta_item_flag(span).map(Some)
    }
    #[inline]
    fn missing_meta_item(_name: &str, _span: Span) -> Result<Self> {
        Ok(None)
    }
}

impl<T: ToKeyString> ToKeyString for Option<T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Some(v) => {
                f.write_str("Some(")?;
                v.fmt_key_string(f)?;
                f.write_char(')')
            }
            None => f.write_str("None"),
        }
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaItem for [T; N] {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        Self::parse_meta_flat_unnamed(inputs, _mode, 0)
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaFlatUnnamed for [T; N] {
    #[inline]
    fn field_count() -> Option<usize> {
        Some(N)
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
        index: usize,
    ) -> Result<Self> {
        let mut a = arrayvec::ArrayVec::<T, N>::new();
        let errors = Errors::new();
        let mut failed = 0;
        errors.push_result(parse_tuple_struct(inputs, N, |stream, _, _| {
            match errors.push_result(T::parse_meta_item(stream, ParseMode::Unnamed)) {
                Some(v) => a.push(v),
                None => {
                    failed += 1;
                    skip_meta_item(stream);
                }
            }
            Ok(())
        }));
        if a.len() + failed != N {
            errors.push(
                inputs_span(inputs),
                format!(
                    "Expected array at index {} of length {}, got {}",
                    index,
                    N,
                    a.len() + failed,
                ),
            );
        }
        errors.check()?;
        Ok(a.into_inner().unwrap_or_else(|_| unreachable!()))
    }
}

impl<T: ToKeyString, const N: usize> ToKeyString for [T; N] {
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('[')?;
        for (i, v) in self.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            v.fmt_key_string(f)?;
        }
        f.write_char(']')
    }
}

macro_rules! impl_parse_meta_item_collection {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(inputs, _mode, 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
                inputs: &[S],
                _mode: ParseMode,
                _index: usize
            ) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        match errors.push_result($param::parse_meta_item(input, ParseMode::Unnamed)) {
                            Some($item) => $push,
                            None => skip_meta_item(input),
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaAppend for $ty <$param> {
            fn parse_meta_append<'s, S, I, P>(inputs: &[S], paths: I) -> Result<Self>
            where
                S: Borrow<ParseBuffer<'s>>,
                I: IntoIterator<Item = P>,
                I::IntoIter: Clone,
                P: AsRef<str>
            {
                let mut $ident = Self::new();
                let errors = Errors::new();
                let paths = paths.into_iter();
                errors.push_result(parse_struct(inputs, |input, p, pspan| {
                    if paths.clone().any(|path| path.as_ref() == p) {
                        match errors.push_result(<_>::parse_meta_item_named(input, p, pspan)) {
                            Some($item) => $push,
                            None => skip_meta_item(input),
                        }
                    } else {
                        skip_meta_item(input);
                    }
                    Ok(())
                }));
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$param: ToKeyString $(+ $bound $(+ $bounds)*)?> ToKeyString for $ty <$param> {
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_char('[')?;
                for (i, v) in self.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    v.fmt_key_string(f)?;
                }
                f.write_char(']')
            }
        }
    };
}

macro_rules! impl_parse_meta_item_set {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(inputs, _mode, 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
                inputs: &[S],
                _mode: ParseMode,
                _index: usize
            ) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let span = input.span();
                        match errors.push_result($param::parse_meta_item(input, ParseMode::Unnamed)) {
                            Some($item) => if !$push {
                                let span = input.span().join(span).unwrap_or(span);
                                errors.push(span, "Duplicate key");
                            },
                            None => skip_meta_item(input),
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaAppend for $ty <$param> {
            fn parse_meta_append<'s, S, I, P>(
                inputs: &[S],
                paths: I,
            ) -> Result<Self>
            where
                S: Borrow<ParseBuffer<'s>>,
                I: IntoIterator<Item = P>,
                I::IntoIter: Clone,
                P: AsRef<str>
            {
                let errors = Errors::new();
                let mut $ident = Self::new();
                let paths = paths.into_iter();
                parse_struct(inputs, |input, p, pspan| {
                    if paths.clone().any(|path| path.as_ref() == p) {
                        let span = input.span();
                        let $item = <_>::parse_meta_item_named(input, p, pspan);
                        let span = input.span().join(span).unwrap_or(span);
                        match errors.push_result($item) {
                            Some($item) => if !$push {
                                errors.push(span, "Duplicate key");
                            },
                            None => skip_meta_item(input),
                        }
                    } else {
                        skip_meta_item(input);
                    }
                    Ok(())
                })?;
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$param: ToKeyString $(+ $bound $(+ $bounds)*)?> ToKeyString for $ty <$param> {
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_char('[')?;
                for (i, v) in self.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    v.fmt_key_string(f)?;
                }
                f.write_char(']')
            }
        }
    };
}

macro_rules! impl_parse_meta_item_map {
    (
        $ty:ident <$kp:ident $(: $kbound:tt $(+ $kbounds:tt)*)?, $vp:ident>,
        $ident:ident, $key:ident, $value:ident, $push:expr
    ) => {
        impl<$kp, $vp> ParseMetaItem for $ty <$kp, $vp>
        where
            $kp: ParseMetaItem + ToKeyString $(+ $kbound $(+ $kbounds)*)?,
            $vp: ParseMetaItem,
        {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Brace::parse_delimited_meta_item(input, _mode)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], mode: ParseMode) -> Result<Self> {
                <Self as ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(inputs, mode, 0)
            }
        }

        impl<$kp, $vp> ParseMetaFlatUnnamed for $ty <$kp, $vp>
        where
            $kp: ParseMetaItem + ToKeyString $(+ $kbound $(+ $kbounds)*)?,
            $vp: ParseMetaItem,
        {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
                inputs: &[S],
                _mode: ParseMode,
                _index: usize
            ) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let start = input.span();
                        let $key = $kp::parse_meta_item(input, ParseMode::Unnamed)?;
                        let span = input.span().join(start).unwrap_or(start);
                        let $value = errors.push_result($key.with_key_string(|ks| {
                            <_>::parse_meta_item_named(input, &ks, start)
                        }));
                        match $value {
                            Some($value) => if !$push {
                                errors.push(span, "Duplicate key");
                            }
                            None => skip_meta_item(input),
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$kp, $vp> ParseMetaRest for $ty <$kp, $vp>
        where
            $kp: ParseMetaItem + ToKeyString $(+ $kbound $(+ $kbounds)*)?,
            $vp: ParseMetaItem,
        {
            fn parse_meta_rest<'s, S: Borrow<ParseBuffer<'s>>>(
                inputs: &[S],
                exclude: &[&str],
            ) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let start = input.span();
                        let $key = $kp::parse_meta_item(input, ParseMode::Unnamed)?;
                        let span = input.span().join(start).unwrap_or(start);
                        let $value = errors.push_result($key.with_key_string(|ks| {
                            if exclude.contains(&ks) {
                                skip_meta_item(input);
                                Ok::<_, crate::Error>(None)
                            } else {
                                Ok(Some(<_>::parse_meta_item_named(input, &ks, start)?))
                            }
                        })).flatten();
                        if let Some($value) = $value {
                            if !$push {
                                errors.push(span, "Duplicate key");
                            }
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$kp, $vp> ToKeyString for $ty <$kp, $vp>
        where
            $kp: ToKeyString $(+ $kbound $(+ $kbounds)*)?,
            $vp: ToKeyString,
        {
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_char('{')?;
                for (i, (k, v)) in self.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    k.fmt_key_string(f)?;
                    f.write_str(" = ")?;
                    v.fmt_key_string(f)?;
                }
                f.write_char('}')
            }
        }
    };
}

impl_parse_meta_item_collection!(Vec<T>, v, item, v.push(item));
impl_parse_meta_item_set!(BTreeSet<T: Ord>, set, item, set.insert(item));
impl_parse_meta_item_map!(
    BTreeMap<K: Ord, V>,
    map,
    key,
    value,
    map.insert(key, value).is_none()
);
impl_parse_meta_item_collection!(BinaryHeap<T: Ord>, heap, item, heap.push(item));
impl_parse_meta_item_set!(HashSet<T: Hash + Eq>, set, item, set.insert(item));
impl_parse_meta_item_map!(
    HashMap<K: Hash + Eq, V>,
    map,
    key,
    value,
    map.insert(key, value).is_none()
);
impl_parse_meta_item_collection!(LinkedList<T>, list, item, list.push_back(item));
impl_parse_meta_item_collection!(VecDeque<T>, v, item, v.push_back(item));

macro_rules! impl_parse_meta_item_wrapper {
    ($i:ident $(::$ip:ident)* <$param:ident>) => {
        impl<$param: ParseMetaItem> ParseMetaItem for $i $(::$ip)* <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
                Ok(Self::new($param::parse_meta_item(input, mode)?))
            }
            #[inline]
            fn parse_meta_item_flag(span: Span) -> Result<Self> {
                $param::parse_meta_item_flag(span).map(Self::new)
            }
        }
    };
}

impl_parse_meta_item_wrapper!(Box<T>);

impl<T: ToKeyString> ToKeyString for Box<T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt_key_string(f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        (**self).with_key_string(f)
    }
}

impl_parse_meta_item_wrapper!(std::rc::Rc<T>);

impl<T: ToKeyString> ToKeyString for std::rc::Rc<T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt_key_string(f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        (**self).with_key_string(f)
    }
}
impl_parse_meta_item_wrapper!(std::cell::Cell<T>);

impl<T: ToKeyString + Copy> ToKeyString for std::cell::Cell<T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.get().fmt_key_string(f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        self.get().with_key_string(f)
    }
}

impl_parse_meta_item_wrapper!(std::cell::RefCell<T>);

impl<T: ToKeyString> ToKeyString for std::cell::RefCell<T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.borrow().fmt_key_string(f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        self.borrow().with_key_string(f)
    }
}

impl<'t, T: ParseMetaItem + Clone> ParseMetaItem for std::borrow::Cow<'t, T> {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        Ok(Self::Owned(T::parse_meta_item(input, mode)?))
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        T::parse_meta_item_flag(span).map(Self::Owned)
    }
}

impl<'t, T: ToKeyString + Clone> ToKeyString for std::borrow::Cow<'t, T> {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt_key_string(f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        (**self).with_key_string(f)
    }
}

impl ParseMetaItem for proc_macro2::TokenTree {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.step(|cursor| {
            cursor
                .token_tree()
                .ok_or_else(|| crate::Error::new(cursor.span(), "unexpected end of tokens"))
        })
    }
}

macro_rules! impl_fmt_key_string_display {
    ($(#[$attr:meta])* $ty:ty) => {
        $(#[$attr])*
        impl ToKeyString for $ty {
            #[inline]
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(self, f)
            }
        }
    };
    ($ty:ty, #proc_macro) => {
        impl_fmt_key_string_display!(
            #[cfg(feature = "proc-macro")]
            #[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
            $ty
        );
    };
}

impl_fmt_key_string_display!(proc_macro2::TokenTree);

/// Consumes up to the next comma or the end of the stream. Returns an error if the stream is
/// empty or the first token is a comma.
impl ParseMetaItem for proc_macro2::TokenStream {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        if input.peek(Token![,]) {
            return Err(syn::Error::new(input.span(), "unexpected comma"));
        }
        input.step(|cursor| {
            let mut cursor = *cursor;
            let mut tts = Vec::new();
            while let Some((tt, rest)) = cursor.token_tree() {
                match tt {
                    proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' => break,
                    tt => tts.push(tt),
                }
                cursor = rest;
            }
            if tts.is_empty() {
                return Err(syn::Error::new(mode.to_span(input), "expected token"));
            }
            Ok((tts.into_iter().collect(), cursor))
        })
    }
    #[inline]
    fn parse_meta_item_flag(_: Span) -> Result<Self> {
        Ok(Default::default())
    }
    #[inline]
    fn missing_meta_item(_name: &str, _span: Span) -> Result<Self> {
        Ok(Default::default())
    }
}

impl ParseMetaFlatUnnamed for proc_macro2::TokenStream {
    #[inline]
    fn field_count() -> Option<usize> {
        None
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
        _index: usize,
    ) -> Result<Self> {
        let mut tts = Self::new();
        for input in inputs {
            let input = input.borrow();
            loop {
                if input.is_empty() {
                    break;
                }
                tts.extend(Self::parse_meta_item(input, ParseMode::Unnamed)?);
            }
        }
        Ok(tts)
    }
}

impl_fmt_key_string_display!(proc_macro2::TokenStream);

impl ParseMetaItem for proc_macro2::Literal {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.step(|cursor| {
            cursor
                .literal()
                .ok_or_else(|| crate::Error::new(cursor.span(), "expected literal"))
        })
    }
}

impl_fmt_key_string_display!(proc_macro2::Literal);

impl ParseMetaItem for proc_macro2::Punct {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.step(|cursor| {
            cursor
                .punct()
                .ok_or_else(|| crate::Error::new(cursor.span(), "expected punctuation"))
        })
    }
}

impl_fmt_key_string_display!(proc_macro2::Punct);

impl ParseMetaItem for proc_macro2::Group {
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.step(|cursor| {
            for delim in {
                use proc_macro2::Delimiter::*;
                [Parenthesis, Brace, Bracket, None]
            } {
                if let Some((group, _, cursor)) = cursor.group(delim) {
                    return Ok((proc_macro2::Group::new(delim, group.token_stream()), cursor));
                }
            }
            Err(crate::Error::new(
                cursor.span(),
                "expected parenthesis or brace or bracket",
            ))
        })
    }
}

#[cfg(feature = "proc-macro")]
#[inline]
fn convert_token_tree(tt: proc_macro2::TokenTree) -> syn::Result<proc_macro::TokenTree> {
    #[inline]
    fn convert_delimiter(d: proc_macro2::Delimiter) -> proc_macro::Delimiter {
        match d {
            proc_macro2::Delimiter::Parenthesis => proc_macro::Delimiter::Parenthesis,
            proc_macro2::Delimiter::Brace => proc_macro::Delimiter::Brace,
            proc_macro2::Delimiter::Bracket => proc_macro::Delimiter::Bracket,
            proc_macro2::Delimiter::None => proc_macro::Delimiter::None,
        }
    }
    #[inline]
    fn convert_spacing(s: proc_macro2::Spacing) -> proc_macro::Spacing {
        match s {
            proc_macro2::Spacing::Alone => proc_macro::Spacing::Alone,
            proc_macro2::Spacing::Joint => proc_macro::Spacing::Joint,
        }
    }
    Ok(match tt {
        proc_macro2::TokenTree::Group(g) => {
            proc_macro::Group::new(convert_delimiter(g.delimiter()), g.stream().into()).into()
        }
        proc_macro2::TokenTree::Ident(i) => {
            proc_macro::Ident::new(&i.to_string(), i.span().unwrap()).into()
        }
        proc_macro2::TokenTree::Punct(p) => {
            proc_macro::Punct::new(p.as_char(), convert_spacing(p.spacing())).into()
        }
        proc_macro2::TokenTree::Literal(l) => l
            .to_string()
            .parse::<proc_macro::Literal>()
            .map(|mut pl| {
                pl.set_span(l.span().unwrap());
                pl
            })
            .map_err(|e| syn::Error::new(l.span(), e))?
            .into(),
    })
}

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::TokenTree {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        convert_token_tree(input.parse::<proc_macro2::TokenTree>()?)
    }
}

impl_fmt_key_string_display!(proc_macro::TokenTree, #proc_macro);

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::TokenStream {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(input.parse::<proc_macro2::TokenStream>()?.into())
    }
}

impl_fmt_key_string_display!(proc_macro::TokenStream, #proc_macro);

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::Literal {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        match convert_token_tree(proc_macro2::TokenTree::Literal(input.parse()?))? {
            proc_macro::TokenTree::Literal(l) => Ok(l),
            _ => unreachable!(),
        }
    }
}

impl_fmt_key_string_display!(proc_macro::Literal, #proc_macro);

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::Punct {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        match convert_token_tree(proc_macro2::TokenTree::Punct(input.parse()?))? {
            proc_macro::TokenTree::Punct(p) => Ok(p),
            _ => unreachable!(),
        }
    }
}

impl_fmt_key_string_display!(proc_macro::Punct, #proc_macro);

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::Group {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        match convert_token_tree(proc_macro2::TokenTree::Group(input.parse()?))? {
            proc_macro::TokenTree::Group(g) => Ok(g),
            _ => unreachable!(),
        }
    }
}

impl_fmt_key_string_display!(proc_macro::Group, #proc_macro);

#[cfg(feature = "proc-macro")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "proc-macro")))]
impl ParseMetaItem for proc_macro::Ident {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        match convert_token_tree(proc_macro2::TokenTree::Ident(input.parse()?))? {
            proc_macro::TokenTree::Ident(i) => Ok(i),
            _ => unreachable!(),
        }
    }
}

impl_fmt_key_string_display!(proc_macro::Ident, #proc_macro);

impl<T: ParseMetaItem, P: Parse + Peek + Default> ParseMetaItem for Punctuated<T, P> {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        Self::parse_meta_flat_unnamed(inputs, _mode, 0)
    }
}

impl<T: ParseMetaItem, P: Parse + Peek + Default> ParseMetaFlatUnnamed for Punctuated<T, P> {
    #[inline]
    fn field_count() -> Option<usize> {
        None
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
        _index: usize,
    ) -> Result<Self> {
        let mut p = Punctuated::new();
        let errors = Errors::new();
        for input in inputs {
            let input = input.borrow();
            loop {
                if input.is_empty() {
                    break;
                }
                match errors.push_result(T::parse_meta_item(input, ParseMode::Unnamed)) {
                    Some(v) => {
                        p.push(v);
                        if !input.is_empty() && errors.push_result(input.parse::<P>()).is_some() {
                            break;
                        }
                    }
                    None => {
                        // skip tokens until we find a P
                        while !input.is_empty() {
                            if input.peek(P::default()) {
                                input.parse::<P>()?;
                                break;
                            }
                            input.step(|c| Ok(((), c.token_tree().map(|(_, c)| c).unwrap())))?;
                        }
                    }
                }
            }
        }
        errors.check()?;
        Ok(p)
    }
}

impl<T: ToKeyString, P: ToTokens> ToKeyString for Punctuated<T, P> {
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('[')?;
        for p in self.pairs() {
            match p {
                syn::punctuated::Pair::Punctuated(t, p) => {
                    t.fmt_key_string(f)?;
                    p.to_token_stream().fmt_key_string(f)?;
                    f.write_char(' ')?;
                }
                syn::punctuated::Pair::End(t) => t.fmt_key_string(f)?,
            }
        }
        f.write_char(']')
    }
}

macro_rules! impl_parse_meta_item_syn {
    ($(#[$attr:meta])* $ty:ty) => {
        $(#[$attr])*
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                input.parse()
            }
        }
        $(#[$attr])*
        impl ToKeyString for $ty {
            #[inline]
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.to_token_stream().fmt_key_string(f)
            }
        }
    };
    ($ty:ty, #full) => {
        impl_parse_meta_item_syn!(
            #[cfg(feature = "full")]
            #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
            $ty
        );
    };
}

macro_rules! impl_parse_meta_paren_item_syn {
    ($(#[$attr:meta])* $ty:ty) => {
        $(#[$attr])*
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
                match mode {
                    ParseMode::Named(_) => {
                        let content = Paren::parse_delimited(input)?;
                        let ret = content.parse()?;
                        content.parse::<Nothing>()?;
                        Ok(ret)
                    }
                    ParseMode::Unnamed => Ok(input.parse()?),
                }
            }
        }
        $(#[$attr])*
        impl ToKeyString for $ty {
            #[inline]
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.to_token_stream().fmt_key_string(f)
            }
        }
    };
    ($ty:ty, #full) => {
        impl_parse_meta_paren_item_syn!(
            #[cfg(feature = "full")]
            #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
            $ty
        );
    };
}

impl_parse_meta_item_syn!(syn::AngleBracketedGenericArguments);
impl_parse_meta_item_syn!(syn::BareFnArg);
impl_parse_meta_item_syn!(syn::BoundLifetimes);
impl_parse_meta_item_syn!(syn::BinOp);
impl_parse_meta_item_syn!(syn::Expr, #full);
impl_parse_meta_item_syn!(syn::ExprArray, #full);
impl_parse_meta_paren_item_syn!(syn::ExprAssign, #full);
impl_parse_meta_item_syn!(syn::ExprCall, #full);
impl_parse_meta_item_syn!(syn::ExprCast, #full);
impl_parse_meta_item_syn!(syn::ExprField, #full);
impl_parse_meta_item_syn!(syn::ExprIndex, #full);
impl_parse_meta_item_syn!(syn::ExprLit, #full);
impl_parse_meta_item_syn!(syn::ExprMethodCall, #full);
impl_parse_meta_item_syn!(syn::ExprParen, #full);
impl_parse_meta_item_syn!(syn::ExprPath, #full);
impl_parse_meta_item_syn!(syn::ExprRange, #full);
impl_parse_meta_item_syn!(syn::ExprRepeat, #full);
impl_parse_meta_item_syn!(syn::ExprTuple, #full);
impl_parse_meta_item_syn!(syn::FnArg, #full);
impl_parse_meta_item_syn!(syn::GenericParam);
impl ParseMetaItem for syn::Ident {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        syn::ext::IdentExt::parse_any(input)
    }
}
impl ToKeyString for syn::Ident {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl_parse_meta_item_syn!(syn::Lifetime);
impl_parse_meta_item_syn!(syn::LifetimeParam);
impl_parse_meta_item_syn!(syn::Lit);
impl_parse_meta_item_syn!(syn::LitStr);
impl_parse_meta_item_syn!(syn::LitByteStr);
impl_parse_meta_item_syn!(syn::LitByte);
impl_parse_meta_item_syn!(syn::LitChar);
impl_parse_meta_item_syn!(syn::LitInt);
impl_parse_meta_item_syn!(syn::LitFloat);
impl_parse_meta_item_syn!(syn::LitBool);
impl_parse_meta_item_syn!(syn::MetaList);
impl_parse_meta_paren_item_syn!(syn::Meta);
impl_parse_meta_paren_item_syn!(syn::MetaNameValue);
#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl ParseMetaItem for syn::Pat {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        syn::Pat::parse_single(input)
    }
}
#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl ToKeyString for syn::Pat {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_token_stream().fmt_key_string(f)
    }
}
impl ParseMetaItem for syn::Path {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.parse()
    }
}
impl ToKeyString for syn::Path {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, seg) in self.segments.iter().enumerate() {
            if i > 0 {
                f.write_str("::")?;
            }
            seg.ident.fmt_key_string(f)?;
            if !seg.arguments.is_empty() {
                seg.arguments.to_token_stream().fmt_key_string(f)?;
            }
        }
        Ok(())
    }
}
impl_parse_meta_item_syn!(syn::PathSegment);
impl_parse_meta_item_syn!(syn::ParenthesizedGenericArguments);
impl_parse_meta_item_syn!(syn::Receiver, #full);
impl_parse_meta_item_syn!(syn::Signature, #full);
impl_parse_meta_item_syn!(syn::TraitBound);
impl_parse_meta_item_syn!(syn::Type);
impl_parse_meta_item_syn!(syn::TypeArray);
impl_parse_meta_item_syn!(syn::TypeBareFn);
impl_parse_meta_item_syn!(syn::TypeImplTrait);
impl_parse_meta_item_syn!(syn::TypePath);
impl_parse_meta_item_syn!(syn::TypePtr);
impl_parse_meta_item_syn!(syn::TypeReference);
impl_parse_meta_item_syn!(syn::TypeSlice);
impl_parse_meta_item_syn!(syn::TypeTraitObject);
impl_parse_meta_item_syn!(syn::TypeTuple);
impl_parse_meta_item_syn!(syn::TypeParamBound);
impl_parse_meta_item_syn!(syn::UseTree, #full);
impl_parse_meta_item_syn!(syn::UnOp);
impl_parse_meta_item_syn!(syn::Visibility);
impl_parse_meta_item_syn!(syn::WherePredicate);

impl ParseMetaItem for () {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        let content = Paren::parse_delimited(input)?;
        content.parse::<Nothing>()?;
        Ok(())
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        for input in inputs {
            input.borrow().parse::<Nothing>()?;
        }
        Ok(())
    }
    #[inline]
    fn parse_meta_item_flag(_: Span) -> Result<Self> {
        Ok(())
    }
}

impl ToKeyString for () {
    #[inline]
    fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("()")
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f("()")
    }
}

macro_rules! impl_parse_meta_item_tuple {
    ($len:literal: $($index:tt $param:tt $item:ident)+) => {
        impl<$($param: ParseMetaItem,)+> ParseMetaItem for ($($param,)+) {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Paren::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(inputs, _mode, 0)
            }
        }

        impl<$($param: ParseMetaItem,)+> ParseMetaFlatUnnamed for ($($param,)+) {
            #[inline]
            fn field_count() -> Option<usize> {
                Some($len)
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
                inputs: &[S],
                _mode: ParseMode,
                _index: usize
            ) -> Result<Self> {
                $(let mut $item = FieldStatus::None;)+
                let errors = Errors::new();
                errors.push_result(parse_tuple_struct(inputs, $len, |stream, _, index| {
                    match index {
                        $($index => $item.parse_unnamed_item(stream, &errors),)+
                        _ => unreachable!(),
                    }
                    Ok(())
                }));
                $(if $item.is_none() {
                    errors.push(
                        inputs_span(inputs),
                        format!("Expected tuple of length {}, got {}", $len, $index),
                    );
                    return errors.bail();
                };)+
                errors.check()?;
                $(let $item = $item.unwrap_or_else(|| unreachable!());)+
                Ok(($($item,)+))
            }
        }

        impl<$($param: ToKeyString,)+> ToKeyString for ($($param,)+) {
            #[inline]
            fn fmt_key_string(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_char('(')?;

                $(
                    impl_parse_meta_item_tuple!(@push_comma f, $index);
                    self.$index.fmt_key_string(f)?;
                )+
                f.write_char(')')
            }
        }
    };
    (@push_comma $f:ident, 0) => { };
    (@push_comma $f:ident, $index:literal) => { $f.write_str(", ")?; };
}

impl_parse_meta_item_tuple!(1: 0 T0 t0);
impl_parse_meta_item_tuple!(2: 0 T0 t0 1 T1 t1);
impl_parse_meta_item_tuple!(3: 0 T0 t0 1 T1 t1 2 T2 t2);
impl_parse_meta_item_tuple!(4: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3);
impl_parse_meta_item_tuple!(5: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4);
impl_parse_meta_item_tuple!(6: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5);
impl_parse_meta_item_tuple!(7: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6);
impl_parse_meta_item_tuple!(8: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7);
impl_parse_meta_item_tuple!(9: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8);
impl_parse_meta_item_tuple!(10: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9);
impl_parse_meta_item_tuple!(11: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10);
impl_parse_meta_item_tuple!(12: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11);
impl_parse_meta_item_tuple!(13: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12);
impl_parse_meta_item_tuple!(14: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13);
impl_parse_meta_item_tuple!(15: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13 14 T14 t14);
impl_parse_meta_item_tuple!(16: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13 14 T14 t14 15 T15 t15);

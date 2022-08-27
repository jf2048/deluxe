use crate::{parse_helpers::*, Errors, Result};
use proc_macro2::Span;
use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    hash::Hash,
};
use syn::{
    parse::{Nothing, Parse, ParseBuffer, ParseStream},
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
    /// If `self` is [`Unnamed`](Self::Unnamed), returns `None`.
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
    /// If `self` is [`Unnamed`](Self::Unnamed), returns <code>[inputs_span](inputs)</code>.
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
    /// context. This argument will rarely be used. It only should be checked when an item
    /// uses [`Self::parse_meta_item_flag`] to provide shorthand for a default enum value, and
    /// wants to avoid parsing it in a named context. In an unnamed context, both values will
    /// still need to be parsed. See the implementation of this trait on `Option<T>` for an
    /// example of when to check `_mode`.
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
        parse_first(inputs, _mode, |input| Self::parse_meta_item(input, _mode))
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
    /// Returns `None` if it can parse any number of fields.
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
    /// be skipped with [`crate::parse_helpers::skip_named_meta_item`].
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
    /// [`crate::parse_helpers::skip_named_meta_item`].
    fn parse_meta_rest<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        exclude: &[&str],
    ) -> Result<Self>;
}

macro_rules! impl_parse_meta_item_primitive {
    ($ty:ty, $lit:ty, $conv:ident) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                impl_parse_meta_item_primitive!(@conv input, $lit, $conv)
            }
        }
    };
    (@conv $input:ident, $lit:ty, base10_parse) => {
        $input.parse::<$lit>()?.base10_parse()
    };
    (@conv $input:ident, $lit:ty, value) => {
        Ok($input.parse::<$lit>()?.value())
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

impl_parse_meta_item_primitive!(String, syn::LitStr, value);
impl_parse_meta_item_primitive!(char, syn::LitChar, value);

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
        Self::parse_meta_flat_unnamed(inputs, 0)
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaFlatUnnamed for [T; N] {
    #[inline]
    fn field_count() -> Option<usize> {
        Some(N)
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        index: usize,
    ) -> Result<Self> {
        let mut a = arrayvec::ArrayVec::<T, N>::new();
        parse_tuple_struct(inputs, N, |stream, _, _| {
            a.push(T::parse_meta_item(stream, ParseMode::Unnamed)?);
            Ok(())
        })?;
        a.into_inner().map_err(|a| {
            syn::Error::new(
                inputs_span(inputs),
                format!(
                    "Expected array at index {} of length {}, got {}",
                    index,
                    N,
                    a.len()
                ),
            )
        })
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
                Self::parse_meta_flat_unnamed(inputs, 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _index: usize) -> Result<Self> {
                let mut $ident = Self::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let $item = $param::parse_meta_item(input, ParseMode::Unnamed)?;
                        $push;
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
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
                parse_struct(inputs, |input, p, pspan| {
                    if paths.clone().any(|path| path.as_ref() == p) {
                        let $item = parse_named_meta_item(input, pspan)?;
                        $push;
                    } else {
                        skip_named_meta_item(input);
                    }
                    Ok(())
                })?;
                errors.check()?;
                Ok($ident)
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
                Self::parse_meta_flat_unnamed(inputs, 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _index: usize) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    let input = input.borrow();
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let span = input.span();
                        let $item = $param::parse_meta_item(input, ParseMode::Unnamed)?;
                        let span = input.span().join(span).unwrap();
                        if !$push {
                            errors.push(span, "Duplicate key");
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
                        let $item = parse_named_meta_item(input, pspan)?;
                        let span = input.span().join(span).unwrap();
                        if !$push {
                            errors.push(span, "Duplicate key");
                        }
                    } else {
                        skip_named_meta_item(input);
                    }
                    Ok(())
                })?;
                errors.check()?;
                Ok($ident)
            }
        }
    };
}

macro_rules! impl_parse_meta_item_map {
    (
        $ty:ident <$kp:ident $(: $kbound:tt $(+ $kbounds:tt)*)?, $vp:ident>,
        $ident:ident, $key:ident, $value:ident, $push:expr
    ) => {
        impl<$kp: ParseMetaItem $(+ $kbound $(+ $kbounds)*)?, $vp: ParseMetaItem> ParseMetaItem for $ty <$kp, $vp> {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Brace::parse_delimited_meta_item(input, _mode)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _mode: ParseMode) -> Result<Self> {
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
                        let span = input.span().join(start).unwrap();
                        let $value = parse_named_meta_item(input, start)?;
                        if !$push {
                            errors.push(span, "Duplicate key");
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
            $kp: ParseMetaItem $(+ $kbound $(+ $kbounds)*)? + std::borrow::Borrow<syn::Path>,
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
                        if exclude.contains(&path_to_string($key.borrow()).as_str()) {
                            skip_named_meta_item(input);
                        } else {
                            let span = input.span().join(start).unwrap();
                            let $value = parse_named_meta_item(input, start)?;
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
    };
}

impl_parse_meta_item_collection!(Vec<T>, v, item, v.push(item));
impl_parse_meta_item_set!(BTreeSet<T: Ord>, set, item, set.insert(item));
impl_parse_meta_item_map!(BTreeMap<K: Ord, V>, map, key, value, map.insert(key, value).is_none());
impl_parse_meta_item_collection!(BinaryHeap<T: Ord>, heap, item, heap.push(item));
impl_parse_meta_item_set!(HashSet<T: Hash + Eq>, set, item, set.insert(item));
impl_parse_meta_item_map!(HashMap<K: Hash + Eq, V>, map, key, value, map.insert(key, value).is_none());
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
impl_parse_meta_item_wrapper!(std::rc::Rc<T>);
impl_parse_meta_item_wrapper!(std::cell::Cell<T>);
impl_parse_meta_item_wrapper!(std::cell::RefCell<T>);

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

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaItem for Punctuated<T, P> {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        Self::parse_meta_flat_unnamed(inputs, 0)
    }
}

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaFlatUnnamed for Punctuated<T, P> {
    #[inline]
    fn field_count() -> Option<usize> {
        None
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _index: usize,
    ) -> Result<Self> {
        let mut p = Punctuated::new();
        for input in inputs {
            let input = input.borrow();
            loop {
                if input.is_empty() {
                    break;
                }
                p.push(T::parse_meta_item(input, ParseMode::Unnamed)?);
                if !input.is_empty() {
                    input.parse::<P>()?;
                }
            }
        }
        Ok(p)
    }
}

macro_rules! impl_parse_meta_item_syn {
    ($ty:ty) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                input.parse()
            }
        }
    };
}

macro_rules! impl_parse_meta_paren_item_syn {
    ($ty:ty) => {
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
    };
}

impl_parse_meta_item_syn!(syn::AngleBracketedGenericArguments);
impl_parse_meta_item_syn!(syn::BareFnArg);
impl_parse_meta_item_syn!(syn::BoundLifetimes);
impl_parse_meta_item_syn!(syn::BinOp);
impl_parse_meta_paren_item_syn!(syn::Binding);
impl_parse_meta_item_syn!(syn::Expr);
impl_parse_meta_item_syn!(syn::ExprArray);
impl_parse_meta_paren_item_syn!(syn::ExprAssign);
impl_parse_meta_item_syn!(syn::ExprCall);
impl_parse_meta_item_syn!(syn::ExprCast);
impl_parse_meta_item_syn!(syn::ExprField);
impl_parse_meta_item_syn!(syn::ExprIndex);
impl_parse_meta_item_syn!(syn::ExprLit);
impl_parse_meta_item_syn!(syn::ExprMethodCall);
impl_parse_meta_item_syn!(syn::ExprParen);
impl_parse_meta_item_syn!(syn::ExprPath);
impl_parse_meta_item_syn!(syn::ExprRange);
impl_parse_meta_item_syn!(syn::ExprRepeat);
impl_parse_meta_item_syn!(syn::ExprTuple);
impl_parse_meta_item_syn!(syn::ExprType);
impl_parse_meta_item_syn!(syn::FnArg);
impl_parse_meta_item_syn!(syn::GenericParam);
impl ParseMetaItem for syn::Ident {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        syn::ext::IdentExt::parse_any(input)
    }
}
impl_parse_meta_item_syn!(syn::Lifetime);
impl_parse_meta_item_syn!(syn::LifetimeDef);
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
impl_parse_meta_paren_item_syn!(syn::NestedMeta);
impl_parse_meta_item_syn!(syn::Pat);
impl_parse_meta_item_syn!(syn::Path);
impl_parse_meta_item_syn!(syn::PathSegment);
impl_parse_meta_item_syn!(syn::ParenthesizedGenericArguments);
impl_parse_meta_item_syn!(syn::Receiver);
impl_parse_meta_item_syn!(syn::Signature);
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
impl_parse_meta_item_syn!(syn::UseTree);
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

macro_rules! impl_parse_meta_item_tuple {
    ($len:literal: $($index:literal $param:tt $item:ident)+) => {
        impl<$($param: ParseMetaItem,)+> ParseMetaItem for ($($param,)+) {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Paren::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(inputs, 0)
            }
        }

        impl<$($param: ParseMetaItem,)+> ParseMetaFlatUnnamed for ($($param,)+) {
            #[inline]
            fn field_count() -> Option<usize> {
                Some($len)
            }
            fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(inputs: &[S], _index: usize) -> Result<Self> {
                $(let mut $item = None;)+
                parse_tuple_struct(inputs, $len, |stream, _, index| {
                    match index {
                        $($index => $item = Some($param::parse_meta_item(stream, ParseMode::Unnamed)?),)+
                        _ => unreachable!(),
                    }
                    Ok(())
                })?;
                $(let $item = match $item {
                    Some(t) => t,
                    None => return Err(syn::Error::new(
                        inputs_span(inputs),
                        concat!("Expected tuple of length ", $len, ", got ", $index),
                    )),
                };)+
                Ok(($($item,)+))
            }
        }
    };
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

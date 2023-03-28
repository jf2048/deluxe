use proc_macro2::{Span, TokenStream};
use quote::TokenStreamExt;
use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut},
};
use syn::parse::{ParseBuffer, ParseStream};

use crate::{parse_helpers::inputs_span, parse_meta::*};

/// The error type for parsers.
pub type Error = syn::Error;
/// The result of a parse method.
pub type Result<T> = syn::Result<T>;

/// A wrapper for a list of errors. Can be empty.
#[derive(Clone, Debug, Default)]
#[repr(transparent)]
pub struct Errors {
    // RefCell here so this can be re-entrant when used from parser combinators
    errors: RefCell<Option<Error>>,
}

impl Errors {
    #[inline]
    /// Creates a new empty error list.
    pub const fn new() -> Self {
        Self {
            errors: RefCell::new(None),
        }
    }
    /// Checks if the list contains any errors.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_none()
    }
    /// Reset the list to an empty state.
    #[inline]
    pub fn clear(&self) {
        self.errors.take();
    }
    /// Pushes one error onto the list. This function is a wrapper around [`syn::Error::new`].
    #[inline]
    pub fn push<T: Display>(&self, span: Span, message: T) {
        self.push_syn(Error::new(span, message));
    }
    /// Pushes one error onto the list, setting the error's span to
    /// [`Span::call_site()`](proc_macro2::Span::call_site).
    #[inline]
    pub fn push_call_site<T: Display>(&self, message: T) {
        self.push(Span::call_site(), message);
    }
    /// Pushes one error onto the list spanning the given syntax tree node. This
    /// function is a wrapper around [`syn::Error::new_spanned`].
    #[inline]
    pub fn push_spanned<T, U>(&self, tokens: T, message: U)
    where
        T: quote::ToTokens,
        U: Display,
    {
        self.push_syn(Error::new_spanned(tokens, message));
    }
    /// Pushes one previously constructed [`Error`] onto the list.
    #[inline]
    pub fn push_syn(&self, error: Error) {
        let mut storage = self.errors.borrow_mut();
        if let Some(storage) = storage.as_mut() {
            storage.combine(error);
        } else {
            storage.replace(error);
        }
    }
    /// Pushes an error onto the list from a [`Result`].
    ///
    /// If `result` is [`Err`], pushes the error and returns [`None`]. If `result` is [`Ok`],
    /// returns <code>[Some]\(T)</code>.
    #[inline]
    pub fn push_result<T>(&self, result: Result<T>) -> Option<T> {
        match result {
            Ok(t) => Some(t),
            Err(e) => {
                self.push_syn(e);
                None
            }
        }
    }
    /// Appends all errors from `iter` into this list.
    #[inline]
    pub fn extend<T: IntoIterator<Item = Error>>(&self, iter: T) {
        let mut errors = self.errors.borrow_mut();
        if let Some(errors) = errors.as_mut() {
            errors.extend(iter);
        } else {
            let mut iter = iter.into_iter();
            if let Some(next) = iter.next() {
                let errors = errors.insert(next);
                errors.extend(iter);
            }
        }
    }
    /// Returns `Err` if the list has errors, or `Ok(value)` if the list is empty.
    ///
    /// If the list has any errors, returns [`Err`] containing one [`Error`] with all of the errors
    /// combined using [`Error::combine`](syn::Error::combine).
    #[inline]
    pub fn into_result<T>(self, value: T) -> Result<T> {
        if let Some(err) = self.errors.take() {
            Err(err)
        } else {
            Ok(value)
        }
    }
    /// Checks if the error list is empty.
    ///
    /// If the list has any errors, returns [`Err`] containing one [`Error`] with all of the errors
    /// combined using [`Error::combine`](syn::Error::combine). Otherwise, returns [`Ok`].
    #[inline]
    pub fn check(self) -> Result<()> {
        self.into_result(())
    }
    /// Returns the inner if the error list has errors.
    ///
    /// # Panics
    ///
    /// Panics if the error list is empty.
    #[inline]
    pub fn unwrap_err(self) -> Error {
        if let Some(err) = self.errors.take() {
            err
        } else {
            panic!("expected Errors to not be empty");
        }
    }
    /// Returns a new `Err` if the error list has errors.
    ///
    /// # Panics
    ///
    /// Panics if the error list is empty.
    #[inline]
    pub fn bail<T>(self) -> Result<T> {
        Err(self.unwrap_err())
    }
    /// Converts the error list into a token stream containing [`std::compile_error`] invocations.
    ///
    /// The errors are generated with [`Error::into_compile_error`](syn::Error::into_compile_error).
    ///
    /// Returns [`None`] if the list is empty.
    #[inline]
    pub fn into_compile_error(self) -> Option<TokenStream> {
        self.errors.take().map(|e| e.into_compile_error())
    }
    /// Returns an iterator of token streams containing [`std::compile_error`] invocations.
    ///
    /// Each token stream will contain one invocation. The errors are generated with
    /// [`Error::into_compile_error`](syn::Error::into_compile_error).
    #[inline]
    pub fn into_compile_errors(self) -> impl IntoIterator<Item = TokenStream> {
        self.errors
            .take()
            .into_iter()
            .map(|e| e.into_compile_error())
    }
    /// Creates a token stream containing the current set of errors and `item`.
    pub fn output_with<Q: quote::ToTokens>(self, item: Q) -> TokenStream {
        let mut tokens = item.into_token_stream();
        quote::ToTokens::to_tokens(&self, &mut tokens);
        tokens
    }
}

impl quote::ToTokens for Errors {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.to_token_stream());
    }
    fn to_token_stream(&self) -> TokenStream {
        self.errors
            .borrow()
            .as_ref()
            .map(|e| e.to_compile_error())
            .unwrap_or_default()
    }
    #[inline]
    fn into_token_stream(self) -> TokenStream
    where
        Self: Sized,
    {
        self.into_compile_error().unwrap_or_default()
    }
}

impl From<Error> for Errors {
    #[inline]
    fn from(err: Error) -> Self {
        Self {
            errors: RefCell::new(Some(err)),
        }
    }
}

impl FromIterator<Error> for Errors {
    #[inline]
    fn from_iter<T: IntoIterator<Item = Error>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let errors = iter.next().map(|mut first| {
            first.extend(iter);
            first
        });
        Self {
            errors: RefCell::new(errors),
        }
    }
}

impl IntoIterator for Errors {
    type Item = Error;
    type IntoIter = ErrorsIntoIter;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        ErrorsIntoIter {
            errors: self.errors.take().map(|e| e.into_iter()),
        }
    }
}

/// An iterator containing all the errors in an [`Errors`].
pub struct ErrorsIntoIter {
    errors: Option<<Error as IntoIterator>::IntoIter>,
}

impl Iterator for ErrorsIntoIter {
    type Item = Error;
    fn next(&mut self) -> Option<Self::Item> {
        self.errors.as_mut().and_then(|e| e.next())
    }
}

/// A wrapper for adding a [`Span`](proc_macro2::Span) to an arbitrary value.
///
/// Implementations are provided for all the parsing traits that simply delegate to `T`, capturing
/// the inital [`Span`](proc_macro2::Span) from the [`ParseStream`](syn::parse::ParseStream).
#[derive(Copy, Clone, Debug)]
pub struct SpannedValue<T> {
    value: T,
    span: Span,
}

impl<T> SpannedValue<T> {
    /// Creates a new value wrapping a T, with the span set to
    /// [`Span::call_site`](proc_macro2::Span::call_site).
    #[inline]
    pub fn new(value: T) -> Self {
        Self::with_span(value, Span::call_site())
    }
    /// Creates a new value wrapping a T, with the span set to `span`.
    #[inline]
    pub fn with_span(value: T, span: Span) -> Self {
        Self { value, span }
    }
    /// Unwraps a `SpannedValue` into a `T`. Note this is an associated function, not a method.
    #[inline]
    pub fn into_inner(value: SpannedValue<T>) -> T {
        value.value
    }
}

impl<T: Default> Default for SpannedValue<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Display> Display for SpannedValue<T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: PartialEq> PartialEq for SpannedValue<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for SpannedValue<T> {}

impl<T: PartialOrd> PartialOrd for SpannedValue<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for SpannedValue<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T: Hash> Hash for SpannedValue<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<T> quote::ToTokens for SpannedValue<T> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut group = proc_macro2::Group::new(proc_macro2::Delimiter::None, Default::default());
        group.set_span(self.span);
        tokens.append(group);
    }
}

impl<T: ParseMetaItem> ParseMetaItem for SpannedValue<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: crate::ParseMode) -> Result<Self> {
        let span = input.span();
        let value = T::parse_meta_item(input, mode)?;
        let span = input.span().join(span).unwrap_or(span);
        Ok(Self { value, span })
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<Self> {
        let span = inputs.first().map(|p| p.borrow().span());
        let value = T::parse_meta_item_inline(inputs, mode)?;
        let span = span
            .and_then(|s| inputs.last().and_then(|p| p.borrow().span().join(s)))
            .unwrap_or_else(Span::call_site);
        Ok(Self { value, span })
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self {
            value: T::parse_meta_item_flag(span)?,
            span,
        })
    }
    #[inline]
    fn parse_meta_item_named(input: ParseStream, name: &str, span: Span) -> Result<Self> {
        let value = T::parse_meta_item_named(input, name, span)?;
        let span = input.span().join(span).unwrap_or(span);
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaFlatUnnamed> ParseMetaFlatUnnamed for SpannedValue<T> {
    #[inline]
    fn field_count() -> Option<usize> {
        T::field_count()
    }
    fn parse_meta_flat_unnamed<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
        index: usize,
    ) -> Result<Self> {
        let mut span = crate::parse_helpers::inputs_span(inputs);
        let value = T::parse_meta_flat_unnamed(inputs, mode, index)?;
        if let Some(closed) = span.join(inputs_span(inputs)) {
            span = closed;
        }
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaFlatNamed> ParseMetaFlatNamed for SpannedValue<T> {
    const ACCEPTS_ALL: bool = T::ACCEPTS_ALL;
    #[inline]
    fn field_names() -> &'static [&'static str] {
        T::field_names()
    }
    fn parse_meta_flat_named<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
        prefix: &str,
        validate: bool,
    ) -> Result<Self> {
        let mut span = crate::parse_helpers::inputs_span(inputs);
        let value = T::parse_meta_flat_named(inputs, mode, prefix, validate)?;
        if let Some(closed) = span.join(inputs_span(inputs)) {
            span = closed;
        }
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaAppend> ParseMetaAppend for SpannedValue<T> {
    fn parse_meta_append<'s, S, I, P>(inputs: &[S], paths: I) -> Result<Self>
    where
        S: Borrow<ParseBuffer<'s>>,
        I: IntoIterator<Item = P>,
        I::IntoIter: Clone,
        P: AsRef<str>,
    {
        let mut span = inputs_span(inputs);
        let value = T::parse_meta_append(inputs, paths)?;
        if let Some(closed) = span.join(inputs_span(inputs)) {
            span = closed;
        }
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaRest> ParseMetaRest for SpannedValue<T> {
    fn parse_meta_rest<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        exclude: &[&str],
    ) -> Result<Self> {
        let mut span = inputs_span(inputs);
        let value = T::parse_meta_rest(inputs, exclude)?;
        if let Some(closed) = span.join(inputs_span(inputs)) {
            span = closed;
        }
        Ok(Self { value, span })
    }
}

impl<T> From<T> for SpannedValue<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self {
            value,
            span: Span::call_site(),
        }
    }
}

impl<T> Deref for SpannedValue<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for SpannedValue<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

/// A value for a boolean named field that can only be a name (set) or omitted (unset).
///
/// Similar to an <code>[Option]&lt;[SpannedValue]&lt;[bool]>></code> but does not allow `=` or
/// `()` after the field name. Thus, it is only useful with named fields. Parsing this out of a
/// tuple struct or tuple variant will always result in a parse error.
///
/// It is not necessary to use [`#[deluxe(default)]`](ParseMetaItem#deluxedefault-1) on a field
/// using this type. The field will automatically be created with a `false` value if the name is
/// omitted.
#[derive(Copy, Clone, Debug, Default)]
pub struct Flag(Option<Span>);

impl Flag {
    /// Creates a new `true` flag value spanned to `span`.
    #[inline]
    pub fn set(span: Span) -> Self {
        Self(Some(span))
    }
    /// Creates a new `true` flag value spanned to [`Span::call_site`].
    #[inline]
    pub fn set_call_site() -> Self {
        Self(Some(Span::call_site()))
    }
    /// Creates a new `false` flag value.
    #[inline]
    pub fn unset() -> Self {
        Self(None)
    }
    /// Returns `true` if the flag was set.
    #[inline]
    pub fn is_set(&self) -> bool {
        self.0.is_some()
    }
}

impl From<bool> for Flag {
    #[inline]
    fn from(value: bool) -> Self {
        Self(value.then(Span::call_site))
    }
}

impl From<Flag> for bool {
    #[inline]
    fn from(value: Flag) -> Self {
        value.is_set()
    }
}

impl Eq for Flag {}

impl PartialEq for Flag {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.is_set() == other.is_set()
    }
}

impl PartialEq<bool> for Flag {
    #[inline]
    fn eq(&self, other: &bool) -> bool {
        self.is_set() == *other
    }
}

impl PartialEq<Flag> for bool {
    #[inline]
    fn eq(&self, other: &Flag) -> bool {
        *self == other.is_set()
    }
}

impl quote::ToTokens for Flag {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(proc_macro2::Ident::new(
            self.0.map(|_| "true").unwrap_or("false"),
            self.0.unwrap_or_else(Span::call_site),
        ));
    }
}

impl ParseMetaItem for Flag {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        Self::parse_meta_item_inline(&[input], mode)
    }
    #[inline]
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        _mode: ParseMode,
    ) -> Result<Self> {
        Err(Error::new(
            crate::parse_helpers::inputs_span(inputs),
            "field with type `Flag` can only be a named field with no value",
        ))
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self(Some(span)))
    }
    #[inline]
    fn parse_meta_item_named(input: ParseStream, _name: &str, span: Span) -> Result<Self> {
        if input.is_empty() || input.peek(syn::Token![,]) {
            Self::parse_meta_item_flag(span)
        } else {
            Err(Error::new(input.span(), "unexpected token"))
        }
    }
    #[inline]
    fn missing_meta_item(_name: &str, _span: Span) -> Result<Self> {
        Ok(Self::unset())
    }
}

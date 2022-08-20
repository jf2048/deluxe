use proc_macro2::{Span, TokenStream};
use std::{
    cell::RefCell,
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut},
};
use syn::spanned::Spanned;

use crate::{parse_helpers::inputs_span, parse_meta::*};

/// The error type for parsers.
pub type Error = syn::Error;
/// The result of a parse method.
pub type Result<T> = syn::Result<T>;

/// A wrapper for a list of errors. Can be empty.
#[derive(Clone, Debug, Default)]
#[repr(transparent)]
pub struct Errors {
    errors: RefCell<Option<Error>>,
}

impl Errors {
    #[inline]
    /// Creates a new empty error list.
    pub fn new() -> Self {
        Default::default()
    }
    /// Checks if the list contains any errors.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_none()
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
    /// Checks if the error list is empty.
    ///
    /// If the list has any errors, returns `Err` containing one `Error` with all of the errors
    /// combined using [`Error::combine`](syn::Error::combine). Otherwise, returns `Ok`.
    #[inline]
    pub fn check(self) -> Result<()> {
        if let Some(err) = self.errors.take() {
            Err(err)
        } else {
            Ok(())
        }
    }
    /// Returns an iterator of token streams containing the [`std::compile_error`] invocations
    /// generated with [`Error::to_compil_error`](syn::Error::to_compile_error).
    #[inline]
    pub fn into_compile_errors(self) -> impl IntoIterator<Item = TokenStream> {
        self.errors.take().into_iter().map(|e| e.to_compile_error())
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

impl<T> Spanned for SpannedValue<T> {
    #[inline]
    fn span(&self) -> Span {
        self.span
    }
}

impl<T: ParseMetaItem> ParseMetaItem for SpannedValue<T> {
    fn parse_meta_item(input: syn::parse::ParseStream, mode: crate::ParseMode) -> Result<Self> {
        let span = input.span();
        let value = T::parse_meta_item(input, mode)?;
        let span = input.span().join(span).unwrap();
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaFlatUnnamed> ParseMetaFlatUnnamed for SpannedValue<T> {
    #[inline]
    fn field_count() -> Option<usize> {
        T::field_count()
    }
    fn parse_meta_flat_unnamed(inputs: &[syn::parse::ParseStream], index: usize) -> Result<Self> {
        let mut span = crate::parse_helpers::inputs_span(inputs);
        let value = T::parse_meta_flat_unnamed(inputs, index)?;
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
    fn parse_meta_flat_named(
        inputs: &[syn::parse::ParseStream],
        prefix: &str,
        validate: bool,
    ) -> Result<Self> {
        let mut span = crate::parse_helpers::inputs_span(inputs);
        let value = T::parse_meta_flat_named(inputs, prefix, validate)?;
        if let Some(closed) = span.join(inputs_span(inputs)) {
            span = closed;
        }
        Ok(Self { value, span })
    }
}

impl<T: ParseMetaAppend> ParseMetaAppend for SpannedValue<T> {
    fn parse_meta_append<I, S>(inputs: &[syn::parse::ParseStream], paths: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        I::IntoIter: Clone,
        S: AsRef<str>,
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
    fn parse_meta_rest(inputs: &[syn::parse::ParseStream], exclude: &[&str]) -> Result<Self> {
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

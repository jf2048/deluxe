//! # Deluxe
//!
//! A procedural macro attribute parser.
//!
//! ### Abstract
//!
//! This crate offers attribute parsing closer to the design of attributes in C#. It has an
//! interface similar to [serde](https://serde.rs). Attributes are written as plain Rust structs or
//! enums, and then parsers for them are generated automatically. They can contain arbitrary
//! expressions and can inherit from other attributes using a "flatten" mechanism.
//!
//! The parsers in this crate directly parse token streams using [`syn`]. As a result, most [`syn`]
//! types can be used directly as fields.
//!
//! ### Usage
//!
//! Functionality in this crate is centered around three traits, and their respective derive macros:
//! - **[`ExtractAttributes`](macro@ExtractAttributes)**
//!
//!   Extracts attributes from an object containing a list of [`syn::Attribute`], and parses them
//!   into a Rust type. Should be implemented for top-level structures that will be parsed directly
//!   out of a set of matching attributes.
//! - **[`ParseAttributes`](macro@ParseAttributes)**
//!
//!   Parses a Rust type from any object containing a list of [`syn::Attribute`]. Should be used if
//!   the set of matching attributes can potentially be shared between this type and other types.
//! - **[`ParseMetaItem`](macro@ParseMetaItem)**
//!
//!   Parses a Rust type from a [`ParseStream`](syn::parse::ParseStream). Should be implemented for
//!   any types that can be nested inside an attribute.
//!
//! Basic usage of this crate requires simply deriving one (or a few) of these traits, and then
//! calling [`extract_attributes`] or [`parse_attributes`]. For more advanced functionality,
//! several `#[deluxe(...)]` attributes are supported on structs, enums, variants and fields. See
//! the documentation for each derive macro for a description of the supported attributes.
//!
//! For more complex usage, manual implementations of these traits can be provided. See the
//! documentation on individual traits in [`deluxe_core`] for more details on how to manually
//! implement your own parsers.
//!
//! ### Basic Example
//!
//! To create a derive macro that can add some simple metadata to a Rust type from an attribute,
//! start by defining a struct that derives [`ExtractAttributes`](macro@ExtractAttributes). Then,
//! call [`extract_attributes`] in your derive macro to create an instance of the struct:
//!
//! ```
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_desc))]
//! struct MyDescription {
//!     name: String,
//!     #[deluxe(default)]
//!     version: Option<String>,
//! }
//!
//! fn my_derive(item: proc_macro2::TokenStream) -> deluxe::Result<proc_macro2::TokenStream> {
//!     let mut input = syn::parse2::<syn::DeriveInput>(item)?;
//!
//!     // Extract the attributes!
//!     let MyDescription { name, version } = deluxe::extract_attributes(&mut input)?;
//!     // Convert to an Iterator so `quote` can make the surrounding tokens optional.
//!     let version = version.into_iter();
//!
//!     // Now get some info to generate an associated function...
//!     let ident = &input.ident;
//!     let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
//!
//!     Ok(quote::quote! {
//!         impl #impl_generics #ident #type_generics #where_clause {
//!             fn my_desc() -> String {
//!                 let mut s = String::new();
//!                 s.push_str("Name: ");
//!                 s.push_str(#name);
//!                 #(
//!                     s.push_str(", Version: ");
//!                     s.push_str(#version);
//!                 )*
//!                 s
//!             }
//!         }
//!     })
//! }
//!
//! # my_derive(quote::quote! {
//! #     #[my_desc(name = "hello world", version = "0.2")]
//! #     struct Hello;
//! # }).unwrap();
//! ```
//!
//! Then, try adding the attribute in some code that uses your macro:
//!
//! ```ignore
//! #[derive(MyDescription, Default)]
//! #[my_desc(name = "hello world", version = "0.2")]
//! struct Hello {
//!     a: i32,
//!     b: String
//! }
//!
//! let hello: Hello = Default::default();
//! assert_eq!(hello.my_desc(), "Name: hello world, Version: 0.2");
//! ```
//!
//! And that's all!
//!
//! ### Related Crates
//!
//! Deluxe is inspired by the [darling](https://docs.rs/darling) and [serde](https://serde.rs)
//! crates.

#![deny(missing_docs)]

#[doc(hidden)]
pub mod ____private {
    pub use deluxe_core::parse_helpers;
    pub use once_cell::sync::OnceCell as SyncOnceCell;
    pub use proc_macro2::Span;
    pub use std::{
        borrow::{Borrow, Cow, ToOwned},
        clone::Clone,
        collections::HashMap,
        convert::AsRef,
        default::Default,
        format_args,
        iter::IntoIterator,
        option::Option,
        primitive::{bool, str, usize},
        string::{String, ToString},
        unreachable,
        vec::Vec,
    };
    pub use syn::{
        parse::{ParseBuffer, ParseStream},
        Error, Ident, Path,
    };
}

#[doc(hidden)]
pub use deluxe_core::{
    parse_named_meta_item_with, ContainerFrom, Errors, ParseMetaAppend, ParseMetaFlatNamed,
    ParseMetaFlatUnnamed, ParseMetaRest, ParseMode,
};
pub use deluxe_core::{
    with, Error, ExtractAttributes, HasAttributes, ParseAttributes, ParseMetaItem, Result,
};
pub use deluxe_macros::*;

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

/// Parses a Rust type out of a token stream.
///
/// This is a small wrapper around [`syn::parse::Parser::parse`].
#[cfg(feature = "proc-macro")]
#[inline]
pub fn parse<T: ParseMetaItem>(attr: proc_macro::TokenStream) -> Result<T> {
    syn::parse::Parser::parse(
        |stream: syn::parse::ParseStream<'_>| {
            T::parse_meta_item_inline(&[stream], ParseMode::Named(proc_macro2::Span::call_site()))
        },
        attr,
    )
}

/// Parses a Rust type out of a token stream.
///
/// This is a small wrapper around [`syn::parse::Parser::parse2`].
#[inline]
pub fn parse2<T: ParseMetaItem>(attr: proc_macro2::TokenStream) -> Result<T> {
    syn::parse::Parser::parse2(
        |stream: syn::parse::ParseStream<'_>| {
            T::parse_meta_item_inline(&[stream], ParseMode::Named(proc_macro2::Span::call_site()))
        },
        attr,
    )
}

/// Parses a Rust type out of another type holding a list of [`syn::Attribute`].
///
/// This is a small wrapper around [`ParseAttributes::parse_attributes`].
#[inline]
pub fn parse_attributes<'t, T, R>(obj: &'t T) -> Result<R>
where
    T: HasAttributes,
    R: ParseAttributes<'t, T>,
{
    R::parse_attributes(obj)
}

/// Extracts attributes out of another type holding a list of [`syn::Attribute`], then parses them
/// into a Rust type.
///
/// This is a small wrapper around [`ExtractAttributes::extract_attributes`].
#[inline]
pub fn extract_attributes<T, R>(obj: &mut T) -> Result<R>
where
    T: HasAttributes,
    R: ExtractAttributes<T>,
{
    R::extract_attributes(obj)
}

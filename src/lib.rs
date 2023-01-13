//! # Deluxe
//!
//! A procedural macro attribute parser.
//!
//! ### Abstract
//!
//! This crate offers attribute parsing closer to the design of attributes in C#. It has an
//! interface similar to [serde](https://serde.rs). Attributes are written as plain Rust structs or
//! enums, and then parsers for them are generated automatically. They can contain arbitrary
//! expressions and can inherit from other attributes using a flattening mechanism.
//!
//! The parsers in this crate directly parse token streams using [`syn`]. As a result, most
//! built-in Rust types and `syn` types can be used directly as fields.
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
//! the examples below, and the documentation for each derive macro for a complete description of
//! the supported attributes.
//!
//! A list of field types supported by default can be seen in the list of provided [`ParseMetaItem`
//! implementations](trait@ParseMetaItem#foreign-impls). For more complex usage, manual
//! implementations of these traits can be provided. See the documentation on individual traits in
//! [`deluxe_core`] for more details on how to manually implement your own parsers.
//!
//! ### Related Crates
//!
//! Deluxe takes inspiration from the [darling](https://docs.rs/darling) crate, but offers a few
//! enhancements over it. Darling is built around pre-parsed [`syn::Meta`] objects, and therefore
//! is restricted to the [meta
//! syntax](https://doc.rust-lang.org/stable/reference/attributes.html#meta-item-attribute-syntax).
//! Deluxe parses its types directly from [`TokenStream`](proc_macro2::TokenStream) objects in the
//! attributes and so is able to use any syntax that parses as a valid token tree. Deluxe also does
//! not provide extra traits for parsing special `syn` objects like
//! [`DeriveInput`](syn::DeriveInput) and [`Field`](syn::Field). Instead, Deluxe uses a generic
//! trait to parse from any type containing a <code>[Vec]&lt;[syn::Attribute]></code>.
//!
//! ### Examples
//!
//! #### Basic
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
//!     version: String,
//! }
//!
//! fn my_derive(item: proc_macro2::TokenStream) -> deluxe::Result<proc_macro2::TokenStream> {
//!     let mut input = syn::parse2::<syn::DeriveInput>(item)?;
//!
//!     // Extract the attributes!
//!     let MyDescription { name, version } = deluxe::extract_attributes(&mut input)?;
//!
//!     // Now get some info to generate an associated function...
//!     let ident = &input.ident;
//!     let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
//!
//!     Ok(quote::quote! {
//!         impl #impl_generics #ident #type_generics #where_clause {
//!             fn my_desc() -> &'static str {
//!                 concat!("Name: ", #name, ", Version: ", #version)
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
//! #### Field Attributes
//!
//! The attributes [`alias`](macro@ParseMetaItem#deluxealias--ident-1),
//! [`default`](macro@ParseMetaItem#deluxedefault-1),
//! [`rename`](macro@ParseMetaItem#deluxerename--ident-1), and
//! [`skip`](macro@ParseMetaItem#deluxeskip-1) are supported, and behave the same as in Serde. The
//! [`append`](macro@ParseMetaItem#deluxeappend) attribute can be used on [`Vec`] fields to
//! aggregate all duplicates of a key. The [`rest`](macro@ParseMetaItem#deluxerest) attribute can
//! be used to do custom processing on any unknown keys.
//!
//! ```
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_object))]
//! struct MyObject {
//!     #[deluxe(alias = object_id)] // can be specified with key `id` or `object_id`
//!     id: u64,
//!     #[deluxe(default)] // field is optional, defaults to `Default::default` if not present
//!     count: u64,
//!     #[deluxe(default = String::from("Empty"))] // defaults to "Empty" if not present
//!     contents: String,
//!     #[deluxe(rename = name)] // can be specified only with key `name`
//!     s: String,
//!     #[deluxe(skip)] // skipped during parsing entirely
//!     internal_flag: bool,
//!     #[deluxe(append, rename = expr)] // appends any extra fields with the key `expr` to the Vec
//!     exprs: Vec<syn::Expr>,
//!     #[deluxe(rest)] // adds any unknown keys to the hash map
//!     rest: std::collections::HashMap<syn::Path, syn::Expr>,
//! }
//! ```
//!
//! ```ignore
//! // omitted fields will be set to defaults
//! #[derive(MyObject)]
//! #[my_object(id = 1, name = "First", expr = 1 + 2, count = 3)]
//! struct FirstObject;
//!
//! // `expr` can be specified multiple times because of the `append` attribute
//! #[derive(MyObject)]
//! #[my_object(object_id = 2, name = "Second", expr = 1 + 2, expr = 3 + 4)]
//! struct SecondObject;
//!
//! // `unknown` and `extra` will be stored in the `rest` hashmap
//! #[derive(MyObject)]
//! #[my_object(id = 3, name = "Third", unknown = 1 + 2, extra = 3 + 4)]
//! struct ThirdObject;
//! ```
//!
//! #### Inheritance
//!
//! The [`flatten`](macro@ParseMetaItem#deluxeflatten-1) attribute can be used to parse keys from
//! one structure inside another:
//!
//! ```
//! #[derive(deluxe::ParseMetaItem)]
//! struct A {
//!     id: u64,
//! }
//!
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(b))]
//! struct B {
//!     #[deluxe(flatten)]
//!     a: A,
//!     name: String,
//! }
//! ```
//!
//! Then, fields from both `A` and `B` can be used when deriving `B`:
//!
//! ```ignore
//! #[derive(B)]
//! #[b(id = 123, name = "object")]
//! struct Object;
//! ```
//!
//! #### Tuple Structs, Tuples and Vecs
//!
//! Deluxe also supports parsing into data structures with unnamed fields.
//!
//! ```
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_tuple))]
//! struct MyTuple(u64, String);
//!
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_idents))]
//! struct MyIdents {
//!     id: u64,
//!     names: (String, String),
//!     idents: Vec<syn::Ident>
//! }
//! ```
//!
//! The standard attribute syntax with parenthesis can be used when specifying a [`Vec`] type. The
//! alternative syntax `key = [...]` can also be used to have an appearance similar to an array
//! literal.
//!
//! ```ignore
//! #[derive(MyTuple)]
//! #[my_tuple(123, "object")]
//! struct Object;
//!
//! #[derive(MyIdents)]
//! #[my_idents(id = 7, names("hello", "world"), idents(a, b, c))]
//! struct ABC;
//!
//! // `idents` contains same values as above
//! #[derive(MyIdents)]
//! #[my_idents(id = 7, names("hello", "world"), idents = [a, b, c])]
//! struct ABC2;
//! ```
//!
//! #### C#-styled Attributes
//!
//! Attributes in C# can support positional arguments first with the named
//! arguments afterwards. This style can be emulated by using a tuple struct with a
//! normal struct flattened at the end. Placing
//! [`#[deluxe(default)]`](macro@ParseMetaItem#deluxedefault) on the struct behaves the same as
//! Serde, by filling in all fields with values from [`Default`], allowing every named argument to
//! be optional.
//!
//! ```
//! #[derive(deluxe::ParseMetaItem, Default)]
//! #[deluxe(default)]
//! struct Flags {
//!     native: bool,
//! }
//!
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(a))]
//! struct A(u64, String, #[deluxe(flatten)] Flags);
//! ```
//!
//! ```ignore
//! #[derive(A)]
//! #[a(123, "object")]
//! struct Object;
//!
//! #[derive(A)]
//! #[a(123, "native-object", native = true)]
//! struct NativeObject;
//! ```
//!
//! #### Enums
//!
//! Enums are supported by using the variant name as a single key, in snake-case. Variants can be
//! renamed, aliased and skipped in the same way as fields.
//!
//! ```
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_enum))]
//! enum MyEnum {
//!     A,
//!     B,
//!     C,
//!     #[deluxe(alias = d)]
//!     AnotherOne,
//!     #[deluxe(rename = e)]
//!     AnotherTwo,
//!     #[deluxe(skip)]
//!     SkipMe
//! }
//! ```
//!
//! ```ignore
//! #[derive(MyEnum)]
//! #[my_enum(b)]
//! struct ObjectB;
//!
//! #[derive(MyEnum)]
//! #[my_enum(another_one)]
//! struct ObjectD;
//! ```
//!
//! #### Complex Enums
//!
//! Enums with struct and tuple variants are also supported. The data inside is used as arguments
//! to the attribute. All field attributes from structs are also supported inside variants.
//!
//! Additionally, enum variants with named fields can be flattened. The behavior of a flattened
//! variant is similar to Serde's `untagged` mode. In a flattened variant, the name of the variant
//! will be ignored. Instead, Deluxe will attempt to use the unique keys in each variant to
//! determine if that variant was specified. A compile error will be thrown if it is not possible
//! to determine a unique, unambiguous key between two variants.
//!
//! ```
//! #[derive(deluxe::ExtractAttributes)]
//! #[deluxe(attributes(my_enum))]
//! enum MyEnum {
//!     A,
//!     B(u64, String),
//!     C { id: u64, name: String },
//!     #[deluxe(flatten)]
//!     D { d: u64, name: String },
//! }
//! ```
//!
//! ```ignore
//! #[derive(MyEnum)]
//! #[my_enum(a)]
//! struct ObjectA;
//!
//! #[derive(MyEnum)]
//! #[my_enum(b(1, "hello"))]
//! struct ObjectB;
//!
//! #[derive(MyEnum)]
//! #[my_enum(c(id = 2, name = "world"))]
//! struct ObjectC;
//!
//! // no inner parenthesis needed here due to flattening
//! #[derive(MyEnum)]
//! #[my_enum(d = 3, name = "moon")]
//! struct ObjectD;
//! ```
//!
//! #### Storing Containers
//!
//! During parsing, Deluxe can store references to the container type holding the attributes for
//! easier access. Container fields are skipped during attribute parsing.
//!
//! ```
//! #[derive(deluxe::ParseAttributes)]
//! #[deluxe(attributes(my_object))]
//! struct MyObject<'t> {
//!     id: u64,
//!     // Fill `container` in using the parsed type. Note this restricts the
//!     // derived `ParseAttributes` impl so it can only be used on `DeriveInput`.
//!     #[deluxe(container)]
//!     container: &'t syn::DeriveInput,
//! }
//!
//! fn my_object(item: proc_macro2::TokenStream) -> deluxe::Result<proc_macro2::TokenStream> {
//!     let input = syn::parse2::<syn::DeriveInput>(item)?;
//!
//!     // `obj.container` now holds a reference to `input`
//!     let obj: MyObject = deluxe::parse_attributes(&input)?;
//!
//!     Ok(quote::quote! { /* ... generate some code here ... */ })
//! }
//! ```
//!
//! Container fields can also be mutable references when using
//! [`ExtractAttributes`](macro@ExtractAttributes). To support both extracting and parsing, a
//! container field can also be a value type. In that case, the container will be cloned into the
//! structure.

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
    define_with_collection, define_with_map, define_with_optional, parse_named_meta_item_with,
    ContainerFrom, Errors, ParseMetaAppend, ParseMetaFlatNamed, ParseMetaFlatUnnamed,
    ParseMetaRest, ParseMode,
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

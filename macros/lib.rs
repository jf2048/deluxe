//! # Deluxe Macros
//!
//! Procedural derive macros for [deluxe](https://docs.rs/deluxe). See the documentation of that
//! crate for an overview.

#![deny(missing_docs)]
#![deny(unsafe_code)]

#[macro_use]
mod util;
mod parse_attributes;
mod parse_meta_item;
mod types;
use util::*;

use deluxe_core::Errors;
use proc_macro::TokenStream;

/// Generates [`ExtractAttributes`](deluxe_core::ExtractAttributes) for a struct or enum.
///
/// This macro is identical to [`ParseAttributes`], except for the following differences:
/// - The generated [`extract_attributes`](deluxe_core::ExtractAttributes::extract_attributes)
///   function will remove any attributes matching the paths listed in
///   [`#[deluxe(attributes(...))]`](ParseAttributes#deluxeattributes).
///
/// - Reference types for [`#[deluxe(container)]`](ParseAttributes#deluxecontainer) fields will not
///   work. The container type must be an owned type or an [`Option`], and the container will
///   always be [`clone`](Clone::clone)d into the field. The cloning will happen after the
///   attributes have been extracted. Using the `lifetime` parameter for custom container types
///   will also not work.
///
/// See the [`ParseAttributes`] and [`ParseMetaItem`] documentation for all other details.
#[proc_macro_derive(ExtractAttributes, attributes(deluxe))]
pub fn derive_extract_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item, &errors)
        .map(|input| {
            parse_attributes::impl_parse_attributes(input, &errors, parse_attributes::Mode::Extract)
        })
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

/// Generates [`ParseAttributes`](deluxe_core::ParseAttributes) for a struct or enum.
///
/// Supported attributes are the same as in [`ParseMetaItem`]. Additionally, some extra attributes
/// are supported.
///
/// ### Extra Container Attributes
///
/// The following extra attributes are supported on structs and enums:
///
/// - ##### `#[deluxe(attributes(...))]`
///
///   Provides a list of paths to match attribute names against. These paths will be checked inside
///   the [`ParseAttributes::path_matches`](deluxe_core::ParseAttributes::path_matches) function to
///   see if a path matches. This attribute can be specified multiple times to append more paths to
///   the list of matching paths.
///
///   Omitting this attribute will cause *all* paths to match. This can be useful when doing your
///   own filtering on an attribute list before parsing.
///
/// ### Extra Field Attributes
///
/// The following extra attributes are supported on struct fields and enum fields:
///
/// - ##### `#[deluxe(container)]`
///
///   Specifies that this field should store the current `obj` being parsed by
///   [`parse_attributes`](deluxe_core::ParseAttributes::parse_attributes). The field can be a
///   value type, an [`Option`], or a reference type. If the field is not a reference type, the
///   container will be [`clone`](Clone::clone)d into the field.
///
///   If the type of the container contains a lifetime, Deluxe will try to use it for a lifetime
///   bound for [`ParseAttributes`](deluxe_core::ParseAttributes). If the container type is a
///   reference, it will try to infer it from the lietime of the reference. Otherwise, it will try
///   to take the first lifetime parameter used by the type if one is present. If no lifetimes can
///   be inferred, a new lifetime parameter will be generated. See
///   [`container(lifetime = ...)`](#deluxecontainerlifetime--t-ty--pathmytype) for instructions on
///   how to specify a lifetime parameter manually.
///
///   If the struct/enum also derives [`ParseMetaItem`], then
///   [`#[deluxe(default)]`](ParseMetaItem#deluxedefault-1) is implied on any container fields. In
///   that case, a common pattern is to use an [`Option`] as the container field. It will be set to
///   [`None`] when calling [`parse_meta_item`](deluxe_core::ParseMetaItem::parse_meta_item), but
///   will be [`Some`] when calling
///   [`parse_attributes`](deluxe_core::ParseAttributes::parse_attributes).
///
///   If used within an enum, only the first container field will supply the trait bounds. Any
///   other container fields in other variants must have a compatible type and lifetime.
///
///   Fields with this attribute can safely be added to a struct or variant using
///   [`#[deluxe(transparent)]`](ParseMetaItem#deluxetransparent). Container fields do not count as
///   a parseable field, as they are never parsed from tokens.
///
/// - ##### `#[deluxe(container(lifetime = 't, ty = path::MyType)]`
///
///   Specifies that this field should store the current `obj` being parsed by
///   [`parse_attributes`](deluxe_core::ParseAttributes::parse_attributes), with a custom lifetime
///   and type bound used for the [`ParseAttributes`](deluxe_core::ParseAttributes) trait
///   implementation.
///
///   Normally the `lifetime` and `ty` are not needed when using
///   [`#[deluxe(container)]`](#deluxecontainer) because the macro can infer the lifetime and type
///   from the field itself. If Deluxe is unable to infer them, these attributes can be supplied to
///   manually provide the lifetime and type. `lifetime` can be omitted if the container is an
///   owning type.
///
///   This attribute is implemented by calling methods on the
///   [`ContainerFrom`](deluxe_core::ContainerFrom) trait. Other container types besides references
///   and [`Option`] will also need to provide an implementation for that trait.
#[proc_macro_derive(ParseAttributes, attributes(deluxe))]
pub fn derive_parse_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item, &errors)
        .map(|input| {
            parse_attributes::impl_parse_attributes(input, &errors, parse_attributes::Mode::Parse)
        })
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

/// Generates [`ParseMetaItem`](deluxe_core::ParseMetaItem) for a struct or enum.
///
/// ### Container Attributes
///
/// The following attributes are supported on structs and enums:
///
/// - ##### `#[deluxe(default)]`
///
///   Initializes the container with [`Default::default`] before parsing.
///
/// - ##### `#[deluxe(default = expr)]`
///
///   Initializes the container with the value of `expr` before parsing. The expression will
///   be evaluated every time it is needed to construct the field, and must evaluate to a value of
///   the same type as the field.
///
/// - ##### `#[deluxe(transparent)]`
///
///   Parses a struct with one field as if it were the field. Can only be used on a struct with a
///   single parseable field. Analogous to `#[repr(transparent)]`. The struct can still contain
///   fields that are [`skip`](#deluxeskip-1), as those will be ignored by `transparent`.
///
/// - ##### `#[deluxe(transparent(flatten_named)]`
///
///   `#[deluxe(transparent(flatten_unnamed)]`
///
///   `#[deluxe(transparent(flatten_unnamed, append)]`
///
///   `#[deluxe(transparent(rest)]`
///
///   Parses a struct with one field as if it were the field, additionally implementing the traits
///   required to use `flatten`, `rest`, or `append`.  Can only be used on a struct with a
///   single parseable field.
///
///   Currently, it is required to provide these additional attributes to generate the trait
///   definitions to use [`flatten`](#deluxeflatten-1), [`append`](#deluxeappend) or
///   [`rest`](#deluxerest) on this type.
///
/// - ##### `#[deluxe(and_then = expr)]`
///
///   Executes an additional function ater parsing to perform additional transformations or
///   validation on the input.
///
///   This attribute is a simple wrapper around
///   [`Result::and_then`](deluxe_core::Result::and_then). The function returned by `expr` must
///   conform to the signature <code>fn(T) -> [deluxe::Result](deluxe_core::Result)&lt;T></code>
///   where `T` is the type of the struct/enum being parsed. Returning
///   [`Err`](deluxe_core::Result::Err) will cause the entire parse to fail.
///
///   This attribute can be specified multiple times. When multiple `and_then` attributes are
///   present, Deluxe will chain each function in the order the attributes were specified.
///
///   ##### Example
///
///   ```ignore
///   #[derive(deluxe::ParseMetaItem)]
///   #[deluxe(and_then = Self::validate)]
///   struct Data(i32);
///   impl Data {
///       fn validate(self) -> deluxe::Result<Self> {
///           // ... perform some checks here ...
///           Ok(self)
///       }
///   }
///   ```
///
/// - ##### `#[deluxe(allow_unknown_fields)]`
///
///   Ignore any tokens and do not generate an error when an unknown field is encountered.
///
/// - ##### `#[deluxe(crate = path)]`
///
///   Specifies `path` as a custom path to the `deluxe` crate. Useful if `proc_macro_crate` is
///   unable to find the `deluxe` crate, for instance if the crate is only re-exported inside
///   another dependency.
///
/// ### Variant Attributes
///
/// The following attributes are supported on variants:
///
/// - ##### `#[deluxe(rename = ident)]`
///
///   Parse the variant with the given `ident` instead of its Rust name.
///
/// - ##### `#[deluxe(alias = ident)]`
///
///   Parse the variant with the given `ident`, or its Rust name. Can be repeated multiple times to
///   provide additional aliases.
///
/// - ##### `#[deluxe(transparent)]`
///
///   Parses a variant with one field as if it were the field. Can only be used on a variant with a
///   single parseable field. Analogous to `#[repr(transparent)]`. The variant can still contain
///   fields that are [`skip`](#deluxeskip-1), as those will be ignored by `transparent`.
///
/// - ##### `#[deluxe(flatten)]`
///
///   Flattens the variant so that its unique fields are used as the key for this variant instead
///   of its name. Can be used on multiple variants as long as they each have a unique set of
///   parseable fields that can be used to identify the variant. Fields with
///   [`flatten`](#deluxeflatten-1), [`append`](#deluxeappend) or [`rest`](#deluxerest) are not
///   counted as unique fields as their field names are ignored.
///
///   A single variant with no parseable fields can also be flattened. In that case, that variant
///   will always be parsed as the default variant. Setting a default variant in this way is
///   mutually exclusive with using [`#[deluxe(default)]`](#deluxedefault) on the enum.
///
/// - ##### `#[deluxe(skip)]`
///
///   Skips this variant from parsing entirely.
///
/// - ##### `#[deluxe(allow_unknown_fields)]`
///
///   Ignore any tokens and do not generate an error when an unknown field is encountered in this
///   variant.
///
/// ### Field Attributes
///
/// The following attributes are supported on struct fields and enum fields:
///
/// - ##### `#[deluxe(rename = ident)]`
///
///   Parse the field with the given `ident` instead of its Rust name.
///
/// - ##### `#[deluxe(alias = ident)]`
///
///   Parse the field with the given `ident`, or its Rust name. Can be repeated multiple times to
///   provide additional aliases.
///
/// - ##### `#[deluxe(default)]`
///
///   Initializes the field with the value of [`Default::default`] if the field is omitted.
///
///   It is not necessary to use this on fields of type [`Option`] or [`Flag`](deluxe_core::Flag),
///   or any other type that has a top-level [`#[deluxe(default)]`](#deluxedefault) on the type
///   itself.
///
/// - ##### `#[deluxe(default = expr)]`
///
///   Initializes the field with the value of `expr` if the field is omitted. The expression will
///   be evaluated every time it is needed to construct the field, and must evaluate to a value of
///   the same type as the field.
///
/// - ##### `#[deluxe(flatten)]`
///
///   Flattens the field so that its fields are parsed inline as part of the current struct or enum
///   variant.
///
///   When the container uses named fields, only enums or other structs with named fields can be
///   flattened. The fields from the flattened field can be freely interspersed with the fields
///   from the containing struct or variant. This has the effect of making it so the order of
///   flattened fields does not matter when using named fields.
///
///   When the container uses unnamed fields, only unnamed structs, tuples, and collections/arrays
///   can be flattened. The order of flattened unnamed fields is important. The fields of the
///   flattened structure will be consumed starting from the position of the field in the
///   containing tuple. Flattening a collection into a tuple struct/variant without a finite size
///   will consume all fields from that position until the end.
///
///   This attribute is implemented by either calling
///   [`ParseMetaFlatUnnamed::parse_meta_flat_unnamed`](deluxe_core::ParseMetaFlatUnnamed::parse_meta_flat_unnamed)
///   or
///   [`ParseMetaFlatNamed::parse_meta_flat_named`](deluxe_core::ParseMetaFlatNamed::parse_meta_flat_named)
///   depending on the type of the containing structure. The appropriate trait will be
///   automatically implemented when deriving [`ParseMetaItem`], but some implementations are
///   provided for common collection types. Custom container types can support flattening by
///   providing implementations of those traits.
///
/// - ##### `#[deluxe(flatten(prefix = path)])`
///
///   Flattens the field so that its fields are parsed inline as part of the current struct or enum
///   variant, only accepting fields that are prefixed with `path`. This can be used if the
///   flattened structure contains field names that conflict with the fields in the containing
///   structure.
///
///   For all other details on this attribute, refer to [`flatten`](#deluxeflatten-1).
///
/// - ##### `#[deluxe(append)]`
///
///   Allows duplicates of this field. Additional fields parsed with the same name will be appended
///   on to the previous value. This attribute is only allowed on named fields.
///
///   This attribute is implemented by calling
///   [`ParseMetaAppend::parse_meta_append`](deluxe_core::ParseMetaAppend::parse_meta_append). Some
///   implementations are provided for common collection types. Custom container types can support
///   appending by providing an implementation of that trait.
///
/// - ##### `#[deluxe(rest)]`
///
///   Inserts all unknown fields into this field. Typically, this field will be a map type with
///   [`syn::Path`] as the key. This attribute is only allowed on named fields.
///
///   This attribute is implemented by calling
///   [`ParseMetaRest::parse_meta_rest`](deluxe_core::ParseMetaRest::parse_meta_rest). Some
///   implementations are provided for common collection types. Custom map types can be allowed as
///   a rest field by providing an implementation of that trait.
///
/// - ##### `#[deluxe(map = expr)]`
///
///   `#[deluxe(and_then = expr)]`
///
///   Executes additional functions ater parsing to perform additional transformations or
///   validation on the input.
///
///   These attributes are simple wrappers around [`Result::map`](deluxe_core::Result::map) and
///   [`Result::and_then`](deluxe_core::Result::and_then). These attributes can be specified
///   multiple times. When multiple are present, Deluxe will chain each function in the order the
///   attributes were specified.
///
///   For `map`, the function returned by `expr` must conform to the signature `fn(T) -> U`. For
///   `and_then`, the function returned by `expr` must conform to the signature <code>fn(T) ->
///   [deluxe::Result](deluxe_core::Result)&lt;U></code>. Returning
///   [`Err`](deluxe_core::Result::Err) will cause the entire parse to fail. Arbitrary types can be
///   used for `T` and `U` as long as the following constraints hold:
///
///   - The first function must have a fully specified type for `T`, which will have its
///   [`ParseMetaItem`](deluxe_core::ParseMetaItem) implementation used.
///   - The `U from any function in the chain matches the `T` for the following function.
///   - The last function must have a type for `U` that matches the type of the field.
///
///   ##### Example
///
///   ```ignore
///   #[derive(deluxe::ParseMetaItem)]
///   struct Data {
///       // parses as an Ident but stored as a string
///       #[deluxe(map = |i: syn::Ident| i.to_string())]
///       ident_string: String,
///       // converts an Ident to a string and does a validation
///       #[deluxe(and_then = Self::check_ident)]
///       valid_ident_string: String,
///   }
///
///   impl Data {
///       fn check_ident(i: syn::Ident) -> deluxe::Result<String> {
///           let s = i.to_string();
///           if s == "invalid" {
///               Err(syn::Error::new(i.span(), "`invalid` not allowed"))
///           } else {
///               Ok(s)
///           }
///       }
///   }
///   ```
///
/// - ##### `#[deluxe(with = module)]`
///
///   When parsing, call functions from the path `module` instead of attempting to call
///   [`ParseMetaItem`](deluxe_core::ParseMetaItem) functions. The path can be a module path or a
///   path to a type containing associated functions.
///
///   The functions will be called as `module::parse_meta_item`, `module::parse_meta_item_inline`,
///   `module::parse_meta_item_flag`, `module::parse_meta_item_named`, and
///   `module::missing_meta_item`. All five functions must be implemented, even if just to return
///   an error. The signatures of these functions should match the equivalent functions in
///   [`ParseMetaItem`](crate::ParseMetaItem), although they can be generic over the return type.
///   Fields using this attribute are not required to implement
///   [`ParseMetaItem`](crate::ParseMetaItem).
///
///   `parse_meta_item_inline` implementations can call
///   [`parse_first`](deluxe_core::parse_helpers::parse_first) to simply delegate the impementation
///   to `parse_meta_item`. `parse_meta_item_flag` implementations can call
///   [`flag_disallowed_error`](deluxe_core::parse_helpers::flag_disallowed_error) for a standard
///   error if flags are not supported by the target type. `parse_meta_item_named` implementations
///   can call [`parse_named_meta_item_with!`](deluxe_core::parse_named_meta_item_with) using
///   `self` as the last parameter for the standard behavior.
///
///   Some common parsers are available in the [`with`](deluxe_core::with) module.
///
/// - ##### `#[deluxe(skip)]`
///
///   Skips this field from parsing entirely. The field still must receive a default value through
///   a `default` attribute either on the struct or the field, so the parse function can still
///   construct the object. If not used in a struct with [`default`](#deluxedefault), then this
///   implies [`default`](#deluxedefault-1) on the field if it is omitted.
#[proc_macro_derive(ParseMetaItem, attributes(deluxe))]
pub fn derive_parse_meta_item(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item, &errors)
        .map(|input| parse_meta_item::impl_parse_meta_item(input, &errors))
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

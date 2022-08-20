//! # Deluxe Macros
//!
//! Procedural derive macros for [deluxe](https://docs.rs/deluxe). See the documentation of that
//! crate for an overview.

#![deny(missing_docs)]

#[macro_use]
mod util;
mod field;
mod parse_attributes;
mod parse_meta_item;
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
/// - The `lifetime` parameter on
///   [`#[deluxe(container)]`](ParseAttributes#deluxecontainerlifetime--t-ty--pathmytype) will be
///   ignored, and reference types for container fields will not work. The container type must be
///   an owned type or an [`Option`], and the container will always be [`clone`](Clone::clone)d
///   into the field. The cloning will happen after the attributes have been extracted.
///
/// See the [`ParseAttributes`] and [`ParseMetaItem`] documentation for all other details.
#[proc_macro_derive(ExtractAttributes, attributes(deluxe))]
pub fn derive_extract_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
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
/// - ##### `#[deluxe(container(lifetime = 't, ty = path::MyType)]`
///
///   Specifies that this field should store the current `obj` being parsed by
///   [`parse_attributes`](deluxe_core::ParseAttributes::parse_attributes). The field can be an
///   [`Option`] or a reference type, in which case the `lifetime` and `ty` arguments can be used
///   to fill in the correct trait bounds on [`ParseAttributes`](deluxe_core::ParseAttributes). The
///   `lifetime` and `ty` arguments can likely be omitted if this is a plain type, reference type,
///   [`Option`], or [`Option`] containing a reference. In those cases the container will be
///   [`clone`](Clone::clone)d into the field.
///
///   If the container field is an [`Option`], then it can additionally be marked as
///   [`#[deluxe(default)]`](ParseMetaItem#deluxedefault-1). This is useful if the struct/enum also
///   derives [`ParseMetaItem`]. In that case, the container will be `None` when calling
///   [`parse_meta_item`](deluxe_core::ParseMetaItem::parse_meta_item), but will be `Some` when
///   calling [`parse_attributes`](deluxe_core::ParseAttributes::parse_attributes).
///
///   If used within an enum, only the first container field will supply the trait bounds. Any
///   other container fields in other variants must have a compatible type and lifetime.
///
///   Fields with this attribute can safely be used with
///   [`#[deluxe(transparent)]`](ParseMetaItem#deluxetransparent). They do not count as a parseable
///   field, as container fields are never parsed from tokens.
///
///   This attribute is implemented by calling methods on the
///   [`ToContainer`](deluxe_core::ToContainer) trait. Other container types besides references and
///   [`Option`] can be supported by providing additional implementations for that trait.
#[proc_macro_derive(ParseAttributes, attributes(deluxe))]
pub fn derive_parse_attributes(item: TokenStream) -> TokenStream {
    let errors = Errors::new();
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
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
///   Can only be used on a struct with a single parseable field. Causes the struct to parse as if
///   it were the inner field. The field cannot be [`flatten`](#deluxeflatten-1),
///   [`append`](#deluxeappend) or [`rest`](#deluxerest). The struct can still contain
///   fields that are [`skip`](#deluxeskip), as those will be ignored by `transparent`.
///
/// - ##### `#[deluxe(allow_unknown_fields)]`
///
///   Ignore any tokens and do not throw an error when an unknown field is encountered.
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
/// - ##### `#[deluxe(allow_unknown_fields)]`
///
///   Ignore any tokens and do not throw an error when an unknown field is encountered in this
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
///   on to the previous value.
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
/// - ##### `#[deluxe(with = module)]`
///
///   When parsing, call functions from the path `module` instead of attempting to call
///   [`ParseMetaItem`](deluxe_core::ParseMetaItem) functions. The path can be a module path or a
///   path to a type containing associated functions.
///
///   The functions will be called as `module::parse_meta_item`, `module::parse_meta_item_inline`,
///   and `modue::parse_meta_item_flag`. All three functions must be implemented, even if just to
///   return an error. The signatures of these functions should match the equivalent functions in
///   [`ParseMetaItem`](crate::ParseMetaItem), although they can be generic over the return type.
///   Fields using this attribute are not required to implement
///   [`ParseMetaItem`](crate::ParseMetaItem).
///
///   `parse_meta_item_flag` implementations can call
///   [`flag_disallowed_error`](deluxe_core::parse_helpers::flag_disallowed_error) for a standard
///   error if flags are not supported by the target type.
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
    let mut tokens = util::parse::<syn::DeriveInput>(item.into(), &errors)
        .map(|input| parse_meta_item::impl_parse_meta_item(input, &errors))
        .unwrap_or_default();
    tokens.extend(errors.into_compile_errors());
    tokens.into()
}

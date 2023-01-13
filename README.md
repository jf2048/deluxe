# Deluxe &emsp;  [![Latest Version]][crates.io] [![Documentation]][docs] [![Build Status]][actions]

[Documentation]: https://docs.rs/deluxe/badge.svg
[docs]: https://docs.rs/deluxe
[Build Status]: https://github.com/jf2048/deluxe/workflows/ci/badge.svg?branch=main
[actions]: https://github.com/jf2048/deluxe/actions?query=branch%3Amain
[Latest Version]: https://img.shields.io/crates/v/deluxe.svg
[crates.io]: https://crates.io/crates/deluxe

A Rust procedural macro attribute parser.

### Abstract

This crate offers attribute parsing closer to the design of attributes in C#. It has an
interface similar to [serde](https://serde.rs). Attributes are written as plain Rust structs or
enums, and then parsers for them are generated automatically. They can contain arbitrary
expressions and can inherit from other attributes using a flattening mechanism.

The parsers in this crate directly parse token streams using
[`syn`](https://docs.rs/syn). As a result, most built-in Rust types and `syn`
types can be used directly as fields.

### Details

Functionality in this crate is centered around three traits, and their respective derive macros:

- **[`ExtractAttributes`]**

  Extracts attributes from an object containing a list of `syn::Attribute`, and parses them
  into a Rust type. Should be implemented for top-level structures that will be parsed directly
  out of a set of matching attributes.
- **[`ParseAttributes`]**

  Parses a Rust type from any object containing a list of `syn::Attribute`. Should be used if
  the set of matching attributes can potentially be shared between this type and other types.
- **[`ParseMetaItem`]**

  Parses a Rust type from a `syn::parse::ParseStream`. Should be implemented for
  any types that can be nested inside an attribute.

Basic usage of this crate requires simply deriving one (or a few) of these traits, and then
calling [`extract_attributes`] or [`parse_attributes`]. For more advanced functionality,
several `#[deluxe(...)]` attributes are supported on structs, enums, variants
and fields. See the examples below, and the documentation for each derive macro
for a complete description of the supported attributes.

A list of field types supported by default can be seen in the list of provided
[`ParseMetaItem`
implementations](https://docs.rs/deluxe-core/latest/deluxe_core/trait.ParseMetaItem.html#foreign-impls).
For more complex usage, manual implementations of these traits can be provided.
See the documentation on the individual traits for more details on how to
manually implement your own parsers.

### Related Crates

Deluxe takes inspiration from the [darling](https://docs.rs/darling) crate, but
offers a few enhancements over it. Darling is built around pre-parsed
`syn::Meta` objects, and therefore is restricted to the [meta
syntax](https://doc.rust-lang.org/stable/reference/attributes.html#meta-item-attribute-syntax).
Deluxe parses its types directly from `TokenStream` objects in the attributes
and so is able to use any syntax that parses as a valid token tree. Deluxe also
does not provide extra traits for parsing special `syn` objects like
`DeriveInput` and `Field`. Instead, Deluxe uses a generic trait to parse from
any type containing a `Vec<syn::Attribute>`.

### Examples

#### Basic

Inside your procedural macro, the [`ExtractAttributes`] trait can be derived to
extract a struct from a named attribute:

```rust
#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_desc))] // match only `my_desc` attributes
struct MyDescription {
    name: String,
    version: String,
}

#[proc_macro_derive(MyDescription, attributes(my_desc))]
pub fn derive_my_description(item: TokenStream) -> TokenStream {
    let mut input = syn::parse::<syn::DeriveInput>(item).unwrap();

    // Extract a description, modifying `input.attrs` to remove the matched attributes.
    let MyDescription { name, version } = match deluxe::extract_attributes(&mut input) {
        Ok(desc) => desc,
        Err(e) => return e.into_compile_error().into()
    };

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let tokens = quote::quote! {
        impl #impl_generics #ident #type_generics #where_clause {
            fn my_desc() -> &'static str {
                concat!("Name: ", #name, ", Version: ", #version)
            }
        }
    };
    tokens.into()
}
```

Then, try adding the attribute in some code that uses your macro:

```rust
#[derive(MyDescription)]
#[my_desc(name = "hello world", version = "0.2")]
struct Hello(String);

let hello = Hello("Moon".into());
assert_eq!(hello.my_desc(), "Name: hello world, Version: 0.2");
```

#### Field Attributes

The attributes `alias`, `default`, `rename`, and `skip` are supported, and
behave the same as in Serde. The `append` attribute can be used
on `Vec` fields to aggregate all duplicates of a key. The `rest` attribute can
be used to do custom processing on any unknown keys.

```rust
#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_object))]
struct MyObject {
    // can be specified with key `id` or `object_id`
    #[deluxe(alias = object_id)]
    id: u64,

    // field is optional, defaults to `Default::default` if not present
    #[deluxe(default)]
    count: u64,

    // defaults to "Empty" if not present
    #[deluxe(default = String::from("Empty"))]
    contents: String,

    // can be specified only with key `name`
    #[deluxe(rename = name)]
    s: String,

    // skipped during parsing entirely
    #[deluxe(skip)]
    internal_flag: bool,

    // appends any extra fields with the key `expr` to the Vec
    #[deluxe(append, rename = expr)]
    exprs: Vec<syn::Expr>,

    // adds any unknown keys to the hash map
    #[deluxe(rest)]
    rest: std::collections::HashMap<syn::Path, syn::Expr>,
}
```

```rust
// omitted fields will be set to defaults
#[derive(MyObject)]
#[my_object(id = 1, name = "First", expr = 1 + 2, count = 3)]
struct FirstObject;

// `expr` can be specified multiple times because of the `append` attribute
#[derive(MyObject)]
#[my_object(object_id = 2, name = "Second", expr = 1 + 2, expr = 3 + 4)]
struct SecondObject;

// `unknown` and `extra` will be stored in the `rest` hashmap
#[derive(MyObject)]
#[my_object(id = 3, name = "Third", unknown = 1 + 2, extra = 3 + 4)]
struct ThirdObject;
```

#### Inheritance

The `flatten` attribute can be used to parse keys from one structure inside another:

```rust
#[derive(deluxe::ParseMetaItem)]
struct A {
    id: u64,
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(b))]
struct B {
    #[deluxe(flatten)]
    a: A,
    name: String,
}
```

Then, fields from both `A` and `B` can be used when deriving `B`:

```rust
#[derive(B)]
#[b(id = 123, name = "object")]
struct Object;
```

#### Tuple Structs, Tuples and Vecs

Deluxe also supports parsing into data structures with unnamed fields.

```rust
#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_tuple))]
struct MyTuple(u64, String);

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_idents))]
struct MyIdents {
    id: u64,
    names: (String, String),
    idents: Vec<syn::Ident>
}
```

The standard attribute syntax with parenthesis can be used when specifying a
`Vec` type. The alternative syntax `key = [...]` can also be used to have an
appearance similar to an array literal.

```rust
#[derive(MyTuple)]
#[my_tuple(123, "object")]
struct Object;

#[derive(MyIdents)]
#[my_idents(id = 7, names("hello", "world"), idents(a, b, c))]
struct ABC;

// `idents` contains same values as above
#[derive(MyIdents)]
#[my_idents(id = 7, names("hello", "world"), idents = [a, b, c])]
struct ABC2;
```

#### C#-styled Attributes

Attributes in C# can support positional arguments first with the named
arguments afterwards. This style can be emulated by using a tuple struct with a
normal struct flattened at the end. Placing `#[deluxe(default)]` on the struct
behaves the same as Serde, by filling in all fields with values from `Default`,
allowing every named argument to be optional.

```rust
#[derive(deluxe::ParseMetaItem, Default)]
#[deluxe(default)]
struct Flags {
    native: bool,
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(a))]
struct A(u64, String, #[deluxe(flatten)] Flags);
```

```rust
#[derive(A)]
#[a(123, "object")]
struct Object;

#[derive(A)]
#[a(123, "native-object", native = true)]
struct NativeObject;
```

#### Enums

Enums are supported by using the variant name as a single key, in snake-case.
Variants can be renamed, aliased and skipped in the same way as fields.

```rust
#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_enum))]
enum MyEnum {
    A,
    B,
    C,
    #[deluxe(alias = d)]
    AnotherOne,
    #[deluxe(rename = e)]
    AnotherTwo,
    #[deluxe(skip)]
    SkipMe
}
```

```rust
#[derive(MyEnum)]
#[my_enum(b)]
struct ObjectB;

#[derive(MyEnum)]
#[my_enum(another_one)]
struct ObjectD;
```

#### Complex Enums

Enums with struct and tuple variants are also supported. The data inside is
used as arguments to the attribute. All field attributes from structs are also
supported inside variants.

Additionally, enum variants with named fields can be flattened. The behavior of
a flattened variant is similar to Serde's `untagged` mode. In a flattened
variant, the name of the variant will be ignored. Instead, Deluxe will attempt
to use the unique keys in each variant to determine if that variant was
specified. A compile error will be thrown if it is not possible to determine a
unique, unambiguous key between two variants.

```rust
#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(my_enum))]
enum MyEnum {
    A,
    B(u64, String),
    C { id: u64, name: String },
    #[deluxe(flatten)]
    D { d: u64, name: String },
}
```

```rust
#[derive(MyEnum)]
#[my_enum(a)]
struct ObjectA;

#[derive(MyEnum)]
#[my_enum(b(1, "hello"))]
struct ObjectB;

#[derive(MyEnum)]
#[my_enum(c(id = 2, name = "world"))]
struct ObjectC;

// no inner parenthesis needed here due to flattening
#[derive(MyEnum)]
#[my_enum(d = 3, name = "moon")]
struct ObjectD;
```

#### Storing Containers

During parsing, Deluxe can store references to the container type holding the attributes
for easier access. Container fields are skipped during attribute parsing.

```rust
#[derive(deluxe::ParseAttributes)]
#[deluxe(attributes(my_object))]
struct MyObject<'t> {
    id: u64,
    // Fill `container` in using the parsed type. Note this restricts the
    // derived `ParseAttributes` impl so it can only be used on `DeriveInput`.
    #[deluxe(container)]
    container: &'t syn::DeriveInput,
}

#[proc_macro_derive(MyObject, attributes(my_desc))]
pub fn derive_my_object(item: TokenStream) -> TokenStream {
    let input = syn::parse::<syn::DeriveInput>(item).unwrap();

    // `obj.container` now holds a reference to `input`
    let obj: MyObject = match deluxe::parse_attributes(&input) {
        Ok(obj) => obj,
        Err(e) => return e.into_compile_error().into()
    };

    let tokens = quote::quote! { /* ... generate some code here ... */ };

    tokens.into()
}
```

To support both extracting and parsing, a container field can also be a value
type. In that case, the container will be cloned into the structure.

[`ParseMetaItem`]: (https://docs.rs/deluxe/latest/deluxe/derive.ParseMetaItem.html)
[`ParseAttributes`]: (https://docs.rs/deluxe/latest/deluxe/derive.ParseAttributes.html)
[`ExtractAttributes`]: (https://docs.rs/deluxe/latest/deluxe/derive.ExtractAttributes.html)
[`parse_attributes`]: (https://docs.rs/deluxe/latest/deluxe/fn.parse_attributes.html)
[`extract_attributes`]: (https://docs.rs/deluxe/latest/deluxe/fn.extract_attributes.html)

#![no_implicit_prelude]

mod test_util;
use test_util::*;

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnit;

#[test]
fn parse_unit() {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyUnit as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };

    ::std::assert_eq!(parser.parse_str("()").unwrap(), MyUnit);
    ::std::assert_eq!(
        parser.parse_str("(1)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnnamedEmpty();

#[test]
fn parse_unnamed_empty() {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyUnnamedEmpty as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };

    ::std::assert_eq!(parser.parse_str("()").unwrap(), MyUnnamedEmpty());
    ::std::assert_eq!(
        parser.parse_str("(1)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNewtype(::std::string::String);

#[test]
fn parse_newtype() {
    use ::std::prelude::v1::*;
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyNewtype as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };

    ::std::assert_eq!(parser.parse_str("\"qwerty\"").unwrap(), MyNewtype("qwerty".into()));
    ::std::assert_eq!(
        parser.parse_str("123").unwrap_err().to_multi_string(),
        "expected string literal"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnnamed(i32, ::std::string::String);

#[test]
fn parse_unnamed() {
    use ::std::prelude::v1::*;
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyUnnamed as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };

    ::std::assert_eq!(parser.parse_str("(123, \"abc\")").unwrap(), MyUnnamed(123, "abc".into()));
    ::std::assert_eq!(
        parser.parse_str("(123)").unwrap_err().to_multi_string(),
        "missing required field 1"
    );
    ::std::assert_eq!(
        parser.parse_str("(123, \"abc\", true)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamedEmpty {}

#[test]
fn parse_named_empty() {
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyNamedEmpty as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };

    ::std::assert_eq!(parser.parse_str("{}").unwrap(), MyNamedEmpty {});
    ::std::assert_eq!(
        parser.parse_str("{a = 123}").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamed {
    a: i32,
    b: ::std::string::String,
}

#[test]
fn parse_named() {
    use ::std::prelude::v1::*;
    use ::syn::parse::Parser;
    let parser = |stream: ::syn::parse::ParseStream<'_>| {
        <MyNamed as ::deluxe::ParseMetaItem>::parse_meta_item(stream, ::deluxe::ParseMode::Unnamed)
    };
    let t = MyNamed {
        a: 123,
        b: "asdf".into(),
    };
    ::std::assert_eq!(parser.parse_str("{a = 123, b = \"asdf\"}").unwrap(), t);
    ::std::assert_eq!(parser.parse_str("{a(123), b = \"asdf\"}").unwrap(), t);
    ::std::assert_eq!(parser.parse_str("{a = 123, b(\"asdf\")}").unwrap(), t);
    ::std::assert_eq!(parser.parse_str("{a(123), b(\"asdf\")}").unwrap(), t);

    ::std::assert_eq!(
        parser.parse_str("{}").unwrap_err().to_multi_string(),
        "missing required field `a`, missing required field `b`"
    );
    ::std::assert_eq!(
        parser.parse_str("{b(\"asdf\")}").unwrap_err().to_multi_string(),
        "missing required field `a`"
    );
    ::std::assert_eq!(
        parser.parse_str("{a(123)}").unwrap_err().to_multi_string(),
        "missing required field `b`"
    );
    ::std::assert_eq!(
        parser.parse_str("{a(123), b(\"asdf\"), c(true)}").unwrap_err().to_multi_string(),
        "unknown field `c`"
    );
    ::std::assert_eq!(
        parser.parse_str("{a(123), b(\"asdf\"), a(456)}").unwrap_err().to_multi_string(),
        "duplicate attribute for `a`"
    );
}

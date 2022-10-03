#![no_implicit_prelude]

use ::quote::quote as q;

mod test_util;
use test_util::*;

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyUnit;

#[test]
fn parse_unit() {
    let parse = parse_meta::<MyUnit>;
    ::std::assert_eq!(parse(q! { () }).unwrap(), MyUnit);
    ::std::assert_eq!(
        parse(q! { (1) }).unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyUnnamedEmpty();

#[test]
fn parse_unnamed_empty() {
    let parse = parse_meta::<MyUnnamedEmpty>;
    ::std::assert_eq!(parse(q! { () }).unwrap(), MyUnnamedEmpty());
    ::std::assert_eq!(
        parse(q! { (1) }).unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent)]
struct MyNewtype(::std::string::String);

#[test]
fn parse_newtype() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyNewtype>;

    ::std::assert_eq!(parse(q! { "qwerty" }).unwrap(), MyNewtype("qwerty".into()));
    ::std::assert_eq!(
        parse(q! { 123 }).unwrap_err().to_multi_string(),
        "expected string literal"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyUnnamed(i32, ::std::string::String);

#[test]
fn parse_unnamed() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyUnnamed>;

    ::std::assert_eq!(
        parse(q! { (123, "abc") }).unwrap(),
        MyUnnamed(123, "abc".into())
    );
    ::std::assert_eq!(
        parse(q! { (123) }).unwrap_err().to_multi_string(),
        "missing required field 1"
    );
    ::std::assert_eq!(
        parse(q! { (123, "abc", true) })
            .unwrap_err()
            .to_multi_string(),
        "unexpected token"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamedEmpty {}

#[test]
fn parse_named_empty() {
    let parse = parse_meta::<MyNamedEmpty>;

    ::std::assert_eq!(parse(q! { {} }).unwrap(), MyNamedEmpty {});
    ::std::assert_eq!(
        parse(q! { {a = 123} }).unwrap_err().to_multi_string(),
        "unknown field `a`"
    );

    ::std::assert_eq!(parse_flag::<MyNamedEmpty>().unwrap(), MyNamedEmpty {});
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamed {
    a: i32,
    b: ::std::string::String,
}

#[test]
fn parse_named() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyNamed>;

    let t = MyNamed {
        a: 123,
        b: "asdf".into(),
    };
    ::std::assert_eq!(parse(q! { {a = 123, b = "asdf"} }).unwrap(), t);
    ::std::assert_eq!(parse(q! { {a(123), b = "asdf"} }).unwrap(), t);
    ::std::assert_eq!(parse(q! { {a = 123, b("asdf")} }).unwrap(), t);
    ::std::assert_eq!(parse(q! { {a(123), b("asdf")} }).unwrap(), t);

    ::std::assert_eq!(
        parse(q! { {} }).unwrap_err().to_multi_string(),
        "missing required field `a`, missing required field `b`"
    );
    ::std::assert_eq!(
        parse(q! { {b("asdf")} }).unwrap_err().to_multi_string(),
        "missing required field `a`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123)} }).unwrap_err().to_multi_string(),
        "missing required field `b`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123), b("asdf"), c(true)} })
            .unwrap_err()
            .to_multi_string(),
        "unknown field `c`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123), b("asdf"), a(456)} })
            .unwrap_err()
            .to_multi_string(),
        "duplicate attribute for `a`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamedChild {
    c: i32,
    #[deluxe(flatten)]
    d: MyNamed,
}

#[test]
fn parse_named_flat() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyNamedChild>;

    let t = MyNamedChild {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
    };
    ::std::assert_eq!(parse(q! { {a = 100, b = "qwerty", c = 900} }).unwrap(), t);
    ::std::assert_eq!(
        parse(q! { {} }).unwrap_err().to_multi_string(),
        "missing required field `a`, missing required field `b`, missing required field `c`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamedChildPrefixed {
    c: i32,
    #[deluxe(flatten(prefix = d))]
    d: MyNamed,
}

#[test]
fn parse_named_flat_prefixed() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyNamedChildPrefixed>;

    let t = MyNamedChildPrefixed {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
    };
    ::std::assert_eq!(
        parse(q! { {d::a = 100, d::b = "qwerty", c = 900} }).unwrap(),
        t
    );
    ::std::assert_eq!(
        parse(q! { {} }).unwrap_err().to_multi_string(),
        "missing required field `d::a`, missing required field `d::b`, missing required field `c`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamedChildLongPrefixed {
    c: i32,
    #[deluxe(flatten(prefix = d::e))]
    d: MyNamed,
    #[deluxe(flatten(prefix = d::e2))]
    d2: MyNamed,
}

#[test]
fn parse_named_flat_long_prefixed() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyNamedChildLongPrefixed>;

    let t = MyNamedChildLongPrefixed {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
        d2: MyNamed {
            a: 200,
            b: "uiop".into(),
        },
    };
    ::std::assert_eq!(
        parse(
            q! { {d::e::a = 100, d::e::b = "qwerty", d::e2::b = "uiop", d::e2::a = 200, c = 900} }
        )
        .unwrap(),
        t
    );
    ::std::assert_eq!(
        parse(q! { {} }).unwrap_err().to_multi_string(),
        "missing required field `d::e::a`, missing required field `d::e::b`, \
        missing required field `d::e2::a`, missing required field `d::e2::b`, \
        missing required field `c`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyNamedComplex {
    paths: ::std::vec::Vec<::syn::Ident>,
    #[deluxe(skip)]
    skipped: bool,
    #[deluxe(default)]
    int: i32,
    #[deluxe(append)]
    exprs: ::std::vec::Vec<::syn::Expr>,
    #[deluxe(rest)]
    rest: ::std::collections::HashMap<::syn::Path, ::syn::Expr>,
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum MyEmptyEnum {}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyFlatNamed {
    j_a: i32,
    j_b: ::std::string::String,
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum MyEnum {
    A,
    B(),
    C(i32),
    #[deluxe(transparent)]
    D(bool),
    E(::std::string::String, f64),
    F {},
    #[deluxe(transparent)]
    G {
        pass: ::syn::Path,
    },
    H {
        s: ::std::string::String,
        f: f64,
    },
    #[deluxe(flatten)]
    FlatI {
        i: ::std::string::String,
    },
    #[deluxe(flatten)]
    FlatJ {
        #[deluxe(flatten)]
        named: MyFlatNamed,
    },
}

#[test]
fn parse_enum() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyEnum>;

    ::std::assert_eq!(parse(q! { { a } }).unwrap(), MyEnum::A);
    ::std::assert_eq!(parse(q! { { a() } }).unwrap(), MyEnum::A);
    ::std::assert_eq!(
        parse(q! { { a(x = 123) } }).unwrap_err().to_multi_string(),
        "unexpected token `x`"
    );
    ::std::assert_eq!(parse(q! { { b() } }).unwrap(), MyEnum::B());
    ::std::assert_eq!(parse(q! { { b = () } }).unwrap(), MyEnum::B());
    ::std::assert_eq!(
        parse(q! { { b(x = 123) } }).unwrap_err().to_multi_string(),
        "unexpected token `x`"
    );
    ::std::assert_eq!(parse(q! { { c(123) } }).unwrap(), MyEnum::C(123));
    ::std::assert_eq!(parse(q! { { d(true) } }).unwrap(), MyEnum::D(true));
    ::std::assert_eq!(
        parse(q! { { e("hello", 4.0) } }).unwrap(),
        MyEnum::E("hello".into(), 4.0),
    );
    ::std::assert_eq!(parse(q! { { f } }).unwrap(), MyEnum::F {});
    ::std::assert_eq!(parse(q! { { f() } }).unwrap(), MyEnum::F {});
    ::std::assert_eq!(parse(q! { { f = {} } }).unwrap(), MyEnum::F {});
    /*::std::assert_eq!(
        parse(q! { { g(themod::theitem) } }).unwrap(),
        MyEnum::G {
            pass: ::syn::parse_quote! { themod::theitem }
        }
    );*/
    ::std::assert_eq!(
        parse(q! { { h(s = "qwerty", f = 1.0) } }).unwrap(),
        MyEnum::H {
            s: "qwerty".into(),
            f: 1.0
        }
    );
    ::std::assert_eq!(
        parse(q! { { j_a = 123, j_b = "asdf" } }).unwrap(),
        MyEnum::FlatJ {
            named: MyFlatNamed {
                j_a: 123,
                j_b: "asdf".into(),
            }
        },
    );
    ::std::assert_eq!(
        parse(q! { { i = "asdf" } }).unwrap(),
        MyEnum::FlatI { i: "asdf".into() }
    );
    /*
    ::std::assert_eq!(
        parse(q! { { x() } }).unwrap_err().to_multi_string(),
        "unknown field `x`",
    );
    */
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum MySimpleEnum {
    A,
    B,
    C,
}

#[test]
fn parse_simple_enum() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MySimpleEnum>;

    ::std::assert_eq!(parse(q! { { a } }).unwrap(), MySimpleEnum::A);
    ::std::assert_eq!(parse(q! { { b } }).unwrap(), MySimpleEnum::B);
    ::std::assert_eq!(parse(q! { { c } }).unwrap(), MySimpleEnum::C);
    ::std::assert_eq!(
        parse(q! { { d } }).unwrap_err().to_multi_string(),
        "unknown field `d`, expected one of `a`, `b`, `c`"
    );
}

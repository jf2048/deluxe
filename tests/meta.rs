#![no_implicit_prelude]

mod test_util;
use test_util::*;

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnit;

#[test]
fn parse_unit() {
    let parse = parse_str::<MyUnit>;
    ::std::assert_eq!(parse("()").unwrap(), MyUnit);
    ::std::assert_eq!(
        parse("(1)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnnamedEmpty();

#[test]
fn parse_unnamed_empty() {
    let parse = parse_str::<MyUnnamedEmpty>;
    ::std::assert_eq!(parse("()").unwrap(), MyUnnamedEmpty());
    ::std::assert_eq!(
        parse("(1)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
#[deluxe(transparent)]
struct MyNewtype(::std::string::String);

#[test]
fn parse_newtype() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyNewtype>;

    ::std::assert_eq!(parse("\"qwerty\"").unwrap(), MyNewtype("qwerty".into()));
    ::std::assert_eq!(
        parse("123").unwrap_err().to_multi_string(),
        "expected string literal"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyUnnamed(i32, ::std::string::String);

#[test]
fn parse_unnamed() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyUnnamed>;

    ::std::assert_eq!(
        parse("(123, \"abc\")").unwrap(),
        MyUnnamed(123, "abc".into())
    );
    ::std::assert_eq!(
        parse("(123)").unwrap_err().to_multi_string(),
        "missing required field 1"
    );
    ::std::assert_eq!(
        parse("(123, \"abc\", true)").unwrap_err().to_multi_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamedEmpty {}

#[test]
fn parse_named_empty() {
    let parse = parse_str::<MyNamedEmpty>;

    ::std::assert_eq!(parse("{}").unwrap(), MyNamedEmpty {});
    ::std::assert_eq!(
        parse("{a = 123}").unwrap_err().to_multi_string(),
        "unknown field `a`"
    );

    ::std::assert_eq!(parse_flag::<MyNamedEmpty>().unwrap(), MyNamedEmpty {});
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamed {
    a: i32,
    b: ::std::string::String,
}

#[test]
fn parse_named() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyNamed>;

    let t = MyNamed {
        a: 123,
        b: "asdf".into(),
    };
    ::std::assert_eq!(parse("{a = 123, b = \"asdf\"}").unwrap(), t);
    ::std::assert_eq!(parse("{a(123), b = \"asdf\"}").unwrap(), t);
    ::std::assert_eq!(parse("{a = 123, b(\"asdf\")}").unwrap(), t);
    ::std::assert_eq!(parse("{a(123), b(\"asdf\")}").unwrap(), t);

    ::std::assert_eq!(
        parse("{}").unwrap_err().to_multi_string(),
        "missing required field `a`, missing required field `b`"
    );
    ::std::assert_eq!(
        parse("{b(\"asdf\")}").unwrap_err().to_multi_string(),
        "missing required field `a`"
    );
    ::std::assert_eq!(
        parse("{a(123)}").unwrap_err().to_multi_string(),
        "missing required field `b`"
    );
    ::std::assert_eq!(
        parse("{a(123), b(\"asdf\"), c(true)}")
            .unwrap_err()
            .to_multi_string(),
        "unknown field `c`"
    );
    ::std::assert_eq!(
        parse("{a(123), b(\"asdf\"), a(456)}")
            .unwrap_err()
            .to_multi_string(),
        "duplicate attribute for `a`"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamedChild {
    c: i32,
    #[deluxe(flatten)]
    d: MyNamed,
}

#[test]
fn parse_named_flat() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyNamedChild>;

    let t = MyNamedChild {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
    };
    ::std::assert_eq!(parse("{a = 100, b = \"qwerty\", c = 900}").unwrap(), t);
    ::std::assert_eq!(
        parse("{}").unwrap_err().to_multi_string(),
        "missing required field `a`, missing required field `b`, missing required field `c`"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamedChildPrefixed {
    c: i32,
    #[deluxe(flatten(prefix = d))]
    d: MyNamed,
}

#[test]
fn parse_named_flat_prefixed() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyNamedChildPrefixed>;

    let t = MyNamedChildPrefixed {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
    };
    ::std::assert_eq!(
        parse("{d::a = 100, d::b = \"qwerty\", c = 900}").unwrap(),
        t
    );
    ::std::assert_eq!(
        parse("{}").unwrap_err().to_multi_string(),
        "missing required field `d::a`, missing required field `d::b`, missing required field `c`"
    );
}

#[derive(::deluxe::ParseMetaItem, PartialEq, Debug)]
struct MyNamedChildLongPrefixed {
    c: i32,
    #[deluxe(flatten(prefix = d::e))]
    d: MyNamed,
}

#[test]
fn parse_named_flat_long_prefixed() {
    use ::std::prelude::v1::*;
    let parse = parse_str::<MyNamedChildLongPrefixed>;

    let t = MyNamedChildLongPrefixed {
        c: 900,
        d: MyNamed {
            a: 100,
            b: "qwerty".into(),
        },
    };
    ::std::assert_eq!(
        parse("{d::e::a = 100, d::e::b = \"qwerty\", c = 900}").unwrap(),
        t
    );
    ::std::assert_eq!(
        parse("{}").unwrap_err().to_multi_string(),
        "missing required field `d::e::a`, missing required field `d::e::b`, missing required field `c`"
    );
}

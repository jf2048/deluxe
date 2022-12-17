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
        parse(q! { (1) }).unwrap_err_string(),
        "unexpected token `1`"
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
        parse(q! { (1) }).unwrap_err_string(),
        "unexpected token `1`"
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
        parse(q! { 123 }).unwrap_err_string(),
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
        parse(q! { (123) }).unwrap_err_string(),
        "missing required field 1"
    );
    ::std::assert_eq!(
        parse(q! { (123, "abc", true) }).unwrap_err_string(),
        "unexpected token `true`"
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
        parse(q! { {a = 123} }).unwrap_err_string(),
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
        parse(q! { {} }).unwrap_err_string(),
        "missing required field `a`, missing required field `b`"
    );
    ::std::assert_eq!(
        parse(q! { {b("asdf")} }).unwrap_err_string(),
        "missing required field `a`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123)} }).unwrap_err_string(),
        "missing required field `b`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123), b("asdf"), c(true)} }).unwrap_err_string(),
        "unknown field `c`"
    );
    ::std::assert_eq!(
        parse(q! { {a(123), b("asdf"), a(456)} }).unwrap_err_string(),
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
        parse(q! { {} }).unwrap_err_string(),
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
        parse(q! { {} }).unwrap_err_string(),
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
        parse(q! { {} }).unwrap_err_string(),
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
    idents: ::std::vec::Vec<::syn::Ident>,
    #[deluxe(skip)]
    skipped: bool,
    #[deluxe(append)]
    exprs: ::std::vec::Vec<::syn::Expr>,
    #[deluxe(rest)]
    rest: ::std::collections::HashMap<::syn::Path, ::syn::Expr>,
}

#[test]
fn vec_field() {
    use ::std::prelude::v1::*;
    fn cs() -> ::proc_macro2::Span {
        ::proc_macro2::Span::call_site()
    }
    let parse = parse_meta::<MyNamedComplex>;

    ::std::assert_eq!(
        parse(q! { { } }).unwrap_err_string(),
        "missing required field `idents`"
    );
    ::std::assert_eq!(
        parse(q! { { idents } }).unwrap_err_string(),
        "unexpected flag, expected `=` or parentheses"
    );
    ::std::assert_eq!(
        parse(q! { { idents = [] } }).unwrap().idents,
        &[] as &[::syn::Ident]
    );
    ::std::assert_eq!(
        parse(q! { { idents() } }).unwrap().idents,
        &[] as &[::syn::Ident]
    );
    let hello_world = &[
        ::syn::Ident::new("hello", cs()),
        ::syn::Ident::new("world", cs()),
    ];
    ::std::assert_eq!(
        parse(q! { { idents = [hello, world] } }).unwrap().idents,
        hello_world,
    );
    ::std::assert_eq!(
        parse(q! { { idents = [hello, world,] } }).unwrap().idents,
        hello_world,
    );
    ::std::assert_eq!(
        parse(q! { { idents(hello, world) } }).unwrap().idents,
        hello_world,
    );
    ::std::assert_eq!(
        parse(q! { { idents(hello, world,) } }).unwrap().idents,
        hello_world,
    );
    ::std::assert_eq!(
        parse(q! { { idents = (hello, world) } }).unwrap_err_string(),
        "expected square brackets"
    );
    ::std::assert_eq!(
        parse(q! { { idents = [hello world] } }).unwrap_err_string(),
        "expected `,`"
    );
}

#[test]
fn skipped_field() {
    let parse = parse_meta::<MyNamedComplex>;
    ::std::assert_eq!(parse(q! { { idents = [] } }).unwrap().skipped, false);
    ::std::assert_eq!(
        parse(q! { { idents = [], skipped = true } }).unwrap_err_string(),
        "unknown field `skipped`"
    );
}

impl MyNamedComplex {
    #[inline]
    fn exprs_str(&self) -> ::std::string::String {
        use ::quote::ToTokens;
        use ::std::prelude::v1::*;
        self.exprs
            .iter()
            .map(|e| ::std::format!("({})", e.to_token_stream()))
            .collect::<Vec<_>>()
            .join(", ")
    }
    #[inline]
    fn rest_str(&self) -> ::std::string::String {
        use ::quote::ToTokens;
        use ::std::prelude::v1::*;
        self.rest
            .iter()
            .map(|(k, v)| ::std::format!("({} => {})", k.to_token_stream(), v.to_token_stream()))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[test]
fn struct_append() {
    let parse = parse_meta::<MyNamedComplex>;
    ::std::assert_eq!(parse(q! { { idents = [] } }).unwrap().exprs, []);
    ::std::assert_eq!(
        parse(q! { { idents = [], exprs = {} } })
            .unwrap()
            .exprs_str(),
        "({})",
    );
    ::std::assert_eq!(
        parse(q! { { idents = [], exprs = {}, exprs = 123 + 4 } })
            .unwrap()
            .exprs_str(),
        "({}), (123 + 4)",
    );
    ::std::assert_eq!(
        parse(q! { { idents = [], exprs = ! } }).unwrap_err_string(),
        "expected token"
    );
}

#[test]
fn struct_rest() {
    let parse = parse_meta::<MyNamedComplex>;
    ::std::assert_eq!(parse(q! { { idents = [] } }).unwrap().exprs, []);
    ::std::assert_eq!(
        parse(q! { { idents = [], abcd = 123 + 4 } })
            .unwrap()
            .rest_str(),
        "(abcd => 123 + 4)",
    );
    ::std::assert_eq!(
        parse(q! { { idents = [], abcd = 123 + 4, hello::world = { "str" } } })
            .unwrap()
            .rest_str(),
        r#"(abcd => 123 + 4), (hello :: world => { "str" })"#,
    );
    ::std::assert_eq!(
        parse(q! { { idents = [], abcd = 123 + 4, hello::world = { "str" }, exprs = 126 - 5 } })
            .unwrap()
            .rest_str(),
        r#"(abcd => 123 + 4), (hello :: world => { "str" })"#,
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct MyUnnamedRest(i32, #[deluxe(flatten)] ::std::vec::Vec<i32>);

#[test]
fn tuple_struct_rest() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<MyUnnamedRest>;
    ::std::assert_eq!(
        parse(q! { () }).unwrap_err_string(),
        "missing required field 0"
    );
    ::std::assert_eq!(parse(q! { (1) }).unwrap(), MyUnnamedRest(1, Vec::new()));
    ::std::assert_eq!(
        parse(q! { (1, 2) }).unwrap(),
        MyUnnamedRest(1, ::std::vec![2])
    );
    ::std::assert_eq!(
        parse(q! { (1, 2,) }).unwrap(),
        MyUnnamedRest(1, ::std::vec![2])
    );
    ::std::assert_eq!(
        parse(q! { (1, 2, 3, 4) }).unwrap(),
        MyUnnamedRest(1, ::std::vec![2, 3, 4])
    );
    ::std::assert_eq!(
        parse(q! { (1, 2, 3, 4, "hello", 6) }).unwrap_err_string(),
        "expected integer literal"
    );
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

    ::std::assert_eq!(
        parse(q! { ( a ) }).unwrap_err_string(),
        "expected curly braces"
    );
    ::std::assert_eq!(parse(q! { { a } }).unwrap(), MyEnum::A);
    ::std::assert_eq!(parse(q! { { a() } }).unwrap(), MyEnum::A);
    ::std::assert_eq!(
        parse(q! { { a(x = 123) } }).unwrap_err_string(),
        "unexpected token `x`"
    );
    ::std::assert_eq!(parse(q! { { b() } }).unwrap(), MyEnum::B());
    ::std::assert_eq!(parse(q! { { b = () } }).unwrap(), MyEnum::B());
    ::std::assert_eq!(
        parse(q! { { b(x = 123) } }).unwrap_err_string(),
        "unexpected token `x`"
    );
    ::std::assert_eq!(parse(q! { { c(123) } }).unwrap(), MyEnum::C(123));
    ::std::assert_eq!(
        parse(q! { { c } }).unwrap_err_string(),
        "unexpected flag, expected `=` or parentheses"
    );
    ::std::assert_eq!(
        parse(q! { { c = 123 } }).unwrap_err_string(),
        "expected parentheses"
    );
    ::std::assert_eq!(parse(q! { { d(true) } }).unwrap(), MyEnum::D(true));
    ::std::assert_eq!(parse(q! { { d = true } }).unwrap(), MyEnum::D(true));
    ::std::assert_eq!(parse(q! { { d } }).unwrap(), MyEnum::D(true));
    ::std::assert_eq!(
        parse(q! { { e("hello", 4.0) } }).unwrap(),
        MyEnum::E("hello".into(), 4.0),
    );
    ::std::assert_eq!(parse(q! { { f } }).unwrap(), MyEnum::F {});
    ::std::assert_eq!(parse(q! { { f() } }).unwrap(), MyEnum::F {});
    ::std::assert_eq!(parse(q! { { f = {} } }).unwrap(), MyEnum::F {});
    ::std::assert_eq!(
        parse(q! { { g(themod::theitem) } }).unwrap(),
        MyEnum::G {
            pass: ::syn::parse_quote! { themod::theitem }
        }
    );
    ::std::assert_eq!(
        parse(q! { { g = themod::theitem } }).unwrap(),
        MyEnum::G {
            pass: ::syn::parse_quote! { themod::theitem }
        }
    );
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
    ::std::assert_eq!(
        parse(q! { { x() } }).unwrap_err_string(),
        "unknown field `x`, expected one of `a`, `b`, `c`, `d`, `e`, `f`, `g`, `h`, `i`, fields from `FlatJ`",
    );
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
        parse(q! { { d } }).unwrap_err_string(),
        "unknown field `d`, expected one of `a`, `b`, `c`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct StructWith {
    normal: ::std::string::String,
    #[deluxe(with = ::deluxe::with::from_str)]
    str_int: i32,
}

#[test]
fn parse_with() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<StructWith>;

    ::std::assert_eq!(
        parse(q! { { normal = "abc", str_int = "123" } }).unwrap(),
        StructWith {
            normal: "abc".into(),
            str_int: 123
        }
    );
    ::std::assert_eq!(
        parse(q! { { normal = "abc", str_int = 123 } }).unwrap_err_string(),
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
struct StructNames {
    #[deluxe(rename = sixty_four, default)]
    renamed: u64,
    #[deluxe(alias = thirty_two, default)]
    aliased: u32,
    #[deluxe(alias = another, alias = another2, default)]
    many_aliases: u32,
    #[deluxe(rename = sixteen, alias = another_sixteen, alias = another2_sixteen, default)]
    alias_renamed: u16,
}

#[test]
fn struct_field_names() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<StructNames>;

    ::std::assert_eq!(parse(q! { { sixty_four = 64 } }).unwrap().renamed, 64);
    ::std::assert_eq!(
        parse(q! { { renamed = 64 } }).unwrap_err_string(),
        "unknown field `renamed`"
    );
    ::std::assert_eq!(
        parse(q! { { sixty_four = 64, sixty_four = 65 } }).unwrap_err_string(),
        "duplicate attribute for `sixty_four`"
    );
    ::std::assert_eq!(
        parse(q! { { sixty_four = 64, renamed = 65 } }).unwrap_err_string(),
        "unknown field `renamed`"
    );

    ::std::assert_eq!(parse(q! { { aliased = 32 } }).unwrap().aliased, 32);
    ::std::assert_eq!(parse(q! { { thirty_two = 32 } }).unwrap().aliased, 32);
    ::std::assert_eq!(
        parse(q! { { aliased = 32, thirty_two = 33 } }).unwrap_err_string(),
        "duplicate attribute for `aliased`"
    );

    ::std::assert_eq!(
        parse(q! { { many_aliases = 32 } }).unwrap().many_aliases,
        32
    );
    ::std::assert_eq!(parse(q! { { another = 32 } }).unwrap().many_aliases, 32);
    ::std::assert_eq!(parse(q! { { another2 = 32 } }).unwrap().many_aliases, 32);
    ::std::assert_eq!(
        parse(q! { { many_aliases = 32, another = 33, another2 = 34 } }).unwrap_err_string(),
        "duplicate attribute for `many_aliases`, duplicate attribute for `many_aliases`"
    );

    ::std::assert_eq!(parse(q! { { sixteen = 16 } }).unwrap().alias_renamed, 16);
    ::std::assert_eq!(
        parse(q! { { another_sixteen = 16 } })
            .unwrap()
            .alias_renamed,
        16
    );
    ::std::assert_eq!(
        parse(q! { { another2_sixteen = 16 } })
            .unwrap()
            .alias_renamed,
        16
    );
    ::std::assert_eq!(
        parse(q! { { alias_renamed = 16 } }).unwrap_err_string(),
        "unknown field `alias_renamed`, did you mean `aliased`?"
    );

    ::std::assert_eq!(
        <StructNames as ::deluxe::ParseMetaFlatNamed>::field_names(),
        &[
            "sixty_four",
            "aliased",
            "thirty_two",
            "many_aliases",
            "another",
            "another2",
            "sixteen",
            "another_sixteen",
            "another2_sixteen"
        ]
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum EnumNames {
    #[deluxe(rename = renamed_a)]
    A,
    #[deluxe(alias = alias_b)]
    B,
    #[deluxe(alias = another_c, alias = another2_c)]
    C,
    #[deluxe(rename = renamed_d, alias = another_d, alias = another2_d)]
    D,
}

#[test]
fn enum_field_names() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<EnumNames>;

    ::std::assert_eq!(parse(q! { { renamed_a } }).unwrap(), EnumNames::A);
    ::std::assert_eq!(parse(q! { { b } }).unwrap(), EnumNames::B);
    ::std::assert_eq!(parse(q! { { alias_b } }).unwrap(), EnumNames::B);
    ::std::assert_eq!(parse(q! { { c } }).unwrap(), EnumNames::C);
    ::std::assert_eq!(parse(q! { { another_c } }).unwrap(), EnumNames::C);
    ::std::assert_eq!(parse(q! { { another2_c } }).unwrap(), EnumNames::C);
    ::std::assert_eq!(parse(q! { { renamed_d } }).unwrap(), EnumNames::D);
    ::std::assert_eq!(parse(q! { { another_d } }).unwrap(), EnumNames::D);
    ::std::assert_eq!(parse(q! { { another2_d } }).unwrap(), EnumNames::D);
    ::std::assert_eq!(
        parse(q! { { a } }).unwrap_err_string(),
        "unknown field `a`, expected one of `renamed_a`, `b`, `c`, `renamed_d`"
    );
    ::std::assert_eq!(
        parse(q! { { d } }).unwrap_err_string(),
        "unknown field `d`, expected one of `renamed_a`, `b`, `c`, `renamed_d`"
    );

    ::std::assert_eq!(
        <EnumNames as ::deluxe::ParseMetaFlatNamed>::field_names(),
        &[
            "renamed_a",
            "b",
            "alias_b",
            "c",
            "another_c",
            "another2_c",
            "renamed_d",
            "another_d",
            "another2_d",
        ]
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
    Default,
)]
#[deluxe(default)]
struct StructDefault {
    value0: i32,
    #[deluxe(default = 1)]
    value1: i32,
}

#[test]
fn struct_defaults() {
    use ::std::prelude::v1::*;

    let parse = parse_meta::<StructDefault>;
    ::std::assert_eq!(
        parse(q! { { } }).unwrap(),
        StructDefault {
            value0: 0,
            value1: 1
        }
    );
    ::std::assert_eq!(
        parse(q! { { value0 = -1, value1 = 2 } }).unwrap(),
        StructDefault {
            value0: -1,
            value1: 2
        }
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(default = Self { value1: 1, value2: 2 })]
struct StructDefaultExpr {
    value1: i32,
    value2: i32,
}

#[test]
fn struct_default_expr() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<StructDefaultExpr>;
    ::std::assert_eq!(
        parse(q! { { } }).unwrap(),
        StructDefaultExpr {
            value1: 1,
            value2: 2
        }
    );
    ::std::assert_eq!(
        parse(q! { { value1 = 11 } }).unwrap(),
        StructDefaultExpr {
            value1: 11,
            value2: 2
        }
    );
    ::std::assert_eq!(
        parse(q! { { value2 = 22 } }).unwrap(),
        StructDefaultExpr {
            value1: 1,
            value2: 22
        }
    );
    ::std::assert_eq!(
        parse(q! { { value1 = 11, value2 = 22 } }).unwrap(),
        StructDefaultExpr {
            value1: 11,
            value2: 22
        }
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
    Default,
)]
struct StructDefaultTuple(i32, #[deluxe(default)] i32);

#[test]
fn struct_default_tuple() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<StructDefaultTuple>;
    ::std::assert_eq!(
        parse(q! {}).unwrap_err_string(),
        "unexpected end of input, expected parentheses"
    );
    ::std::assert_eq!(
        parse(q! { () }).unwrap_err_string(),
        "missing required field 0"
    );
    ::std::assert_eq!(parse(q! { (1) }).unwrap(), StructDefaultTuple(1, 0));
    ::std::assert_eq!(parse(q! { (1, 2) }).unwrap(), StructDefaultTuple(1, 2));
    ::std::assert_eq!(
        parse(q! { (1, 2, 3) }).unwrap_err_string(),
        "unexpected token `3`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(default = Self(1, 2))]
struct StructDefaultTupleExpr(i32, i32);

#[test]
fn struct_default_tuple_expr() {
    use ::std::prelude::v1::*;

    let parse = parse_meta::<StructDefaultTupleExpr>;
    ::std::assert_eq!(parse(q! { () }).unwrap(), StructDefaultTupleExpr(1, 2));
    ::std::assert_eq!(parse(q! { (3) }).unwrap(), StructDefaultTupleExpr(3, 2));
    ::std::assert_eq!(parse(q! { (3,) }).unwrap(), StructDefaultTupleExpr(3, 2));
    ::std::assert_eq!(parse(q! { (3, 4) }).unwrap(), StructDefaultTupleExpr(3, 4));
    ::std::assert_eq!(parse(q! { (3, 4,) }).unwrap(), StructDefaultTupleExpr(3, 4));
    ::std::assert_eq!(
        parse(q! { (3, 4,,) }).unwrap_err_string(),
        "unexpected token `,`"
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
struct FieldDefaults {
    #[deluxe(default = MyNewtype(::std::string::String::new()))]
    name: MyNewtype,
    #[deluxe(default)]
    value0: i32,
    #[deluxe(default = 1)]
    value1: i32,
    value: i32,
}

#[test]
fn field_defaults() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<FieldDefaults>;
    ::std::assert_eq!(
        parse(q! { { } }).unwrap_err_string(),
        "missing required field `value`"
    );
    ::std::assert_eq!(
        parse(q! { { value = 123 } }).unwrap(),
        FieldDefaults {
            name: MyNewtype(String::from("")),
            value0: 0,
            value1: 1,
            value: 123,
        }
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
    Default,
)]
#[deluxe(default)]
enum EnumDefault {
    #[default]
    A,
    B,
    C(#[deluxe(default)] i32),
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(default = Self::A)]
enum EnumDefaultExpr {
    A,
    B,
    C,
}

#[test]
fn enum_defaults() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<EnumDefault>;
    ::std::assert_eq!(parse(q! { { } }).unwrap(), EnumDefault::A);
    ::std::assert_eq!(parse(q! { { c } }).unwrap(), EnumDefault::C(0));
    ::std::assert_eq!(parse(q! { { c() } }).unwrap(), EnumDefault::C(0));
    ::std::assert_eq!(parse(q! { { c = () } }).unwrap(), EnumDefault::C(0));
    ::std::assert_eq!(parse(q! { { c(1) } }).unwrap(), EnumDefault::C(1));
    ::std::assert_eq!(parse(q! { { c(1,) } }).unwrap(), EnumDefault::C(1));
    ::std::assert_eq!(
        parse(q! { { c = (,) } }).unwrap_err_string(),
        "expected integer literal"
    );

    let parse = parse_meta::<EnumDefaultExpr>;
    ::std::assert_eq!(parse(q! { { } }).unwrap(), EnumDefaultExpr::A);
}

use ::deluxe as renamed_deluxe;

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(crate = renamed_deluxe)]
struct RenamedCrate(i32);

#[test]
fn renamed_crate() {
    let parse = parse_meta::<RenamedCrate>;
    ::std::assert_eq!(parse(q! { (50) }).unwrap(), RenamedCrate(50));
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(allow_unknown_fields)]
struct StructAllowUnknown {
    value: i32,
}

#[test]
fn struct_allow_unknown() {
    let parse = parse_meta::<StructAllowUnknown>;
    ::std::assert_eq!(
        parse(q! { { value = 10 } }).unwrap(),
        StructAllowUnknown { value: 10 }
    );
    ::std::assert_eq!(
        parse(q! { { value = 10, another = "hello" } }).unwrap(),
        StructAllowUnknown { value: 10 }
    );
    ::std::assert_eq!(
        parse(
            q! { { value = 10, complex = ::c::X + 123 + (Z(Y) % { x[0].a }), another = "hello" } }
        )
        .unwrap(),
        StructAllowUnknown { value: 10 }
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum EnumAllow {
    Known {
        value: i32,
    },
    #[deluxe(allow_unknown_fields)]
    Unknown {
        value: i32,
    },
    FlattenedKnown {
        #[deluxe(flatten)]
        s: MyNamed,
    },
    FlattenedUnknown {
        #[deluxe(flatten)]
        s: StructAllowUnknown,
    },
}

#[test]
fn enum_allow_unknown() {
    use ::std::prelude::v1::*;
    let parse = parse_meta::<EnumAllow>;
    ::std::assert_eq!(
        parse(q! { { known(value(50)) } }).unwrap(),
        EnumAllow::Known { value: 50 }
    );
    ::std::assert_eq!(
        parse(q! { { known(value(50), another("thing")) } }).unwrap_err_string(),
        "unknown field `another`"
    );
    ::std::assert_eq!(
        parse(q! { { unknown(value(50)) } }).unwrap(),
        EnumAllow::Unknown { value: 50 }
    );
    ::std::assert_eq!(
        parse(q! { { unknown(value(50), another("thing")) } }).unwrap(),
        EnumAllow::Unknown { value: 50 }
    );
    ::std::assert_eq!(
        parse(q! { { flattened_known(a(50), b("thing")) } }).unwrap(),
        EnumAllow::FlattenedKnown {
            s: MyNamed {
                a: 50,
                b: String::from("thing")
            }
        }
    );
    ::std::assert_eq!(
        parse(q! { { flattened_known(a(50), b("thing"), another("two")) } }).unwrap_err_string(),
        "unknown field `another`"
    );
    ::std::assert_eq!(
        parse(q! { { flattened_unknown(value(60)) } }).unwrap(),
        EnumAllow::FlattenedUnknown {
            s: StructAllowUnknown { value: 60 }
        }
    );
    ::std::assert_eq!(
        parse(q! { { flattened_unknown(value(60), another("thing")) } }).unwrap(),
        EnumAllow::FlattenedUnknown {
            s: StructAllowUnknown { value: 60 }
        }
    );
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(flatten_named))]
struct MyTransparentUnnamedStruct(MyNamed);

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(flatten_named))]
struct MyTransparentNamedStruct {
    named: MyNamed,
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(flatten_unnamed, append))]
struct MyTransparentUnnamedVec(::std::vec::Vec<i32>);

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(flatten_unnamed, append))]
struct MyTransparentNamedVec {
    nums: ::std::vec::Vec<i32>
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(rest))]
struct MyTransparentUnnamedMap(::std::collections::HashMap<::syn::Path, i32>);


#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(transparent(rest))]
struct MyTransparentNamedMap {
    nums: ::std::collections::HashMap<::syn::Path, i32>,
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
enum TransparentHolder {
    A { #[deluxe(flatten)] s: MyTransparentUnnamedStruct },
    B { #[deluxe(flatten)] s: MyTransparentNamedStruct },
    C(#[deluxe(flatten)] MyTransparentUnnamedVec),
    D(#[deluxe(flatten)] MyTransparentNamedVec),
    E { #[deluxe(append)] v: MyTransparentUnnamedVec },
    F { #[deluxe(append)] v: MyTransparentNamedVec },
    G { #[deluxe(rest)] r: MyTransparentUnnamedMap, },
    H { #[deluxe(rest)] r: MyTransparentNamedMap, },
}

#[test]
fn transparent_flat() {
    // TODO
}

#![deny(unsafe_code)]
#![no_implicit_prelude]

use ::quote::quote as q;

mod test_util;
use test_util::*;

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(single))]
struct SingleAttribute(char);

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(single))]
struct SingleAttributeNamed {
    c: char,
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(multi1, multi2))]
struct MultiAttributes(char);

#[test]
fn multi_attributes() {
    use ::deluxe::HasAttributes;

    let expr: ::syn::Expr = ::syn::parse2(q! { #[single('a')] true }).unwrap();
    let m: SingleAttribute = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 1);
    ::std::assert_eq!(m.0, 'a');

    let expr: ::syn::Expr = ::syn::parse2(q! { #[single(c = 'a')] true }).unwrap();
    let m: SingleAttributeNamed = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 1);
    ::std::assert_eq!(m.c, 'a');

    let expr: ::syn::Expr = ::syn::parse2(q! { true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, SingleAttribute>(&expr).unwrap_err_string(),
        "missing required field 0 on #[single]"
    );

    let expr: ::syn::Expr = ::syn::parse2(q! { #[single] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, SingleAttribute>(&expr).unwrap_err_string(),
        "missing required field 0 on #[single]"
    );

    let expr: ::syn::Expr = ::syn::parse2(q! { true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, SingleAttributeNamed>(&expr).unwrap_err_string(),
        "missing required field #[single(c)]"
    );

    let expr: ::syn::Expr = ::syn::parse2(q! { #[single] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, SingleAttributeNamed>(&expr).unwrap_err_string(),
        "missing required field #[single(c)]"
    );

    let expr: ::syn::Expr = ::syn::parse2(q! { #[multi1('a')] true }).unwrap();
    let m: MultiAttributes = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 1);
    ::std::assert_eq!(m.0, 'a');

    let expr: ::syn::Expr = ::syn::parse2(q! { #[multi2('b')] true }).unwrap();
    let m: MultiAttributes = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 1);
    ::std::assert_eq!(m.0, 'b');

    let expr = ::syn::parse2(q! { true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "missing required field 0 on #[multi1]"
    );

    let expr = ::syn::parse2(q! { #[multi1()] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "missing required field 0 on #[multi1]"
    );

    let expr = ::syn::parse2(q! { #[multi2()] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "missing required field 0 on #[multi2]"
    );

    let expr = ::syn::parse2(q! { #[multi2] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "missing required field 0 on #[multi2]"
    );

    let expr = ::syn::parse2(q! { #[multi3('c')] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "missing required field 0 on #[multi1]"
    );

    let expr = ::syn::parse2(q! { #[multi2('c')] #[multi2('d')] true }).unwrap();
    ::std::assert_eq!(
        ::deluxe::parse_attributes::<::syn::Expr, MultiAttributes>(&expr).unwrap_err_string(),
        "unexpected token"
    );
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(many))]
struct Many(i32, i32, i32, i32, i32, i32);

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(many), transparent)]
struct ManyVec(::std::vec::Vec<i32>);

#[test]
fn split_attributes() {
    let expr: ::syn::Expr =
        ::syn::parse2(q! { #[many(1)] #[many(2, 3)] #[many(4, 5, 6)] true }).unwrap();
    let m: Many = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(m, Many(1, 2, 3, 4, 5, 6));

    let expr: ::syn::Expr =
        ::syn::parse2(q! { #[many(1)] #[many(2, 3)] #[many(4, 5, 6)] true }).unwrap();
    let m: ManyVec = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(m, ManyVec(::std::vec![1, 2, 3, 4, 5, 6]));
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::clone))]
struct CloneContainer {
    #[deluxe(container)]
    container: ::syn::Expr,
    int: i32,
}

#[test]
fn clone_container() {
    use ::deluxe::HasAttributes;
    let mut expr = ::syn::parse2(q! {
        #[container::ignored(int = 4)]
        #[container::clone(int = 2)]
        true
    })
    .unwrap();
    let cont: CloneContainer = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 2);
    ::std::assert_eq!(cont.container, expr);
    ::std::assert_eq!(cont.int, 2);
    let cont2: CloneContainer = ::deluxe::extract_attributes(&mut expr).unwrap();
    ::std::assert_eq!(expr.attrs().len(), 1);
    ::std::assert_eq!(cont2.container, expr);
    ::std::assert_eq!(cont2.int, 2);
    ::std::assert_ne!(cont.container, cont2.container);
}

#[derive(::deluxe::ParseAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::r#ref))]
struct RefContainer<'e> {
    #[deluxe(container)]
    container: &'e ::syn::Expr,
    int: i32,
}

#[test]
fn ref_container() {
    let expr = ::syn::parse2(q! { #[container::r#ref(int = 3)] true }).unwrap();
    let cont: RefContainer = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container, &expr);
    ::std::assert_eq!(cont.int, 3);
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::clone_generic))]
struct CloneGenericContainer<T> {
    #[deluxe(container)]
    container: T,
    int: i32,
}

#[test]
fn clone_generic_container() {
    let mut expr = ::syn::parse2(q! { #[container::clone_generic(int = 4)] true }).unwrap();
    let cont: CloneGenericContainer<::syn::Expr> = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container, expr);
    ::std::assert_eq!(cont.int, 4);
    let cont2: CloneGenericContainer<::syn::Expr> =
        ::deluxe::extract_attributes(&mut expr).unwrap();
    ::std::assert_eq!(cont2.container, expr);
    ::std::assert_eq!(cont2.int, 4);
}

#[derive(::deluxe::ParseAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::ref_generic))]
struct RefGenericContainer<'t, T> {
    #[deluxe(container)]
    container: &'t T,
    int: i32,
}

#[test]
fn ref_generic_container() {
    let expr = ::syn::parse2(q! { #[container::ref_generic(int = 5)] true }).unwrap();
    let cont: RefGenericContainer<::syn::Expr> = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container, &expr);
    ::std::assert_eq!(cont.int, 5);
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(attributes(container::clone_option))]
struct CloneOptionContainer {
    #[deluxe(container, default)]
    container: ::std::option::Option<::syn::Expr>,
    int: i32,
}

#[test]
fn clone_option_container() {
    let mut expr = ::syn::parse2(q! { #[container::clone_option(int = 6)] true }).unwrap();
    let cont: CloneOptionContainer = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container.unwrap(), expr);
    ::std::assert_eq!(cont.int, 6);
    let cont2: CloneOptionContainer = ::deluxe::extract_attributes(&mut expr).unwrap();
    ::std::assert_eq!(cont2.container, ::std::option::Option::Some(expr));
    ::std::assert_eq!(cont2.int, 6);
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ParseMetaItem, PartialEq, Debug)]
#[deluxe(attributes(container::ref_option))]
struct RefOptionContainer<'e> {
    #[deluxe(container, default)]
    container: ::std::option::Option<&'e ::syn::Expr>,
    int: i32,
}

#[test]
fn ref_option_container() {
    let expr = ::syn::parse2(q! { #[container::ref_option(int = 7)] true }).unwrap();
    let cont: RefOptionContainer = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container, ::std::option::Option::Some(&expr));
    ::std::assert_eq!(cont.int, 7);
}

#[derive(
    ::deluxe::ParseAttributes,
    ::deluxe::ExtractAttributes,
    ::deluxe::ParseMetaItem,
    PartialEq,
    Debug,
)]
#[deluxe(attributes(container::clone_generic_option))]
struct CloneGenericOptionContainer<T> {
    #[deluxe(container, default)]
    container: ::std::option::Option<T>,
    int: i32,
}

#[test]
fn clone_generic_option_container() {
    let mut expr = ::syn::parse2(q! { #[container::clone_generic_option(int = 8)] true }).unwrap();
    let cont: CloneGenericOptionContainer<::syn::Expr> = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container.unwrap(), expr);
    ::std::assert_eq!(cont.int, 8);
    let cont2: CloneGenericOptionContainer<::syn::Expr> =
        ::deluxe::extract_attributes(&mut expr).unwrap();
    ::std::assert_eq!(cont2.container, ::std::option::Option::Some(expr));
    ::std::assert_eq!(cont2.int, 8);
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ParseMetaItem, PartialEq, Debug)]
#[deluxe(attributes(container::ref_generic_option))]
struct RefGenericOptionContainer<'t, T> {
    #[deluxe(container, default)]
    container: ::std::option::Option<&'t T>,
    int: i32,
}

#[test]
fn ref_generic_option_container() {
    let expr = ::syn::parse2(q! { #[container::ref_generic_option(int = 9)] true }).unwrap();
    let cont: RefGenericOptionContainer<::syn::Expr> = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert_eq!(cont.container, ::std::option::Option::Some(&expr));
    ::std::assert_eq!(cont.int, 9);
}

#[derive(::deluxe::ParseAttributes, ::deluxe::ExtractAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::an_enum))]
enum ContainerEnum {
    A(#[deluxe(container)] ::syn::Expr, #[deluxe(skip)] i32, i32),
    B {
        #[deluxe(container)]
        container: ::syn::Expr,
        #[deluxe(skip)]
        skipped: i32,
        value: i32,
    },
    #[deluxe(flatten)]
    C {
        #[deluxe(container)]
        container: ::syn::Expr,
        #[deluxe(skip)]
        skipped: i32,
        c: i32,
    },
    #[deluxe(flatten)]
    D {
        #[deluxe(container)]
        container: ::syn::Expr,
        #[deluxe(skip)]
        skipped: i32,
    },
}

#[test]
fn container_enum() {
    let expr: ::syn::Expr = ::syn::parse2(q! {
        #[container::an_enum(a(500))]
        true
    })
    .unwrap();
    let cont: ContainerEnum = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert!(::std::matches!(cont, ContainerEnum::A(e, 0, 500) if e == expr));

    let expr: ::syn::Expr = ::syn::parse2(q! {
        #[container::an_enum(b(value = 501))]
        true
    })
    .unwrap();
    let cont: ContainerEnum = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert!(
        ::std::matches!(cont, ContainerEnum::B { container: e, skipped: 0, value: 501 } if e == expr)
    );

    let expr: ::syn::Expr = ::syn::parse2(q! {
        #[container::an_enum(c = 502)]
        true
    })
    .unwrap();
    let cont: ContainerEnum = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert!(
        ::std::matches!(cont, ContainerEnum::C { container: e, skipped: 0, c: 502 } if e == expr)
    );

    let expr: ::syn::Expr = ::syn::parse2(q! {
        #[container::an_enum()]
        true
    })
    .unwrap();
    let cont: ContainerEnum = ::deluxe::parse_attributes(&expr).unwrap();
    ::std::assert!(
        ::std::matches!(cont, ContainerEnum::D { container: e, skipped: 0 } if e == expr)
    );
}

#[derive(Debug, PartialEq)]
struct IdentWrapper<'t>(&'t ::syn::Ident);

impl<'t> ::deluxe::ContainerFrom<'t, ::syn::ItemFn> for IdentWrapper<'t> {
    #[inline]
    fn container_from(t: &'t ::syn::ItemFn) -> Self {
        Self(&t.sig.ident)
    }
}

#[derive(::deluxe::ParseAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::wrapper))]
struct WrapperContainer<'t> {
    #[deluxe(container(ty = ::syn::ItemFn))]
    container: IdentWrapper<'t>,
    int: i32,
}

#[derive(Debug, PartialEq)]
struct Ident2Wrapper<'s, 't>(::std::borrow::Cow<'s, str>, &'t ::syn::Ident);

impl<'s, 't> ::deluxe::ContainerFrom<'t, ::syn::ItemFn> for Ident2Wrapper<'s, 't> {
    #[inline]
    fn container_from(t: &'t ::syn::ItemFn) -> Self {
        Self(::std::convert::From::from("hello"), &t.sig.ident)
    }
}

#[derive(::deluxe::ParseAttributes, PartialEq, Debug)]
#[deluxe(attributes(container::wrapper))]
struct Wrapper2Container<'s, 't> {
    #[deluxe(container(ty = ::syn::ItemFn, lifetime = 't))]
    container: Ident2Wrapper<'s, 't>,
    int: i32,
}

#[test]
fn custom_container() {
    use ::deluxe::HasAttributes;
    let func: ::syn::ItemFn = ::syn::parse2(q! {
        #[container::wrapper(int = 72)]
        fn abc() -> i32 { 0 }
    })
    .unwrap();
    let cont: WrapperContainer = ::deluxe::parse_attributes(&func).unwrap();
    ::std::assert_eq!(func.attrs().len(), 1);
    ::std::assert_eq!(cont.container.0, &func.sig.ident);
    ::std::assert_eq!(cont.int, 72);

    let cont: Wrapper2Container = ::deluxe::parse_attributes(&func).unwrap();
    ::std::assert_eq!(func.attrs().len(), 1);
    ::std::assert_eq!(cont.container.0, "hello");
    ::std::assert_eq!(cont.container.1, &func.sig.ident);
    ::std::assert_eq!(cont.int, 72);
}

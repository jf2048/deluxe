#![no_implicit_prelude]

use ::quote::quote as q;

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

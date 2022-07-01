#[doc(hidden)]
pub mod ____private {
    pub use deluxe_core::parse_helpers;
    pub use proc_macro2::Span;
    pub use std::{
        borrow::Cow,
        clone::Clone,
        format,
        option::Option,
        primitive::{bool, str},
        string::ToString,
        unreachable,
        vec::Vec,
    };
    pub use syn::{parse::ParseStream, Error, Path};
}

pub use deluxe_core::{
    Error, Errors, ExtractAttributes, HasAttributes, ParseAttributes, ParseMetaFlatNamed,
    ParseMetaFlatUnnamed, ParseMetaItem, Result,
};
pub use deluxe_macros::*;

#[cfg(test)]
mod tests {
    #![no_implicit_prelude]

    use crate::____private as private;

    struct U(::std::primitive::i32);

    impl crate::ParseMetaItem for U {
        fn parse_meta_item(input: private::ParseStream) -> crate::Result<Self> {
            crate::Result::Ok(Self(crate::ParseMetaItem::parse_meta_item(input)?))
        }
    }

    impl<T: crate::HasAttributes> crate::ParseAttributes<T> for U {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("u")
        }
        fn parse_attributes(obj: &T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let mut ____field0 = private::Option::None;
            let attrs = crate::HasAttributes::attrs(obj);
            for attr in attrs {
                if <Self as crate::ParseAttributes<T>>::path_matches(&attr.path) {
                    if ____field0.is_some() {
                        errors.push_spanned(&attr.path, "Duplicate attribute");
                    }
                    ____field0 = private::Option::Some(private::parse_helpers::parse_tokens(
                        private::Clone::clone(&attr.tokens),
                        crate::ParseMetaItem::parse_named_meta_item,
                    )?);
                    if let crate::Result::Err(err) =
                        private::parse_helpers::parse_eof_or_trailing_comma(input)
                    {
                        errors.extend(err);
                    }
                    return crate::Result::Ok(ret);
                }
            }

            crate::Result::Err(crate::Error::new_single(
                private::Span::call_site(),
                "Attribute not found",
            ))
        }
    }

    impl<T: crate::HasAttributes> crate::ExtractAttributes<T> for U {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("u")
        }
        fn extract_attributes(obj: &mut T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let attrs = crate::HasAttributes::attrs_mut(obj)?;
            let mut index = 0;
            while index < attrs.len() {
                if <Self as crate::ParseAttributes<T>>::path_matches(&attrs[index].path) {
                    let attr = attrs.remove(index);
                    let ret = private::parse_helpers::parse_tokens(
                        attr.tokens,
                        crate::ParseMetaItem::parse_named_meta_item,
                    )?;
                    return crate::Result::Ok(ret);
                } else {
                    index += 1;
                }
            }
            crate::Result::Err(crate::Error::new_single(
                private::Span::call_site(),
                "Attribute not found",
            ))
        }
    }

    struct A(::std::primitive::i32, ::std::string::String);

    impl crate::ParseMetaItem for A {
        fn parse_meta_item(input: private::ParseStream) -> crate::Result<Self> {
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            private::parse_helpers::parse_tuple_struct(input, 2, |input, index| {
                match index {
                    0 => {
                        ____field1 = private::Option::Some(
                            crate::ParseMetaItem::parse_unnamed_meta_item(input)?,
                        )
                    }
                    1 => {
                        ____field2 = private::Option::Some(
                            crate::ParseMetaItem::parse_unnamed_meta_item(input)?,
                        )
                    }
                    _ => private::unreachable!(),
                }
                crate::Result::Ok(())
            })?;
            let errors = crate::Errors::new();
            if ____field1.is_none() {
                errors.push(private::Span::call_site(), "Missing required tuple field 0");
            }
            if ____field2.is_none() {
                errors.push(private::Span::call_site(), "Missing required tuple field 1");
            }
            if !input.is_empty() {
                errors.push(input.span(), "Unknown token");
                return crate::Result::Err(errors);
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors),
            };
            crate::Result::Ok(A(____field1, ____field2))
        }
        #[inline]
        fn parse_unnamed_meta_item(input: private::ParseStream) -> crate::Result<Self> {
            <private::parse_helpers::Paren as private::parse_helpers::ParseDelimited>::parse_delimited_meta_item(input)
        }
        #[inline]
        fn parse_named_meta_item(input: private::ParseStream) -> crate::Result<Self> {
            <private::parse_helpers::Paren as private::parse_helpers::ParseDelimited>::parse_delimited_meta_item(input)
        }
    }

    impl crate::ParseMetaFlatUnnamed for A {
        #[inline]
        fn parse_meta_flat_unnamed(input: private::ParseStream) -> crate::Result<Self> {
            <Self as crate::ParseMetaItem>::parse_meta_item(input)
        }
    }

    impl<T: crate::HasAttributes> crate::ParseAttributes<T> for A {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("a")
        }
        fn parse_attributes(obj: &T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            let attrs = crate::HasAttributes::attrs(obj);
            for attr in attrs {
                if <Self as crate::ParseAttributes<T>>::path_matches(&attr.path) {
                    let res = private::parse_helpers::parse_struct_attr_tokens(
                        private::Clone::clone(&attr.tokens),
                        |input| {
                            private::parse_helpers::parse_tuple_struct(
                                input,
                                2,
                                |input, index| {
                                    match index {
                                        0 => {
                                            ____field1 = private::Option::Some(
                                                crate::ParseMetaItem::parse_unnamed_meta_item(
                                                    input,
                                                )?,
                                            )
                                        }
                                        1 => {
                                            ____field2 = private::Option::Some(
                                                crate::ParseMetaItem::parse_unnamed_meta_item(
                                                    input,
                                                )?,
                                            )
                                        }
                                        _ => private::unreachable!(),
                                    }
                                    crate::Result::Ok(())
                                },
                            )?;
                            if let crate::Result::Err(err) =
                                private::parse_helpers::parse_eof_or_trailing_comma(input)
                            {
                                errors.extend(err);
                            }
                            crate::Result::Ok(())
                        },
                    );
                    if let crate::Result::Err(err) = res {
                        errors.extend(err);
                    }
                }
            }
            if ____field1.is_none() {
                errors.push(private::Span::call_site(), "Missing required tuple field 0");
            }
            if ____field2.is_none() {
                errors.push(private::Span::call_site(), "Missing required tuple field 1");
            }
            if !input.is_empty() {
                errors.push(input.span(), "Unknown token");
                return crate::Result::Err(errors);
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors),
            };
            crate::Result::Ok(A(____field1, ____field2))
        }
    }

    impl<T: crate::HasAttributes> crate::ExtractAttributes<T> for A {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("a")
        }
        fn extract_attributes(obj: &mut T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            let attrs = crate::HasAttributes::attrs_mut(obj)?;
            let mut index = 0;
            while index < attrs.len() {
                if <Self as crate::ExtractAttributes<T>>::path_matches(&attrs[index].path) {
                    let attr = attrs.remove(index);
                    let res =
                        private::parse_helpers::parse_struct_attr_tokens(attr.tokens, |input| {
                            private::parse_helpers::parse_tuple_struct(input, 2, |input, index| {
                                match index {
                                    0 => {
                                        ____field1 = private::Option::Some(
                                            crate::ParseMetaItem::parse_unnamed_meta_item(input)?,
                                        )
                                    }
                                    1 => {
                                        ____field2 = private::Option::Some(
                                            crate::ParseMetaItem::parse_unnamed_meta_item(input)?,
                                        )
                                    }
                                    _ => private::unreachable!(),
                                }
                                crate::Result::Ok(())
                            })
                        });
                    if let crate::Result::Err(err) = res {
                        errors.extend(err);
                    }
                } else {
                    index += 1;
                }
            }
            if !errors.is_empty() {
                return crate::Result::Err(errors);
            }
            crate::Result::Ok(A(____field1.unwrap(), ____field2.unwrap()))
        }
    }

    struct B {
        my_a: ::std::primitive::i32,
        my_b: ::std::string::String,
    }

    impl crate::ParseMetaItem for B {
        fn parse_meta_item(input: private::ParseStream) -> crate::Result<Self> {
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            let errors = crate::Errors::new();
            private::parse_helpers::parse_struct(input, |input, ident| {
                match private::ToString::to_string(&ident).as_str() {
                    "my_a" => {
                        ____field1 = private::Option::Some(
                            crate::ParseMetaItem::parse_named_meta_item(input)?,
                        )
                    }
                    "my_b" => {
                        ____field2 = private::Option::Some(
                            crate::ParseMetaItem::parse_named_meta_item(input)?,
                        )
                    }
                    _ => errors.push_spanned(ident, private::format!("Unknown field `{}`", ident)),
                }
                crate::Result::Ok(())
            })?;
            if ____field1.is_none() {
                errors.push(private::Span::call_site(), "Missing required field `my_a`");
            }
            if ____field2.is_none() {
                errors.push(private::Span::call_site(), "Missing required field `my_b`");
            }
            if !input.is_empty() {
                errors.push(input.span(), "Unknown token");
                return crate::Result::Err(errors);
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors),
            };
            crate::Result::Ok(B {
                my_a: ____field1,
                my_b: ____field2,
            })
        }
    }

    impl crate::ParseMetaFlatNamed for B {
        #[inline]
        fn field_names() -> private::Cow<'static, [&'static private::str]> {
            private::Cow::Borrowed(&["my_a", "my_b"])
        }
        fn parse_meta_flat_named(
            input: private::ParseStream,
        ) -> crate::Result<(Self, private::Vec<&'static private::str>)> {
            <Self as crate::ParseMetaItem>::parse_meta_item(input);
            todo!()
        }
    }
}

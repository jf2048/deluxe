#[doc(hidden)]
pub mod ____private {
    pub use deluxe_core::parse_helpers;
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
    from_str, Error, Errors, ExtractAttributes, HasAttributes, ParseAttributes, ParseMetaFlatNamed,
    ParseMetaFlatUnnamed, ParseMetaItem, ParseMode, Result,
};
pub use deluxe_macros::*;

#[cfg(test)]
mod tests {
    #![no_implicit_prelude]

    use crate::____private as private;

    struct U(::std::primitive::i32);

    impl crate::ParseMetaItem for U {
        fn parse_meta_item(
            input: private::ParseStream,
            mode: crate::ParseMode,
        ) -> crate::Result<Self> {
            crate::Result::Ok(Self(crate::ParseMetaItem::parse_meta_item(input, mode)?))
        }
    }

    impl<T: crate::HasAttributes> crate::ParseAttributes<T> for U {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("u")
        }
        fn parse_attributes(obj: &T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let mut ret = private::Option::None;
            let attrs = crate::HasAttributes::attrs(obj);
            for attr in attrs {
                if <Self as crate::ParseAttributes<T>>::path_matches(&attr.path) {
                    if ret.is_some() {
                        errors.push_spanned(&attr.path, "Duplicate attribute for `U`");
                    }
                    let res = private::parse_helpers::parse_tokens(
                        private::Clone::clone(&attr.tokens),
                        private::parse_helpers::parse_named_meta_item,
                    );
                    match res {
                        crate::Result::Ok(res) => ret = private::Option::Some(res),
                        crate::Result::Err(err) => errors.push_syn(err),
                    }
                }
            }

            match ret {
                private::Option::None => {
                    errors.push_call_site("Attribute not found for `U`");
                    crate::Result::Err(errors.check().unwrap_err())
                }
                private::Option::Some(ret) => {
                    errors.check()?;
                    crate::Result::Ok(ret)
                }
            }
        }
    }

    impl<T: crate::HasAttributes> crate::ExtractAttributes<T> for U {
        #[inline]
        fn path_matches(path: &private::Path) -> private::bool {
            path.is_ident("u")
        }
        fn extract_attributes(obj: &mut T) -> crate::Result<Self> {
            let errors = crate::Errors::new();
            let mut ret = private::Option::None;
            let attrs = crate::HasAttributes::attrs_mut(obj)?;
            let mut index = 0;
            while index < attrs.len() {
                if <Self as crate::ParseAttributes<T>>::path_matches(&attrs[index].path) {
                    let attr = attrs.remove(index);
                    if ret.is_some() {
                        errors.push_spanned(&attr.path, "Duplicate attribute for `U`");
                    }
                    let res = private::parse_helpers::parse_tokens(
                        private::Clone::clone(&attr.tokens),
                        private::parse_helpers::parse_named_meta_item,
                    );
                    match res {
                        crate::Result::Ok(res) => ret = private::Option::Some(res),
                        crate::Result::Err(err) => errors.push_syn(err),
                    }
                } else {
                    index += 1;
                }
            }

            match ret {
                private::Option::None => {
                    errors.push_call_site("Attribute not found for `U`");
                    crate::Result::Err(errors.check().unwrap_err())
                }
                private::Option::Some(ret) => {
                    errors.check()?;
                    crate::Result::Ok(ret)
                }
            }
        }
    }

    struct A(::std::primitive::i32, ::std::string::String);

    impl crate::ParseMetaItem for A {
        #[inline]
        fn parse_meta_item(
            input: private::ParseStream,
            _mode: crate::ParseMode,
        ) -> crate::Result<Self> {
            <private::parse_helpers::Paren as private::parse_helpers::ParseDelimited>::parse_delimited_meta_item(input, crate::ParseMode::Unnamed)
        }
        fn parse_meta_item_inline(
            input: private::ParseStream,
            _mode: crate::ParseMode,
        ) -> crate::Result<Self> {
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            private::parse_helpers::parse_tuple_struct(input, 2, |input, index| {
                match index {
                    0 => {
                        ____field1 = private::Option::Some(crate::ParseMetaItem::parse_meta_item(
                            input,
                            crate::ParseMode::Unnamed,
                        )?)
                    }
                    1 => {
                        ____field2 = private::Option::Some(crate::ParseMetaItem::parse_meta_item(
                            input,
                            crate::ParseMode::Unnamed,
                        )?)
                    }
                    _ => private::unreachable!(),
                }
                crate::Result::Ok(())
            })?;
            let errors = crate::Errors::new();
            if ____field1.is_none() {
                errors.push_call_site("Missing required tuple field 0 for `A`");
            }
            if ____field2.is_none() {
                errors.push_call_site("Missing required tuple field 1 for `A`");
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
            };
            crate::Result::Ok(A(____field1, ____field2))
        }
    }

    impl crate::ParseMetaFlatUnnamed for A {
        #[inline]
        fn field_count() -> private::Option<usize> {
            private::Option::Some(2)
        }
        #[inline]
        fn parse_meta_flat_unnamed(input: private::ParseStream) -> crate::Result<Self> {
            <Self as crate::ParseMetaItem>::parse_meta_item_inline(input, crate::ParseMode::Unnamed)
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
                                                crate::ParseMetaItem::parse_meta_item(
                                                    input,
                                                    crate::ParseMode::Unnamed,
                                                )?,
                                            )
                                        }
                                        1 => {
                                            ____field2 = private::Option::Some(
                                                crate::ParseMetaItem::parse_meta_item(
                                                    input,
                                                    crate::ParseMode::Unnamed,
                                                )?,
                                            )
                                        }
                                        _ => private::unreachable!(),
                                    }
                                    crate::Result::Ok(())
                                },
                            )?;
                            crate::Result::Ok(())
                        },
                    );
                    if let crate::Result::Err(err) = res {
                        errors.extend(err);
                    }
                }
            }
            if ____field1.is_none() {
                errors.push_call_site("Missing required tuple field 0");
            }
            if ____field2.is_none() {
                errors.push_call_site("Missing required tuple field 1");
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
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
                                            crate::ParseMetaItem::parse_meta_item(
                                                input,
                                                crate::ParseMode::Unnamed,
                                            )?,
                                        )
                                    }
                                    1 => {
                                        ____field2 = private::Option::Some(
                                            crate::ParseMetaItem::parse_meta_item(
                                                input,
                                                crate::ParseMode::Unnamed,
                                            )?,
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
            errors.check()?;
            crate::Result::Ok(A(____field1.unwrap(), ____field2.unwrap()))
        }
    }

    struct B {
        my_a: ::std::primitive::i32,
        my_b: ::std::string::String,
    }

    impl crate::ParseMetaItem for B {
        #[inline]
        fn parse_meta_item(
            input: private::ParseStream,
            _mode: crate::ParseMode,
        ) -> crate::Result<Self> {
            <private::parse_helpers::Brace as private::parse_helpers::ParseDelimited>::parse_delimited_meta_item(input, crate::ParseMode::Named)
        }
        fn parse_meta_item_inline(
            input: private::ParseStream,
            _mode: crate::ParseMode,
        ) -> crate::Result<Self> {
            let mut ____field1 = private::Option::None;
            let mut ____field2 = private::Option::None;
            let errors = crate::Errors::new();
            private::parse_helpers::parse_struct(input, |input, ident| {
                match private::ToString::to_string(&ident).as_str() {
                    "my_a" => {
                        ____field1 = private::Option::Some(
                            private::parse_helpers::parse_named_meta_item(input)?,
                        )
                    }
                    "my_b" => {
                        ____field2 = private::Option::Some(
                            private::parse_helpers::parse_named_meta_item(input)?,
                        )
                    }
                    _ => errors.push_spanned(&ident, private::format!("Unknown field `{}`", ident)),
                }
                crate::Result::Ok(())
            })?;
            if ____field1.is_none() {
                errors.push_call_site("Missing required field `my_a`");
            }
            if ____field2.is_none() {
                errors.push_call_site("Missing required field `my_b`");
            }
            let ____field1 = match ____field1 {
                private::Option::Some(____field1) => ____field1,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
            };
            let ____field2 = match ____field2 {
                private::Option::Some(____field2) => ____field2,
                private::Option::None => return crate::Result::Err(errors.check().unwrap_err()),
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
            <Self as crate::ParseMetaItem>::parse_meta_item_inline(input, crate::ParseMode::Named);
            ::std::todo!()
        }
    }
}

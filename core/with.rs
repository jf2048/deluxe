//! Custom parsing helpers for `#[deluxe(with = ...)]`.

/// Helpers for parsing any type that implements [`std::str::FromStr`].
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::from_str)]`
pub mod from_str {
    #![allow(missing_docs)]
    use crate::{Error, ParseMode, Result};
    use std::{borrow::Borrow, str::FromStr};
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item<T: FromStr>(input: ParseStream, _mode: ParseMode) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        let s = input.parse::<syn::LitStr>()?;
        T::from_str(&s.value()).map_err(|e| Error::new_spanned(s, e.to_string()))
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: FromStr>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag<T>(span: proc_macro2::Span) -> Result<T> {
        Err(crate::parse_helpers::flag_disallowed_error(span))
    }
    #[inline]
    pub fn parse_meta_item_named<T: FromStr>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T>(name: &str, span: proc_macro2::Span) -> Result<T> {
        Err(crate::parse_helpers::missing_field_error(name, span))
    }
}

/// Helpers for parsing any type that implements [`std::str::FromStr`] and [`Default`].
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::from_str_default)]`
pub mod from_str_default {
    #![allow(missing_docs)]
    use crate::{ParseMode, Result};
    use std::{borrow::Borrow, str::FromStr};
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item<T: FromStr>(input: ParseStream, mode: ParseMode) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        super::from_str::parse_meta_item(input, mode)
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: FromStr>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        super::from_str::parse_meta_item_inline(inputs, mode)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: Default>(_span: proc_macro2::Span) -> Result<T> {
        Ok(Default::default())
    }
    pub fn parse_meta_item_named<T: FromStr + Default>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T>
    where
        T::Err: std::fmt::Display,
    {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T: Default>(_name: &str, _span: proc_macro2::Span) -> Result<T> {
        Ok(Default::default())
    }
}

/// Helpers for parsing a module path using
/// [`syn::Path::parse_mod_style`](::syn::Path::parse_mod_style).
///
/// The field should be a `syn::Path`. Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::mod_path)]`
pub mod mod_path {
    #![allow(missing_docs)]
    use crate::{ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<syn::Path> {
        input.call(syn::Path::parse_mod_style)
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<syn::Path> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag(span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::flag_disallowed_error(span))
    }
    #[inline]
    pub fn parse_meta_item_named(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<syn::Path> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item(name: &str, span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::missing_field_error(name, span))
    }
}

/// Helpers for parsing a path allowing any keywords as identifiers, and containing no path arguments.
///
/// The field should be a `syn::Path`. Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::any_path)]`
pub mod any_path {
    #![allow(missing_docs)]
    use crate::{ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<syn::Path> {
        input.call(crate::parse_helpers::parse_any_path)
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<syn::Path> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag(span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::flag_disallowed_error(span))
    }
    #[inline]
    pub fn parse_meta_item_named(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<syn::Path> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item(name: &str, span: proc_macro2::Span) -> Result<syn::Path> {
        Err(crate::parse_helpers::missing_field_error(name, span))
    }
}

/// Helpers for parsing any type that implements [`ParseMetaItem`](crate::ParseMetaItem) parsed out
/// of a quoted string first.
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::quoted)]`
pub mod quoted {
    #![allow(missing_docs)]
    use crate::{Error, Errors, ParseMetaItem, ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{ParseBuffer, ParseStream};

    pub fn parse_meta_item<T: ParseMetaItem>(input: ParseStream, mode: ParseMode) -> Result<T> {
        let v = input.parse::<syn::LitStr>()?;
        syn::parse::Parser::parse_str(
            |stream: ParseStream<'_>| T::parse_meta_item(stream, mode),
            &v.value(),
        )
        .map_err(|e| {
            e.into_iter()
                .map(|e| Error::new(v.span(), e))
                .collect::<Errors>()
                .check()
                .unwrap_err()
        })
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: ParseMetaItem>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: ParseMetaItem>(span: proc_macro2::Span) -> Result<T> {
        T::parse_meta_item_flag(span)
    }
    #[inline]
    pub fn parse_meta_item_named<T: ParseMetaItem>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T: ParseMetaItem>(name: &str, span: proc_macro2::Span) -> Result<T> {
        T::missing_meta_item(name, span)
    }
}

/// Helpers for parsing any type that implements [`ParseMetaItem`](crate::ParseMetaItem) possibly
/// parsed out of a quoted string first.
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::maybe_quoted)]`
pub mod maybe_quoted {
    #![allow(missing_docs)]
    use crate::{ParseMetaItem, ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item<T: ParseMetaItem>(input: ParseStream, mode: ParseMode) -> Result<T> {
        if input.peek(syn::LitStr) {
            crate::with::quoted::parse_meta_item(input, mode)
        } else {
            T::parse_meta_item(input, mode)
        }
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: ParseMetaItem>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: ParseMetaItem>(span: proc_macro2::Span) -> Result<T> {
        T::parse_meta_item_flag(span)
    }
    #[inline]
    pub fn parse_meta_item_named<T: ParseMetaItem>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T: ParseMetaItem>(name: &str, span: proc_macro2::Span) -> Result<T> {
        T::missing_meta_item(name, span)
    }
}

/// Helpers for parsing any type that implements [`syn::parse::Parse`](::syn::parse::Parse).
///
/// Can be used on a field by specifying the module, like `#[deluxe(with = deluxe::with::syn)]`
pub mod syn {
    #![allow(missing_docs)]
    use crate::{ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{Parse, ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item<T: Parse>(input: ParseStream, _mode: ParseMode) -> Result<T> {
        input.parse::<T>()
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: Parse>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: Parse>(span: proc_macro2::Span) -> Result<T> {
        crate::parse_helpers::parse_empty(span, T::parse)
    }
    #[inline]
    pub fn parse_meta_item_named<T: Parse>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T>(name: &str, span: proc_macro2::Span) -> Result<T> {
        Err(crate::parse_helpers::missing_field_error(name, span))
    }
}

/// Helpers for parsing any type that implements [`syn::parse::Parse`](::syn::parse::Parse) parsed
/// out of a quoted string first.
///
/// Can be used on a field by specifying the module, like
/// `#[deluxe(with = deluxe::with::syn_quoted)]`
pub mod syn_quoted {
    #![allow(missing_docs)]
    use crate::{Error, Errors, ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{Parse, ParseBuffer, ParseStream};

    pub fn parse_meta_item<T: Parse>(input: ParseStream, _mode: ParseMode) -> Result<T> {
        let v = input.parse::<syn::LitStr>()?;
        syn::parse_str(&v.value()).map_err(|e| {
            e.into_iter()
                .map(|e| Error::new(v.span(), e))
                .collect::<Errors>()
                .check()
                .unwrap_err()
        })
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: Parse>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T> {
        crate::parse_helpers::parse_first(inputs, mode, parse_meta_item)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: Parse>(span: proc_macro2::Span) -> Result<T> {
        crate::parse_helpers::parse_empty(span, T::parse)
    }
    #[inline]
    pub fn parse_meta_item_named<T: Parse>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T>(name: &str, span: proc_macro2::Span) -> Result<T> {
        Err(crate::parse_helpers::missing_field_error(name, span))
    }
}

/// Generates a module for parsing an optional value using `#[deluxe(with = ...)].
///
/// Takes three arguments separated by commas:
/// - The generated module. Can include attributes and a visibility specifier.
/// - The module to use as an inner parser, relative to the new module.
/// - The target type.
///
/// # Example
///
/// Defines a new module named `mod_path_optional` that parses an [`Option`] using
/// [`with::mod_path`](self::mod_path).
///
/// ```
/// deluxe_core::define_with_optional!(
///     pub mod mod_path_optional,
///     deluxe_core::with::mod_path,
///     syn::Path
/// );
/// ```
#[macro_export]
macro_rules! define_with_optional {
    (
        $(#[$attrs:meta])* $vis:vis mod $mod:ident,
        $($path:ident)? $(:: $path_rest:ident)*,
        $ty:ty $(,)?
    ) => {
        $(#[$attrs])* $vis mod $mod {
            #[repr(transparent)]
            struct Inner($ty);

            impl $crate::ParseMetaItem for Inner {
                #[inline]
                fn parse_meta_item(
                    input: $crate::syn::parse::ParseStream,
                    mode: $crate::ParseMode,
                ) -> $crate::Result<Self> {
                    $crate::Result::Ok(Self($($path)? $(::$path_rest)* ::parse_meta_item(input, mode)?))
                }
            }

            #[inline]
            pub fn parse_meta_item(
                input: $crate::syn::parse::ParseStream,
                mode: $crate::ParseMode,
            ) -> $crate::Result<$crate::Option<$ty>> {
                $crate::Result::Ok(
                    <$crate::Option<Inner> as $crate::ParseMetaItem>::parse_meta_item(input, mode)?
                        .map(|p| p.0),
                )
            }
            #[inline]
            pub fn parse_meta_item_inline<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                mode: $crate::ParseMode,
            ) -> $crate::Result<$crate::Option<$ty>> {
                $crate::Result::Ok(
                    <$crate::Option<Inner> as $crate::ParseMetaItem>::parse_meta_item_inline(inputs, mode)?
                        .map(|p| p.0),
                )
            }
            #[inline]
            pub fn parse_meta_item_flag(
                _span: $crate::Span,
            ) -> $crate::Result<$crate::Option<$ty>> {
                $crate::Result::Ok($crate::Option::None)
            }
            #[inline]
            pub fn parse_meta_item_named(
                input: $crate::syn::parse::ParseStream,
                _name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$crate::Option<$ty>> {
                $crate::parse_named_meta_item_with!(input, span, self)
            }
            #[inline]
            pub fn missing_meta_item(
                name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$crate::Option<$ty>> {
                $crate::Result::Ok($crate::Option::None)
            }
        }
    };
}

/// Generates a module for parsing a collection using `#[deluxe(with = ...)].
///
/// Takes three arguments separated by commas:
/// - The generated module. Can include attributes and a visibility specifier.
/// - The module to use as an inner parser, relative to the new module.
/// - The target collection type.
///
/// # Example
///
/// Defines a new module named `mod_path_vec` that parses a [`Vec`] of paths using
/// [`with::mod_path`](self::mod_path).
///
/// ```
/// deluxe_core::define_with_collection!(
///     pub mod mod_path_vec,
///     deluxe_core::with::mod_path,
///     Vec<syn::Path>
/// );
/// ```
#[macro_export]
macro_rules! define_with_collection {
    (
        $(#[$attrs:meta])* $vis:vis mod $mod:ident,
        $($path:ident)? $(:: $path_rest:ident)*,
        $($coll:ident)? $(:: $colls:ident)* < $ty:ty > $(,)?
    ) => {
        $(#[$attrs])* $vis mod $mod {
            #[repr(transparent)]
            struct Inner($ty);

            impl $crate::ParseMetaItem for Inner {
                #[inline]
                fn parse_meta_item(
                    input: $crate::syn::parse::ParseStream,
                    mode: $crate::ParseMode,
                ) -> $crate::Result<Self> {
                    $crate::Result::Ok(Self($($path)? $(::$path_rest)* ::parse_meta_item(input, mode)?))
                }
            }

            #[inline]
            pub fn parse_meta_item(
                input: $crate::syn::parse::ParseStream,
                mode: $crate::ParseMode,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$($coll)? $(:: $colls)* <Inner> as $crate::ParseMetaItem>::parse_meta_item(input, mode)?
                            ),
                            |p| p.0,
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_item_inline<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                mode: $crate::ParseMode,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$($coll)? $(:: $colls)* <Inner> as $crate::ParseMetaItem>::parse_meta_item_inline(inputs, mode)?
                            ),
                            |p| p.0,
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_item_flag(
                _span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::Result::Ok($crate::Default::default())
            }
            #[inline]
            pub fn parse_meta_item_named(
                input: $crate::syn::parse::ParseStream,
                _name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::parse_named_meta_item_with!(input, span, self)
            }
            #[inline]
            pub fn missing_meta_item(
                name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::Result::Ok($crate::Default::default())
            }
            #[inline]
            pub fn field_count() -> $crate::Option<$crate::primitive::usize> {
                $crate::Option::None
            }
            #[inline]
            pub fn parse_meta_flat_unnamed<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                mode: $crate::ParseMode,
                index: $crate::primitive::usize
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$($coll)? $(:: $colls)* <Inner> as $crate::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(inputs, mode, index)?
                            ),
                            |p| p.0,
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_append<'s, S, I, P>(
                inputs: &[S],
                paths: I,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$ty>>
            where
                S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>,
                I: $crate::IntoIterator<Item = P>,
                I::IntoIter: $crate::Clone,
                P: $crate::AsRef<$crate::primitive::str>,
            {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$($coll)? $(:: $colls)* <Inner> as $crate::ParseMetaAppend>::parse_meta_append(inputs, paths)?
                            ),
                            |p| p.0,
                        ),
                    )
                )
            }
        }
    };
}

/// Generates a module for parsing a map collection using `#[deluxe(with = ...)].
///
/// Takes four arguments separated by commas:
/// - The generated module. Can include attributes and a visibility specifier.
/// - The module to use as an inner key parser, relative to the new module.
/// - The module to use as an inner value parser, relative to the new module.
/// - The target collection type.
///
/// # Example
///
/// Defines a new module named `mod_path_hashmap` that parses a
/// [`HashMap`](std::collections::HashMap) mapping paths to quoted expressions using
/// [`with::mod_path`](self::mod_path) for the key.
///
/// ```
/// deluxe_core::define_with_map!(
///     pub mod mod_path_hashmap,
///     deluxe_core::with::mod_path,
///     deluxe_core::with::quoted,
///     std::collections::HashMap<syn::Path, syn::Expr>
/// );
/// ```
#[macro_export]
macro_rules! define_with_map {
    (
        $(#[$attrs:meta])* $vis:vis mod $mod:ident,
        $($key_path:ident)? $(:: $key_path_rest:ident)*,
        $($value_path:ident)? $(:: $value_path_rest:ident)*,
        $($coll:ident)? $(:: $colls:ident)* < $key:ty, $val:ty > $(,)?
    ) => {
        $(#[$attrs])* $vis mod $mod {
            struct InnerKey($key);

            impl $crate::Eq for InnerKey where $key: $crate::Eq {}
            impl $crate::PartialEq for InnerKey where $key: $crate::PartialEq {
                #[inline]
                fn eq(&self, other: &Self) -> $crate::primitive::bool {
                    self.0.eq(&other.0)
                }
            }
            impl $crate::Hash for InnerKey where $key: $crate::Hash {
                #[inline]
                fn hash<H: $crate::Hasher>(&self, state: &mut H) {
                    self.0.hash(state)
                }
            }
            impl $crate::ToKeyString for InnerKey where $key: $crate::ToKeyString {
                #[inline]
                fn fmt_key_string(&self, f: &mut $crate::fmt::Formatter) -> $crate::fmt::Result {
                    self.0.fmt_key_string(f)
                }
                #[inline]
                fn with_key_string<R>(&self, f: impl $crate::ops::FnOnce(&$crate::primitive::str) -> R) -> R {
                    self.0.with_key_string(f)
                }
            }

            impl $crate::ParseMetaItem for InnerKey {
                #[inline]
                fn parse_meta_item(
                    input: $crate::syn::parse::ParseStream,
                    mode: $crate::ParseMode,
                ) -> $crate::Result<Self> {
                    $crate::Result::Ok(Self(
                        $($key_path)? $(::$key_path_rest)* ::parse_meta_item(input, mode)?,
                    ))
                }
            }

            struct InnerValue($val);

            impl $crate::ParseMetaItem for InnerValue {
                #[inline]
                fn parse_meta_item(
                    input: $crate::syn::parse::ParseStream,
                    mode: $crate::ParseMode,
                ) -> $crate::Result<Self> {
                    $crate::Result::Ok(Self(
                        $($value_path)? $(::$value_path_rest)* ::parse_meta_item(input, mode)?,
                    ))
                }
            }

            #[inline]
            pub fn parse_meta_item(
                input: $crate::syn::parse::ParseStream,
                mode: $crate::ParseMode,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$crate::HashMap<InnerKey, InnerValue> as $crate::ParseMetaItem>::parse_meta_item(input, mode)?
                            ),
                            |(k, v)| (k.0, v.0),
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_item_inline<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                mode: $crate::ParseMode,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$crate::HashMap<InnerKey, InnerValue> as $crate::ParseMetaItem>::parse_meta_item_inline(inputs, mode)?
                            ),
                            |(k, v)| (k.0, v.0),
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_item_flag(
                _span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok($crate::Default::default())
            }
            #[inline]
            pub fn parse_meta_item_named(
                input: $crate::syn::parse::ParseStream,
                _name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::parse_named_meta_item_with!(input, span, self)
            }
            #[inline]
            pub fn missing_meta_item(
                name: &$crate::primitive::str,
                span: $crate::Span,
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok($crate::Default::default())
            }
            #[inline]
            pub fn field_count() -> $crate::Option<$crate::primitive::usize> {
                $crate::Option::None
            }
            #[inline]
            pub fn parse_meta_flat_unnamed<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                mode: $crate::ParseMode,
                index: $crate::primitive::usize
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$crate::HashMap<InnerKey, InnerValue> as $crate::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(inputs, mode, index)?
                            ),
                            |(k, v)| (k.0, v.0),
                        ),
                    )
                )
            }
            #[inline]
            pub fn parse_meta_rest<'s, S: $crate::Borrow<$crate::syn::parse::ParseBuffer<'s>>>(
                inputs: &[S],
                exclude: &[&$crate::primitive::str],
            ) -> $crate::Result<$($coll)? $(:: $colls)* <$key, $val>> {
                $crate::Result::Ok(
                    $crate::Iterator::collect(
                        $crate::Iterator::map(
                            $crate::IntoIterator::into_iter(
                                <$crate::HashMap<InnerKey, InnerValue> as $crate::ParseMetaRest>::parse_meta_rest(inputs, exclude)?
                            ),
                            |(k, v)| (k.0, v.0),
                        ),
                    )
                )
            }
        }
    };
}

/// Helpers for parsing any type that implements [`ParseMetaItem`](crate::ParseMetaItem) as itself.
///
/// Only meant to be used from [`define_with_map`](macro@define_with_map).
pub mod identity {
    #![allow(missing_docs)]
    use crate::{ParseMetaItem, ParseMode, Result};
    use std::borrow::Borrow;
    use syn::parse::{ParseBuffer, ParseStream};

    #[inline]
    pub fn parse_meta_item<T: ParseMetaItem>(input: ParseStream, mode: ParseMode) -> Result<T> {
        T::parse_meta_item(input, mode)
    }
    #[inline]
    pub fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>, T: ParseMetaItem>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<T> {
        T::parse_meta_item_inline(inputs, mode)
    }
    #[inline]
    pub fn parse_meta_item_flag<T: ParseMetaItem>(span: proc_macro2::Span) -> Result<T> {
        T::parse_meta_item_flag(span)
    }
    #[inline]
    pub fn parse_meta_item_named<T: ParseMetaItem>(
        input: ParseStream,
        _name: &str,
        span: proc_macro2::Span,
    ) -> Result<T> {
        crate::parse_named_meta_item_with!(input, span, self)
    }
    #[inline]
    pub fn missing_meta_item<T: ParseMetaItem>(name: &str, span: proc_macro2::Span) -> Result<T> {
        T::missing_meta_item(name, span)
    }
}

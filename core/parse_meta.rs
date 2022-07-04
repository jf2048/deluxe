use crate::{parse_helpers::*, Errors, Result};
use proc_macro2::Span;
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    hash::Hash,
};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    punctuated::Punctuated,
    Token,
};

/// Base trait for parsing an attribute out of a [`TokenStream`](proc_macro::TokenStream).
pub trait ParseMetaItem: Sized {
    /// Parse the item in an inline context. The stream can consume any number of commas. Items
    /// with a finite length should not consume a trailing comma.
    fn parse_meta_item(input: ParseStream) -> Result<Self>;
    /// Parse the item in the context of an unnamed field. The default implementation calls
    /// [`Self::parse_meta_item`].
    ///
    /// If the the item is a collection and can parse commas, custom implementations should make
    /// sure to require a top-level delimiter. The
    /// [`ParseDelimited::parse_delimited_meta_item`](crate::parse_helpers::ParseDelimited::parse_delimited_meta_item)
    /// helper is intended to be used here.
    #[inline]
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        Self::parse_meta_item(input)
    }
    /// Parse the item in the context of a named field. The default implementation calls
    /// [`parse_helpers::parse_named_meta_item`](crate::parse_helpers::parse_named_meta_item).
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_meta_item(input)
    }
}

pub trait ParseMetaFlatUnnamed: Sized {
    fn field_count() -> Option<usize>;
    fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self>;
}

pub trait ParseMetaFlatNamed: Sized {
    fn field_names() -> Cow<'static, [&'static str]>;
    fn parse_meta_flat_named(input: ParseStream) -> Result<(Self, Vec<&'static str>)>;
}

macro_rules! impl_parse_meta_item_primitive {
    ($ty:ty, $lit:ty, $conv:ident) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                impl_parse_meta_item_primitive!(@conv input, $lit, $conv)
            }
        }
    };
    (@conv $input:ident, $lit:ty, base10_parse) => {
        Ok($input.parse::<$lit>()?.base10_parse()?)
    };
    (@conv $input:ident, $lit:ty, value) => {
        Ok($input.parse::<$lit>()?.value())
    };
}

impl_parse_meta_item_primitive!(i8, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(i128, syn::LitInt, base10_parse);

impl ParseMetaItem for u8 {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitByte) {
            Ok(input.parse::<syn::LitByte>()?.value())
        } else if lookahead.peek(syn::LitInt) {
            Ok(input.parse::<syn::LitInt>()?.base10_parse()?)
        } else {
            Err(lookahead.error().into())
        }
    }
}

impl_parse_meta_item_primitive!(u16, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u32, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u64, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(u128, syn::LitInt, base10_parse);
impl_parse_meta_item_primitive!(f32, syn::LitFloat, base10_parse);
impl_parse_meta_item_primitive!(f64, syn::LitFloat, base10_parse);

impl ParseMetaItem for bool {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        Ok(input.parse::<syn::LitBool>()?.value())
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_optional_named_meta_item(input).map(|o| o.unwrap_or(true))
    }
}

impl_parse_meta_item_primitive!(String, syn::LitStr, value);
impl_parse_meta_item_primitive!(char, syn::LitChar, value);

impl<T: ParseMetaItem> ParseMetaItem for Option<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        T::parse_meta_item(input).map(Some)
    }
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        mod keywords {
            syn::custom_keyword!(Some);
            syn::custom_keyword!(None);
        }
        let lookahead = input.lookahead1();
        if lookahead.peek(keywords::Some) {
            input.parse::<keywords::Some>()?;
            Paren::parse_delimited_meta_item(input).map(Some)
        } else if lookahead.peek(keywords::None) {
            input.parse::<keywords::None>()?;
            Ok(None)
        } else {
            Err(lookahead.error().into())
        }
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaItem for [T; N] {
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let span = input.span();
        let mut a = arrayvec::ArrayVec::<T, N>::new();
        parse_tuple_struct(input, N, |stream, _| {
            a.push(T::parse_unnamed_meta_item(stream)?);
            Ok(())
        })?;
        let span = input.span().join(span).unwrap_or_else(Span::call_site);
        Ok(a.into_inner().map_err(|a| {
            syn::Error::new(
                span,
                format!("Expected array of length {}, got {}", N, a.len()),
            )
        })?)
    }
    #[inline]
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input)
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_collection(input)
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaFlatUnnamed for [T; N] {
    #[inline]
    fn field_count() -> Option<usize> {
        Some(N)
    }
    #[inline]
    fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
        Self::parse_meta_item(input)
    }
}

macro_rules! impl_parse_meta_item_collection {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                let mut $ident = Self::new();
                loop {
                    if input.is_empty() {
                        break;
                    }
                    let $item = $param::parse_unnamed_meta_item(input)?;
                    $push;
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }
                Ok($ident)
            }
            #[inline]
            fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input)
            }
            #[inline]
            fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
                parse_named_collection(input)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            #[inline]
            fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
                Self::parse_meta_item(input)
            }
        }
    };
}

macro_rules! impl_parse_meta_item_set {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                loop {
                    if input.is_empty() {
                        break;
                    }
                    let span = input.span();
                    let $item = $param::parse_unnamed_meta_item(input)?;
                    let span = input.span().join(span).unwrap();
                    if !$push {
                        errors.push(span, "Duplicate key");
                    }
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }
                errors.check()?;
                Ok($ident)
            }
            #[inline]
            fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input)
            }
            #[inline]
            fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
                parse_named_collection(input)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            #[inline]
            fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
                Self::parse_meta_item(input)
            }
        }
    };
}

macro_rules! impl_parse_meta_item_map {
    (
        $ty:ident <$kp:ident $(: $kbound:tt $(+ $kbounds:tt)*)?, $vp:ident>,
        $ident:ident, $key:ident, $value:ident, $push:expr
    ) => {
        impl<$kp: ParseMetaItem $(+ $kbound $(+ $kbounds)*)?, $vp: ParseMetaItem> ParseMetaItem for $ty <$kp, $vp> {
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                loop {
                    if input.is_empty() {
                        break;
                    }
                    let span = input.span();
                    let $key = $kp::parse_unnamed_meta_item(input)?;
                    let span = input.span().join(span).unwrap();
                    let $value = $vp::parse_named_meta_item(input)?;
                    if !$push {
                        errors.push(span, "Duplicate key");
                    }
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }
                errors.check()?;
                Ok($ident)
            }
            #[inline]
            fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
                Brace::parse_delimited_meta_item(input)
            }
            #[inline]
            fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
                parse_named_collection(input)
            }
        }

        impl<$kp: ParseMetaItem $(+ $kbound $(+ $kbounds)*)?, $vp: ParseMetaItem> ParseMetaFlatNamed for $ty <$kp, $vp> {
            #[inline]
            fn field_names() -> Cow<'static, [&'static str]> {
                Cow::Borrowed(&[])
            }
            #[inline]
            fn parse_meta_flat_named(input: ParseStream) -> Result<(Self, Vec<&'static str>)> {
                Ok((Self::parse_meta_item(input)?, Vec::new()))
            }
        }
    };
}

impl_parse_meta_item_collection!(Vec<T>, v, item, v.push(item));
impl_parse_meta_item_set!(BTreeSet<T: Ord>, set, item, set.insert(item));
impl_parse_meta_item_map!(BTreeMap<K: Ord, V>, map, key, value, map.insert(key, value).is_none());
impl_parse_meta_item_collection!(BinaryHeap<T: Ord>, heap, item, heap.push(item));
impl_parse_meta_item_set!(HashSet<T: Hash + Eq>, set, item, set.insert(item));
impl_parse_meta_item_map!(HashMap<K: Hash + Eq, V>, map, key, value, map.insert(key, value).is_none());
impl_parse_meta_item_collection!(LinkedList<T>, list, item, list.push_back(item));
impl_parse_meta_item_collection!(VecDeque<T>, v, item, v.push_back(item));

impl<T: ParseMetaItem> ParseMetaItem for Box<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        Ok(Self::new(T::parse_meta_item(input)?))
    }
}

impl<T: ParseMetaItem> ParseMetaItem for std::rc::Rc<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        Ok(Self::new(T::parse_meta_item(input)?))
    }
}

impl<T: ParseMetaItem> ParseMetaItem for std::cell::Cell<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        Ok(Self::new(T::parse_meta_item(input)?))
    }
}

impl<T: ParseMetaItem> ParseMetaItem for std::cell::RefCell<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        Ok(Self::new(T::parse_meta_item(input)?))
    }
}

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaItem for Punctuated<T, P> {
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let mut p = Punctuated::new();
        loop {
            if input.is_empty() {
                break;
            }
            p.push(T::parse_meta_item(input)?);
            if !input.is_empty() {
                input.parse::<P>()?;
            }
        }
        Ok(p)
    }
    #[inline]
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input)
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_collection(input)
    }
}

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaFlatUnnamed for Punctuated<T, P> {
    #[inline]
    fn field_count() -> Option<usize> {
        None
    }
    #[inline]
    fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
        Self::parse_meta_item(input)
    }
}

macro_rules! impl_parse_meta_item_syn {
    ($ty:ty) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                Ok(input.parse()?)
            }
        }
    };
}

macro_rules! impl_parse_meta_paren_item_syn {
    ($ty:ty) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                Ok(input.parse()?)
            }
            #[inline]
            fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
                Paren::parse_delimited_meta_item(input)
            }
        }
    };
}

impl_parse_meta_item_syn!(syn::AngleBracketedGenericArguments);
impl_parse_meta_item_syn!(syn::BareFnArg);
impl_parse_meta_item_syn!(syn::BoundLifetimes);
impl_parse_meta_item_syn!(syn::BinOp);
impl_parse_meta_paren_item_syn!(syn::Binding);
impl_parse_meta_item_syn!(syn::Expr);
impl_parse_meta_item_syn!(syn::ExprArray);
impl_parse_meta_paren_item_syn!(syn::ExprAssign);
impl_parse_meta_item_syn!(syn::ExprCall);
impl_parse_meta_item_syn!(syn::ExprCast);
impl_parse_meta_item_syn!(syn::ExprField);
impl_parse_meta_item_syn!(syn::ExprIndex);
impl_parse_meta_item_syn!(syn::ExprLit);
impl_parse_meta_item_syn!(syn::ExprMethodCall);
impl_parse_meta_item_syn!(syn::ExprParen);
impl_parse_meta_item_syn!(syn::ExprPath);
impl_parse_meta_item_syn!(syn::ExprRange);
impl_parse_meta_item_syn!(syn::ExprRepeat);
impl_parse_meta_item_syn!(syn::ExprTuple);
impl_parse_meta_item_syn!(syn::ExprType);
impl_parse_meta_item_syn!(syn::FnArg);
impl_parse_meta_item_syn!(syn::GenericParam);
impl_parse_meta_item_syn!(syn::Ident);
impl_parse_meta_item_syn!(syn::Lifetime);
impl_parse_meta_item_syn!(syn::LifetimeDef);
impl_parse_meta_item_syn!(syn::Lit);
impl_parse_meta_item_syn!(syn::LitStr);
impl_parse_meta_item_syn!(syn::LitByteStr);
impl_parse_meta_item_syn!(syn::LitByte);
impl_parse_meta_item_syn!(syn::LitChar);
impl_parse_meta_item_syn!(syn::LitInt);
impl_parse_meta_item_syn!(syn::LitFloat);
impl_parse_meta_item_syn!(syn::LitBool);
impl_parse_meta_item_syn!(syn::MetaList);
impl_parse_meta_paren_item_syn!(syn::Meta);
impl_parse_meta_paren_item_syn!(syn::MetaNameValue);
impl_parse_meta_paren_item_syn!(syn::NestedMeta);
impl_parse_meta_item_syn!(syn::Pat);
impl_parse_meta_item_syn!(syn::Path);
impl_parse_meta_item_syn!(syn::PathSegment);
impl_parse_meta_item_syn!(syn::ParenthesizedGenericArguments);
impl_parse_meta_item_syn!(syn::Receiver);
impl_parse_meta_item_syn!(syn::Signature);
impl_parse_meta_item_syn!(syn::TraitBound);
impl_parse_meta_item_syn!(syn::Type);
impl_parse_meta_item_syn!(syn::TypeArray);
impl_parse_meta_item_syn!(syn::TypeBareFn);
impl_parse_meta_item_syn!(syn::TypeImplTrait);
impl_parse_meta_item_syn!(syn::TypePath);
impl_parse_meta_item_syn!(syn::TypePtr);
impl_parse_meta_item_syn!(syn::TypeReference);
impl_parse_meta_item_syn!(syn::TypeSlice);
impl_parse_meta_item_syn!(syn::TypeTraitObject);
impl_parse_meta_item_syn!(syn::TypeTuple);
impl_parse_meta_item_syn!(syn::TypeParamBound);
impl_parse_meta_item_syn!(syn::UseTree);
impl_parse_meta_item_syn!(syn::UnOp);
impl_parse_meta_item_syn!(syn::Visibility);
impl_parse_meta_item_syn!(syn::WherePredicate);

impl ParseMetaItem for () {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let content = Paren::parse_delimited(input)?;
        content.parse::<Nothing>()?;
        Ok(())
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        if input.is_empty() || input.peek(Token![,]) {
            return Ok(());
        }
        let lookahead = input.lookahead1();
        lookahead.peek(Token![,]);
        Err(lookahead.error().into())
    }
}

macro_rules! impl_parse_meta_item_tuple {
    ($len:literal: $($index:literal $param:tt $item:ident)+) => {
        impl<$($param: ParseMetaItem,)+> ParseMetaItem for ($($param,)+) {
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                let span = input.span();
                $(let mut $item = None;)+
                parse_tuple_struct(input, $len, |stream, index| {
                    match index {
                        $($index => $item = Some($param::parse_unnamed_meta_item(stream)?),)+
                        _ => unreachable!(),
                    }
                    Ok(())
                })?;
                let span = input.span().join(span).unwrap_or_else(Span::call_site);
                $(let $item = match $item {
                    Some(t) => t,
                    None => return Err(syn::Error::new(
                        span,
                        concat!("Expected tuple of length ", $len, ", got ", $index),
                    )),
                };)+
                Ok(($($item,)+))
            }
            #[inline]
            fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
                Paren::parse_delimited_meta_item(input)
            }
            #[inline]
            fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
                parse_named_collection(input)
            }
        }

        impl<$($param: ParseMetaItem,)+> ParseMetaFlatUnnamed for ($($param,)+) {
            #[inline]
            fn field_count() -> Option<usize> {
                Some($len)
            }
            #[inline]
            fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
                Self::parse_meta_item(input)
            }
        }
    };
}

impl_parse_meta_item_tuple!(1: 0 T0 t0);
impl_parse_meta_item_tuple!(2: 0 T0 t0 1 T1 t1);
impl_parse_meta_item_tuple!(3: 0 T0 t0 1 T1 t1 2 T2 t2);
impl_parse_meta_item_tuple!(4: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3);
impl_parse_meta_item_tuple!(5: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4);
impl_parse_meta_item_tuple!(6: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5);
impl_parse_meta_item_tuple!(7: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6);
impl_parse_meta_item_tuple!(8: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7);
impl_parse_meta_item_tuple!(9: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8);
impl_parse_meta_item_tuple!(10: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9);
impl_parse_meta_item_tuple!(11: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10);
impl_parse_meta_item_tuple!(12: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11);
impl_parse_meta_item_tuple!(13: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12);
impl_parse_meta_item_tuple!(14: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13);
impl_parse_meta_item_tuple!(15: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13 14 T14 t14);
impl_parse_meta_item_tuple!(16: 0 T0 t0 1 T1 t1 2 T2 t2 3 T3 t3 4 T4 t4 5 T5 t5 6 T6 t6 7 T7 t7 8 T8 t8 9 T9 t9 10 T10 t10 11 T11 t11 12 T12 t12 13 T13 t13 14 T14 t14 15 T15 t15);


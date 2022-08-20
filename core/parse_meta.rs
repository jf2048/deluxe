use crate::{parse_helpers::*, Errors, Result};
use proc_macro2::Span;
use std::{
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    hash::Hash,
};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    punctuated::Punctuated,
    Token,
};

pub enum ParseMode {
    Named,
    Unnamed,
}

/// Base trait for parsing a single field out of [`syn::parse::ParseStream`].
pub trait ParseMetaItem: Sized {
    /// Parse the item. If the item can contain commas, it should be wrapped in a pair of
    /// delimiters.
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self>;
    /// Parse the item in an inline context. The stream can consume any number of commas. Items
    /// with a finite length should not consume a trailing comma. The default implementation calls
    /// [`Self::parse_meta_item`].
    #[inline]
    fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Self::parse_meta_item(input, _mode)
    }
    #[inline]
    fn parse_meta_item_flag(_span: Span) -> Result<Self> {
        Err(flag_disallowed_error(_span))
    }
}

pub trait ParseMetaFlatUnnamed: Sized {
    fn field_count() -> Option<usize>;
    fn parse_meta_flat_unnamed(inputs: &[ParseStream], index: usize) -> Result<Self>;
}

pub trait ParseMetaFlatNamed: Sized {
    fn field_names() -> &'static [&'static str];
    fn parse_meta_flat_named(inputs: &[ParseStream], prefix: &str, validate: bool) -> Result<Self>;
    const ACCEPTS_ALL: bool = false;
}

pub trait ParseMetaAppend: Sized {
    fn parse_meta_append<I, S>(inputs: &[ParseStream], paths: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        I::IntoIter: Clone,
        S: AsRef<str>;
}

pub trait ParseMetaRest: Sized {
    fn parse_meta_rest(inputs: &[ParseStream]) -> Result<Self>;
}

macro_rules! impl_parse_meta_item_primitive {
    ($ty:ty, $lit:ty, $conv:ident) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                impl_parse_meta_item_primitive!(@conv input, $lit, $conv)
            }
        }
    };
    (@conv $input:ident, $lit:ty, base10_parse) => {
        $input.parse::<$lit>()?.base10_parse()
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
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitByte) {
            Ok(input.parse::<syn::LitByte>()?.value())
        } else if lookahead.peek(syn::LitInt) {
            Ok(input.parse::<syn::LitInt>()?.base10_parse()?)
        } else {
            Err(lookahead.error())
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
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(input.parse::<syn::LitBool>()?.value())
    }
    #[inline]
    fn parse_meta_item_flag(_: Span) -> Result<Self> {
        Ok(true)
    }
}

impl_parse_meta_item_primitive!(String, syn::LitStr, value);
impl_parse_meta_item_primitive!(char, syn::LitChar, value);

impl<T: ParseMetaItem> ParseMetaItem for Option<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        match mode {
            ParseMode::Named => T::parse_meta_item(input, mode).map(Some),
            ParseMode::Unnamed => {
                mod keywords {
                    syn::custom_keyword!(Some);
                    syn::custom_keyword!(None);
                }
                let lookahead = input.lookahead1();
                if lookahead.peek(keywords::Some) {
                    input.parse::<keywords::Some>()?;
                    Paren::parse_delimited_meta_item(input, mode).map(Some)
                } else if lookahead.peek(keywords::None) {
                    input.parse::<keywords::None>()?;
                    Ok(None)
                } else {
                    Err(lookahead.error())
                }
            }
        }
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        T::parse_meta_item_flag(span).map(Some)
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaItem for [T; N] {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
    }
    #[inline]
    fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Self::parse_meta_flat_unnamed(&[input], 0)
    }
}

impl<T: ParseMetaItem, const N: usize> ParseMetaFlatUnnamed for [T; N] {
    #[inline]
    fn field_count() -> Option<usize> {
        Some(N)
    }
    fn parse_meta_flat_unnamed(inputs: &[ParseStream], index: usize) -> Result<Self> {
        let mut a = arrayvec::ArrayVec::<T, N>::new();
        parse_tuple_struct(inputs, N, |stream, _, _| {
            a.push(T::parse_meta_item(stream, ParseMode::Unnamed)?);
            Ok(())
        })?;
        a.into_inner().map_err(|a| {
            syn::Error::new(
                inputs_span(inputs),
                format!(
                    "Expected array at index {} of length {}, got {}",
                    index,
                    N,
                    a.len()
                ),
            )
        })
    }
}

macro_rules! impl_parse_meta_item_collection {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(&[input], 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed(inputs: &[ParseStream], _index: usize) -> Result<Self> {
                let mut $ident = Self::new();
                for input in inputs {
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let $item = $param::parse_meta_item(input, ParseMode::Unnamed)?;
                        $push;
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                Ok($ident)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaAppend for $ty <$param> {
            fn parse_meta_append<I, S>(
                inputs: &[ParseStream],
                paths: I,
            ) -> Result<Self>
            where
                I: IntoIterator<Item = S>,
                I::IntoIter: Clone,
                S: AsRef<str>
            {
                let mut $ident = Self::new();
                let errors = Errors::new();
                let paths = paths.into_iter();
                parse_struct(inputs.iter().cloned(), |input, p, _| {
                    if paths.clone().any(|path| path.as_ref() == p) {
                        let $item = parse_named_meta_item(input)?;
                        $push;
                    } else {
                        skip_named_meta_item(input);
                    }
                    Ok(())
                })?;
                errors.check()?;
                Ok($ident)
            }
        }
    };
}

macro_rules! impl_parse_meta_item_set {
    ($ty:ident <$param:ident $(: $bound:tt $(+ $bounds:tt)*)?>, $ident:ident, $item:ident, $push:expr) => {
        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaItem for $ty <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(&[input], 0)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaFlatUnnamed for $ty <$param> {
            #[inline]
            fn field_count() -> Option<usize> {
                None
            }
            fn parse_meta_flat_unnamed(inputs: &[ParseStream], _index: usize) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let span = input.span();
                        let $item = $param::parse_meta_item(input, ParseMode::Unnamed)?;
                        let span = input.span().join(span).unwrap();
                        if !$push {
                            errors.push(span, "Duplicate key");
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
            }
        }

        impl<$param: ParseMetaItem $(+ $bound $(+ $bounds)*)?> ParseMetaAppend for $ty <$param> {
            fn parse_meta_append<I, S>(
                inputs: &[ParseStream],
                paths: I,
            ) -> Result<Self>
            where
                I: IntoIterator<Item = S>,
                I::IntoIter: Clone,
                S: AsRef<str>
            {
                let errors = Errors::new();
                let mut $ident = Self::new();
                let paths = paths.into_iter();
                parse_struct(inputs.iter().cloned(), |input, p, _| {
                    if paths.clone().any(|path| path.as_ref() == p) {
                        let span = input.span();
                        let $item = parse_named_meta_item(input)?;
                        let span = input.span().join(span).unwrap();
                        if !$push {
                            errors.push(span, "Duplicate key");
                        }
                    } else {
                        skip_named_meta_item(input);
                    }
                    Ok(())
                })?;
                errors.check()?;
                Ok($ident)
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
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Brace::parse_delimited_meta_item(input, ParseMode::Named)
            }
            #[inline]
            fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_rest(&[input])
            }
        }

        impl<$kp: ParseMetaItem $(+ $kbound $(+ $kbounds)*)?, $vp: ParseMetaItem> ParseMetaRest for $ty <$kp, $vp> {
            fn parse_meta_rest(
                inputs: &[ParseStream],
            ) -> Result<Self> {
                let mut $ident = Self::new();
                let errors = Errors::new();
                for input in inputs {
                    loop {
                        if input.is_empty() {
                            break;
                        }
                        let span = input.span();
                        let $key = $kp::parse_meta_item(input, ParseMode::Unnamed)?;
                        let span = input.span().join(span).unwrap();
                        let $value = parse_named_meta_item(input)?;
                        if !$push {
                            errors.push(span, "Duplicate key");
                        }
                        if !input.is_empty() {
                            input.parse::<Token![,]>()?;
                        }
                    }
                }
                errors.check()?;
                Ok($ident)
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

macro_rules! impl_parse_meta_item_wrapper {
    ($i:ident $(::$ip:ident)* <$param:ident>) => {
        impl<$param: ParseMetaItem> ParseMetaItem for $i $(::$ip)* <$param> {
            #[inline]
            fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
                Ok(Self::new($param::parse_meta_item(input, mode)?))
            }
            #[inline]
            fn parse_meta_item_flag(span: Span) -> Result<Self> {
                $param::parse_meta_item_flag(span).map(Self::new)
            }
        }
    };
}

impl_parse_meta_item_wrapper!(Box<T>);
impl_parse_meta_item_wrapper!(std::rc::Rc<T>);
impl_parse_meta_item_wrapper!(std::cell::Cell<T>);
impl_parse_meta_item_wrapper!(std::cell::RefCell<T>);

impl<'t, T: ParseMetaItem + Clone> ParseMetaItem for std::borrow::Cow<'t, T> {
    #[inline]
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        Ok(Self::Owned(T::parse_meta_item(input, mode)?))
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        T::parse_meta_item_flag(span).map(Self::Owned)
    }
}

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaItem for Punctuated<T, P> {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Bracket::parse_delimited_meta_item(input, ParseMode::Unnamed)
    }
    #[inline]
    fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Self::parse_meta_flat_unnamed(&[input], 0)
    }
}

impl<T: ParseMetaItem, P: Parse + Default> ParseMetaFlatUnnamed for Punctuated<T, P> {
    #[inline]
    fn field_count() -> Option<usize> {
        None
    }
    fn parse_meta_flat_unnamed(inputs: &[ParseStream], _index: usize) -> Result<Self> {
        let mut p = Punctuated::new();
        for input in inputs {
            loop {
                if input.is_empty() {
                    break;
                }
                p.push(T::parse_meta_item(input, ParseMode::Unnamed)?);
                if !input.is_empty() {
                    input.parse::<P>()?;
                }
            }
        }
        Ok(p)
    }
}

macro_rules! impl_parse_meta_item_syn {
    ($ty:ty) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                input.parse()
            }
        }
    };
}

macro_rules! impl_parse_meta_paren_item_syn {
    ($ty:ty) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
                match mode {
                    ParseMode::Named => {
                        let content = Paren::parse_delimited(input)?;
                        let ret = content.parse()?;
                        content.parse::<Nothing>()?;
                        Ok(ret)
                    }
                    ParseMode::Unnamed => Ok(input.parse()?),
                }
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
impl ParseMetaItem for syn::Ident {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        syn::ext::IdentExt::parse_any(input)
    }
}
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
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        let content = Paren::parse_delimited(input)?;
        content.parse::<Nothing>()?;
        Ok(())
    }
    #[inline]
    fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        input.parse::<Nothing>().map(|_| ())
    }
    #[inline]
    fn parse_meta_item_flag(_: Span) -> Result<Self> {
        Ok(())
    }
}

macro_rules! impl_parse_meta_item_tuple {
    ($len:literal: $($index:literal $param:tt $item:ident)+) => {
        impl<$($param: ParseMetaItem,)+> ParseMetaItem for ($($param,)+) {
            #[inline]
            fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Paren::parse_delimited_meta_item(input, ParseMode::Unnamed)
            }
            #[inline]
            fn parse_meta_item_inline(input: ParseStream, _mode: ParseMode) -> Result<Self> {
                Self::parse_meta_flat_unnamed(&[input], 0)
            }
        }

        impl<$($param: ParseMetaItem,)+> ParseMetaFlatUnnamed for ($($param,)+) {
            #[inline]
            fn field_count() -> Option<usize> {
                Some($len)
            }
            fn parse_meta_flat_unnamed(inputs: &[ParseStream], _index: usize) -> Result<Self> {
                $(let mut $item = None;)+
                parse_tuple_struct(inputs, $len, |stream, _, index| {
                    match index {
                        $($index => $item = Some($param::parse_meta_item(stream, ParseMode::Unnamed)?),)+
                        _ => unreachable!(),
                    }
                    Ok(())
                })?;
                $(let $item = match $item {
                    Some(t) => t,
                    None => return Err(syn::Error::new(
                        inputs_span(inputs),
                        concat!("Expected tuple of length ", $len, ", got ", $index),
                    )),
                };)+
                Ok(($($item,)+))
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

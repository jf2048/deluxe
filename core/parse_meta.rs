use crate::{parse_helpers::*, Errors, Result};
use std::{borrow::Cow, collections::HashMap, hash::Hash};
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

pub trait ParseMetaFlatNamed: Sized {
    fn field_names() -> Cow<'static, [&'static str]>;
    fn parse_meta_flat_named(input: ParseStream) -> Result<(Self, Vec<&'static str>)>;
}

pub trait ParseMetaFlatUnnamed: Sized {
    fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self>;
}

macro_rules! impl_parse_meta_item_prim {
    ($ty:ty, $lit:ty, $conv:ident) => {
        impl ParseMetaItem for $ty {
            #[inline]
            fn parse_meta_item(input: ParseStream) -> Result<Self> {
                impl_parse_meta_item_prim!(@conv input, $lit, $conv)
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

impl_parse_meta_item_prim!(i8, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(i16, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(i32, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(i64, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(i128, syn::LitInt, base10_parse);

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

impl_parse_meta_item_prim!(u16, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(u32, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(u64, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(u128, syn::LitInt, base10_parse);
impl_parse_meta_item_prim!(f32, syn::LitFloat, base10_parse);
impl_parse_meta_item_prim!(f64, syn::LitFloat, base10_parse);

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

impl_parse_meta_item_prim!(String, syn::LitStr, value);
impl_parse_meta_item_prim!(char, syn::LitChar, value);

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

impl<T: ParseMetaItem> ParseMetaItem for Vec<T> {
    #[inline]
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let mut v = Vec::new();
        loop {
            if input.is_empty() {
                break;
            }
            v.push(T::parse_meta_item(input)?);
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        Ok(v)
    }
    #[inline]
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        let content = Bracket::parse_delimited(input)?;
        Self::parse_meta_item(&content)
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_collection(input)
    }
}

impl<T: ParseMetaItem> ParseMetaFlatUnnamed for Vec<T> {
    #[inline]
    fn parse_meta_flat_unnamed(input: ParseStream) -> Result<Self> {
        Self::parse_meta_item(input)
    }
}

impl<K, V> ParseMetaItem for HashMap<K, V>
where
    K: ParseMetaItem + Hash + Eq,
    V: ParseMetaItem,
{
    fn parse_meta_item(input: ParseStream) -> Result<Self> {
        let mut ht = HashMap::new();
        let errors = Errors::new();
        loop {
            if input.is_empty() {
                break;
            }
            let span = input.span();
            let key = K::parse_meta_item(input)?;
            let span = input.span().join(span).unwrap();
            let value = parse_named_meta_item(input)?;
            if ht.contains_key(&key) {
                errors.push(span, "Duplicate key");
            } else if errors.is_empty() {
                ht.insert(key, value);
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        errors.check()?;
        Ok(ht)
    }
    #[inline]
    fn parse_unnamed_meta_item(input: ParseStream) -> Result<Self> {
        let content = Brace::parse_delimited(input)?;
        Self::parse_meta_item(&content)
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_collection(input)
    }
}

impl<K, V> ParseMetaFlatNamed for HashMap<K, V>
where
    K: ParseMetaItem + Hash + Eq,
    V: ParseMetaItem,
{
    #[inline]
    fn field_names() -> Cow<'static, [&'static str]> {
        Cow::Borrowed(&[])
    }
    #[inline]
    fn parse_meta_flat_named(input: ParseStream) -> Result<(Self, Vec<&'static str>)> {
        Ok((Self::parse_meta_item(input)?, Vec::new()))
    }
}

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
        let content = Bracket::parse_delimited(input)?;
        Self::parse_meta_item(&content)
    }
    #[inline]
    fn parse_named_meta_item(input: ParseStream) -> Result<Self> {
        parse_named_collection(input)
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

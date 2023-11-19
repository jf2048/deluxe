use crate::Result;
use proc_macro2::Span;
use syn::spanned::Spanned;

/// Parses a structure out of a [`syn::Attribute`] list.
///
/// This trait is intended for types that may share a set of matching attributes with other types.
pub trait ParseAttributes<'t, T: HasAttributes>: Sized {
    /// Checks if a given attribute path can be parsed by this type.
    fn path_matches(path: &syn::Path) -> bool;
    /// Iterates the attributes from a `T`, parsing any that match.
    ///
    /// Implementations should use the [`ref_tokens`](crate::parse_helpers::ref_tokens) helper to
    /// automatically get references to any matching `TokenStream`s, and then parse them using
    /// [`parse_struct_attr_tokens`](crate::parse_helpers::parse_struct_attr_tokens).
    fn parse_attributes(obj: &'t T) -> Result<Self>;
}

/// Extracts a structure out of a [`syn::Attribute`] list.
///
/// This trait is intended for types that "consume" attributes from another syntax tree node.
pub trait ExtractAttributes<T: HasAttributes>: Sized {
    /// Checks if a given attribute path can be extracted by this type.
    fn path_matches(path: &syn::Path) -> bool;
    /// Iterates the attributes from a `T`, extracting and parsing any that match.
    ///
    /// Implementations should use the [`take_tokens`](crate::parse_helpers::take_tokens) helper to
    /// automatically extract any matching `TokenStream`s, and then parse them using
    /// [`parse_struct_attr_tokens`](crate::parse_helpers::parse_struct_attr_tokens).
    fn extract_attributes(obj: &mut T) -> Result<Self>;
}

/// Trait for a [`syn`] type containing a list of attributes.
///
/// Implementations are provided for all [`syn`] types containing a
/// <code>[Vec]&lt;[syn::Attribute]></code>.
pub trait HasAttributes {
    /// Returns an immutable slice of attributes.
    fn attrs(&self) -> &[syn::Attribute];
    /// Returns a mutable [`Vec`] of attributes.
    ///
    /// Returns [`Err`] if the type does not support mutable attributes.
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>>;
}

impl HasAttributes for syn::Attribute {
    #[inline]
    fn attrs(&self) -> &[syn::Attribute] {
        std::slice::from_ref(self)
    }
    #[inline]
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        Err(syn::Error::new(
            self.span(),
            "`HasAttributes::attrs_mut()` not supported on single `syn::Attribute`, try moving it into a `Vec<syn::Attribute>`"
        ))
    }
}

impl HasAttributes for [syn::Attribute] {
    #[inline]
    fn attrs(&self) -> &[syn::Attribute] {
        self
    }
    #[inline]
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        let span = self
            .iter()
            .fold(None, |s, a| {
                Some(s.and_then(|s| a.span().join(s)).unwrap_or_else(|| a.span()))
            })
            .unwrap_or_else(Span::call_site);
        Err(syn::Error::new(
            span,
            "`HasAttributes::attrs_mut()` not supported on immutable `[syn::Attribute]`, try using a `Vec<syn::Attribute>`"
        ))
    }
}

impl HasAttributes for Vec<syn::Attribute> {
    #[inline]
    fn attrs(&self) -> &[syn::Attribute] {
        self.as_slice()
    }
    #[inline]
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        Ok(self)
    }
}

macro_rules! impl_has_attributes {
    ($(#[$attr:meta])* $ty:ty) => {
        $(#[$attr])*
        impl HasAttributes for $ty {
            #[inline]
            fn attrs(&self) -> &[syn::Attribute] {
                &self.attrs
            }
            #[inline]
            fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
                Ok(&mut self.attrs)
            }
        }
    };
    ($ty:ty, #full) => {
        impl_has_attributes!(
            #[cfg(feature = "full")]
            #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
            $ty
        );
    };
}

impl_has_attributes!(syn::Arm, #full);
impl_has_attributes!(syn::BareFnArg);
impl_has_attributes!(syn::ConstParam);
impl_has_attributes!(syn::DeriveInput);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::Expr {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Array(expr) => &expr.attrs,
            Self::Assign(expr) => &expr.attrs,
            Self::Async(expr) => &expr.attrs,
            Self::Await(expr) => &expr.attrs,
            Self::Binary(expr) => &expr.attrs,
            Self::Block(expr) => &expr.attrs,
            Self::Break(expr) => &expr.attrs,
            Self::Call(expr) => &expr.attrs,
            Self::Cast(expr) => &expr.attrs,
            Self::Closure(expr) => &expr.attrs,
            Self::Const(expr) => &expr.attrs,
            Self::Continue(expr) => &expr.attrs,
            Self::Field(expr) => &expr.attrs,
            Self::ForLoop(expr) => &expr.attrs,
            Self::Group(expr) => &expr.attrs,
            Self::If(expr) => &expr.attrs,
            Self::Index(expr) => &expr.attrs,
            Self::Infer(expr) => &expr.attrs,
            Self::Let(expr) => &expr.attrs,
            Self::Lit(expr) => &expr.attrs,
            Self::Loop(expr) => &expr.attrs,
            Self::Macro(expr) => &expr.attrs,
            Self::Match(expr) => &expr.attrs,
            Self::MethodCall(expr) => &expr.attrs,
            Self::Paren(expr) => &expr.attrs,
            Self::Path(expr) => &expr.attrs,
            Self::Range(expr) => &expr.attrs,
            Self::Reference(expr) => &expr.attrs,
            Self::Repeat(expr) => &expr.attrs,
            Self::Return(expr) => &expr.attrs,
            Self::Struct(expr) => &expr.attrs,
            Self::Try(expr) => &expr.attrs,
            Self::TryBlock(expr) => &expr.attrs,
            Self::Tuple(expr) => &expr.attrs,
            Self::Unary(expr) => &expr.attrs,
            Self::Unsafe(expr) => &expr.attrs,
            Self::While(expr) => &expr.attrs,
            Self::Yield(expr) => &expr.attrs,
            _ => &[],
        }
    }

    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Array(expr) => Ok(&mut expr.attrs),
            Self::Assign(expr) => Ok(&mut expr.attrs),
            Self::Async(expr) => Ok(&mut expr.attrs),
            Self::Await(expr) => Ok(&mut expr.attrs),
            Self::Binary(expr) => Ok(&mut expr.attrs),
            Self::Block(expr) => Ok(&mut expr.attrs),
            Self::Break(expr) => Ok(&mut expr.attrs),
            Self::Call(expr) => Ok(&mut expr.attrs),
            Self::Cast(expr) => Ok(&mut expr.attrs),
            Self::Closure(expr) => Ok(&mut expr.attrs),
            Self::Const(expr) => Ok(&mut expr.attrs),
            Self::Continue(expr) => Ok(&mut expr.attrs),
            Self::Field(expr) => Ok(&mut expr.attrs),
            Self::ForLoop(expr) => Ok(&mut expr.attrs),
            Self::Group(expr) => Ok(&mut expr.attrs),
            Self::If(expr) => Ok(&mut expr.attrs),
            Self::Index(expr) => Ok(&mut expr.attrs),
            Self::Let(expr) => Ok(&mut expr.attrs),
            Self::Lit(expr) => Ok(&mut expr.attrs),
            Self::Loop(expr) => Ok(&mut expr.attrs),
            Self::Macro(expr) => Ok(&mut expr.attrs),
            Self::Match(expr) => Ok(&mut expr.attrs),
            Self::MethodCall(expr) => Ok(&mut expr.attrs),
            Self::Paren(expr) => Ok(&mut expr.attrs),
            Self::Path(expr) => Ok(&mut expr.attrs),
            Self::Range(expr) => Ok(&mut expr.attrs),
            Self::Reference(expr) => Ok(&mut expr.attrs),
            Self::Repeat(expr) => Ok(&mut expr.attrs),
            Self::Return(expr) => Ok(&mut expr.attrs),
            Self::Struct(expr) => Ok(&mut expr.attrs),
            Self::Try(expr) => Ok(&mut expr.attrs),
            Self::TryBlock(expr) => Ok(&mut expr.attrs),
            Self::Tuple(expr) => Ok(&mut expr.attrs),
            Self::Unary(expr) => Ok(&mut expr.attrs),
            Self::Unsafe(expr) => Ok(&mut expr.attrs),
            Self::While(expr) => Ok(&mut expr.attrs),
            Self::Yield(expr) => Ok(&mut expr.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::Expr::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::Expr` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::ExprArray, #full);
impl_has_attributes!(syn::ExprAssign, #full);
impl_has_attributes!(syn::ExprAsync, #full);
impl_has_attributes!(syn::ExprAwait, #full);
impl_has_attributes!(syn::ExprBinary, #full);
impl_has_attributes!(syn::ExprBlock, #full);
impl_has_attributes!(syn::ExprBreak, #full);
impl_has_attributes!(syn::ExprCall, #full);
impl_has_attributes!(syn::ExprCast, #full);
impl_has_attributes!(syn::ExprClosure, #full);
impl_has_attributes!(syn::ExprConst, #full);
impl_has_attributes!(syn::ExprContinue, #full);
impl_has_attributes!(syn::ExprField, #full);
impl_has_attributes!(syn::ExprForLoop, #full);
impl_has_attributes!(syn::ExprGroup, #full);
impl_has_attributes!(syn::ExprIf, #full);
impl_has_attributes!(syn::ExprIndex, #full);
impl_has_attributes!(syn::ExprInfer, #full);
impl_has_attributes!(syn::ExprLet, #full);
impl_has_attributes!(syn::ExprLit, #full);
impl_has_attributes!(syn::ExprLoop, #full);
impl_has_attributes!(syn::ExprMacro, #full);
impl_has_attributes!(syn::ExprMatch, #full);
impl_has_attributes!(syn::ExprMethodCall, #full);
impl_has_attributes!(syn::ExprParen, #full);
impl_has_attributes!(syn::ExprPath, #full);
impl_has_attributes!(syn::ExprRange, #full);
impl_has_attributes!(syn::ExprReference, #full);
impl_has_attributes!(syn::ExprRepeat, #full);
impl_has_attributes!(syn::ExprReturn, #full);
impl_has_attributes!(syn::ExprStruct, #full);
impl_has_attributes!(syn::ExprTry, #full);
impl_has_attributes!(syn::ExprTryBlock, #full);
impl_has_attributes!(syn::ExprTuple, #full);
impl_has_attributes!(syn::ExprUnary, #full);
impl_has_attributes!(syn::ExprUnsafe, #full);
impl_has_attributes!(syn::ExprWhile, #full);
impl_has_attributes!(syn::ExprYield, #full);
impl_has_attributes!(syn::Field);
impl_has_attributes!(syn::FieldPat, #full);
impl_has_attributes!(syn::FieldValue, #full);
impl_has_attributes!(syn::File, #full);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::ForeignItem {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Fn(item) => &item.attrs,
            Self::Static(item) => &item.attrs,
            Self::Type(item) => &item.attrs,
            Self::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Fn(item) => Ok(&mut item.attrs),
            Self::Static(item) => Ok(&mut item.attrs),
            Self::Type(item) => Ok(&mut item.attrs),
            Self::Macro(item) => Ok(&mut item.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::ForeignItem::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::ForeignItem` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::ForeignItemFn, #full);
impl_has_attributes!(syn::ForeignItemMacro, #full);
impl_has_attributes!(syn::ForeignItemStatic, #full);
impl_has_attributes!(syn::ForeignItemType, #full);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::ImplItem {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Const(item) => &item.attrs,
            Self::Fn(item) => &item.attrs,
            Self::Type(item) => &item.attrs,
            Self::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Const(item) => Ok(&mut item.attrs),
            Self::Fn(item) => Ok(&mut item.attrs),
            Self::Type(item) => Ok(&mut item.attrs),
            Self::Macro(item) => Ok(&mut item.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::ImplItem::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::ImplItem` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::ImplItemConst, #full);
impl_has_attributes!(syn::ImplItemFn, #full);
impl_has_attributes!(syn::ImplItemMacro, #full);
impl_has_attributes!(syn::ImplItemType, #full);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::Item {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Const(expr) => &expr.attrs,
            Self::Enum(expr) => &expr.attrs,
            Self::ExternCrate(expr) => &expr.attrs,
            Self::Fn(expr) => &expr.attrs,
            Self::ForeignMod(expr) => &expr.attrs,
            Self::Impl(expr) => &expr.attrs,
            Self::Macro(expr) => &expr.attrs,
            Self::Mod(expr) => &expr.attrs,
            Self::Static(expr) => &expr.attrs,
            Self::Struct(expr) => &expr.attrs,
            Self::Trait(expr) => &expr.attrs,
            Self::TraitAlias(expr) => &expr.attrs,
            Self::Type(expr) => &expr.attrs,
            Self::Union(expr) => &expr.attrs,
            Self::Use(expr) => &expr.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Const(expr) => Ok(&mut expr.attrs),
            Self::Enum(expr) => Ok(&mut expr.attrs),
            Self::ExternCrate(expr) => Ok(&mut expr.attrs),
            Self::Fn(expr) => Ok(&mut expr.attrs),
            Self::ForeignMod(expr) => Ok(&mut expr.attrs),
            Self::Impl(expr) => Ok(&mut expr.attrs),
            Self::Macro(expr) => Ok(&mut expr.attrs),
            Self::Mod(expr) => Ok(&mut expr.attrs),
            Self::Static(expr) => Ok(&mut expr.attrs),
            Self::Struct(expr) => Ok(&mut expr.attrs),
            Self::Trait(expr) => Ok(&mut expr.attrs),
            Self::TraitAlias(expr) => Ok(&mut expr.attrs),
            Self::Type(expr) => Ok(&mut expr.attrs),
            Self::Union(expr) => Ok(&mut expr.attrs),
            Self::Use(expr) => Ok(&mut expr.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::Item::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::Item` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::ItemConst, #full);
impl_has_attributes!(syn::ItemEnum, #full);
impl_has_attributes!(syn::ItemExternCrate, #full);
impl_has_attributes!(syn::ItemFn, #full);
impl_has_attributes!(syn::ItemForeignMod, #full);
impl_has_attributes!(syn::ItemImpl, #full);
impl_has_attributes!(syn::ItemMacro, #full);
impl_has_attributes!(syn::ItemMod, #full);
impl_has_attributes!(syn::ItemStatic, #full);
impl_has_attributes!(syn::ItemStruct, #full);
impl_has_attributes!(syn::ItemTrait, #full);
impl_has_attributes!(syn::ItemTraitAlias, #full);
impl_has_attributes!(syn::ItemType, #full);
impl_has_attributes!(syn::ItemUnion, #full);
impl_has_attributes!(syn::ItemUse, #full);
impl_has_attributes!(syn::LifetimeParam);
impl_has_attributes!(syn::Local, #full);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::Pat {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Const(expr) => &expr.attrs,
            Self::Ident(expr) => &expr.attrs,
            Self::Lit(expr) => &expr.attrs,
            Self::Macro(expr) => &expr.attrs,
            Self::Or(expr) => &expr.attrs,
            Self::Paren(expr) => &expr.attrs,
            Self::Path(expr) => &expr.attrs,
            Self::Range(expr) => &expr.attrs,
            Self::Reference(expr) => &expr.attrs,
            Self::Rest(expr) => &expr.attrs,
            Self::Slice(expr) => &expr.attrs,
            Self::Struct(expr) => &expr.attrs,
            Self::Tuple(expr) => &expr.attrs,
            Self::TupleStruct(expr) => &expr.attrs,
            Self::Type(expr) => &expr.attrs,
            Self::Wild(expr) => &expr.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Const(expr) => Ok(&mut expr.attrs),
            Self::Ident(expr) => Ok(&mut expr.attrs),
            Self::Lit(expr) => Ok(&mut expr.attrs),
            Self::Macro(expr) => Ok(&mut expr.attrs),
            Self::Or(expr) => Ok(&mut expr.attrs),
            Self::Paren(expr) => Ok(&mut expr.attrs),
            Self::Path(expr) => Ok(&mut expr.attrs),
            Self::Range(expr) => Ok(&mut expr.attrs),
            Self::Reference(expr) => Ok(&mut expr.attrs),
            Self::Rest(expr) => Ok(&mut expr.attrs),
            Self::Slice(expr) => Ok(&mut expr.attrs),
            Self::Struct(expr) => Ok(&mut expr.attrs),
            Self::Tuple(expr) => Ok(&mut expr.attrs),
            Self::TupleStruct(expr) => Ok(&mut expr.attrs),
            Self::Type(expr) => Ok(&mut expr.attrs),
            Self::Wild(expr) => Ok(&mut expr.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::Pat::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::Pat` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::PatIdent, #full);
impl_has_attributes!(syn::PatOr, #full);
impl_has_attributes!(syn::PatParen, #full);
impl_has_attributes!(syn::PatReference, #full);
impl_has_attributes!(syn::PatRest, #full);
impl_has_attributes!(syn::PatSlice, #full);
impl_has_attributes!(syn::PatStruct, #full);
impl_has_attributes!(syn::PatTuple, #full);
impl_has_attributes!(syn::PatTupleStruct, #full);
impl_has_attributes!(syn::PatType, #full);
impl_has_attributes!(syn::PatWild, #full);
impl_has_attributes!(syn::Receiver, #full);

#[cfg(feature = "full")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
impl HasAttributes for syn::TraitItem {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            Self::Const(item) => &item.attrs,
            Self::Fn(item) => &item.attrs,
            Self::Type(item) => &item.attrs,
            Self::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            Self::Const(item) => Ok(&mut item.attrs),
            Self::Fn(item) => Ok(&mut item.attrs),
            Self::Type(item) => Ok(&mut item.attrs),
            Self::Macro(item) => Ok(&mut item.attrs),
            Self::Verbatim(_) => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` not supported with `syn::TraitItem::Verbatim`",
            )),
            _ => Err(syn::Error::new_spanned(
                self,
                "`HasAttributes::attrs_mut` encountered unknown `syn::TraitItem` variant",
            )),
        }
    }
}

impl_has_attributes!(syn::TraitItemConst, #full);
impl_has_attributes!(syn::TraitItemFn, #full);
impl_has_attributes!(syn::TraitItemMacro, #full);
impl_has_attributes!(syn::TraitItemType, #full);
impl_has_attributes!(syn::TypeParam);
impl_has_attributes!(syn::Variadic, #full);
impl_has_attributes!(syn::Variant);

/// Converts a [`HasAttributes`] type to a stored container value.
///
/// This trait is only called by the `#[deluxe(container)`] attribute on fields to properly handle
/// conversions to corresponding owned and [`Option`] types.
///
/// Custom implementations can be provided if a wrapper for the `syn` type is needed, or if needing
/// to transform the `syn` type into something else. When using a custom container type, make sure
/// to set `#[deluxe(container(ty = ...))]` on the container field. The
/// [`container_from`](Self::container_from) method will be called when deriving
/// [`ParseAttributes`], and [`container_from_mut`](Self::container_from_mut) will be called when
/// deriving [`ExtractAttributes`].
///
/// ```ignore
/// struct IdentWrapper<'t>(&'t syn::Ident);
///
/// impl<'t> deluxe::ContainerFrom<'t, syn::ItemFn> for IdentWrapper<'t> {
///     #[inline]
///     fn container_from(t: &'t syn::ItemFn) -> Self {
///         Self(&t.sig.ident)
///     }
/// }
///
/// impl<'t> deluxe::ContainerFrom<'t, syn::ItemStruct> for IdentWrapper<'t> {
///     #[inline]
///     fn container_from(t: &'t syn::ItemFn) -> Self {
///         Self(&t.ident)
///     }
/// }
///
/// // can only be parsed from an `ItemFn`
/// #[derive(deluxe::ParseAttributes)]
/// struct Func<'t> {
///     id: u64,
///     #[deluxe(container(ty = syn::ItemFn))]
///     container: IdentWrapper<'t>,
/// }
///
/// // can only be parsed from an `ItemStruct`
/// #[derive(deluxe::ParseAttributes)]
/// struct Struct<'t> {
///     id: u64,
///     #[deluxe(container(ty = syn::ItemStruct))]
///     container: IdentWrapper<'t>,
/// }
/// ```
pub trait ContainerFrom<'t, T> {
    /// Converts a reference to a stored container `T` into this type.
    #[inline]
    #[allow(unused)]
    fn container_from(t: &'t T) -> Self
    where
        Self: Sized,
    {
        unimplemented!("immutable container not supported for this type")
    }
    /// Converts a mutable reference to a stored container `T` into this type.
    #[inline]
    #[allow(unused)]
    fn container_from_mut(t: &'t mut T) -> Self
    where
        Self: Sized,
    {
        unimplemented!("mutable container not supported for this type")
    }
}

impl<'t, T: HasAttributes + Clone> ContainerFrom<'t, T> for T {
    #[inline]
    fn container_from(t: &'t T) -> Self {
        t.clone()
    }
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        t.clone()
    }
}

impl<'t, T: HasAttributes + Clone> ContainerFrom<'t, T> for Option<T> {
    #[inline]
    fn container_from(t: &'t T) -> Self {
        Some(t.clone())
    }
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        Some(t.clone())
    }
}

impl<'t, T: HasAttributes> ContainerFrom<'t, T> for &'t T {
    #[inline]
    fn container_from(t: &'t T) -> Self {
        t
    }
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        t
    }
}

impl<'t, T: HasAttributes> ContainerFrom<'t, T> for Option<&'t T> {
    #[inline]
    fn container_from(t: &'t T) -> Self {
        Some(t)
    }
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        Some(t)
    }
}

impl<'t, T: HasAttributes> ContainerFrom<'t, T> for &'t mut T {
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        t
    }
}

impl<'t, T: HasAttributes> ContainerFrom<'t, T> for Option<&'t mut T> {
    #[inline]
    fn container_from_mut(t: &'t mut T) -> Self {
        Some(t)
    }
}

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
            syn::Expr::Array(expr) => &expr.attrs,
            syn::Expr::Assign(expr) => &expr.attrs,
            syn::Expr::Async(expr) => &expr.attrs,
            syn::Expr::Await(expr) => &expr.attrs,
            syn::Expr::Binary(expr) => &expr.attrs,
            syn::Expr::Block(expr) => &expr.attrs,
            syn::Expr::Break(expr) => &expr.attrs,
            syn::Expr::Call(expr) => &expr.attrs,
            syn::Expr::Cast(expr) => &expr.attrs,
            syn::Expr::Closure(expr) => &expr.attrs,
            syn::Expr::Const(expr) => &expr.attrs,
            syn::Expr::Continue(expr) => &expr.attrs,
            syn::Expr::Field(expr) => &expr.attrs,
            syn::Expr::ForLoop(expr) => &expr.attrs,
            syn::Expr::Group(expr) => &expr.attrs,
            syn::Expr::If(expr) => &expr.attrs,
            syn::Expr::Index(expr) => &expr.attrs,
            syn::Expr::Infer(expr) => &expr.attrs,
            syn::Expr::Let(expr) => &expr.attrs,
            syn::Expr::Lit(expr) => &expr.attrs,
            syn::Expr::Loop(expr) => &expr.attrs,
            syn::Expr::Macro(expr) => &expr.attrs,
            syn::Expr::Match(expr) => &expr.attrs,
            syn::Expr::MethodCall(expr) => &expr.attrs,
            syn::Expr::Paren(expr) => &expr.attrs,
            syn::Expr::Path(expr) => &expr.attrs,
            syn::Expr::Range(expr) => &expr.attrs,
            syn::Expr::Reference(expr) => &expr.attrs,
            syn::Expr::Repeat(expr) => &expr.attrs,
            syn::Expr::Return(expr) => &expr.attrs,
            syn::Expr::Struct(expr) => &expr.attrs,
            syn::Expr::Try(expr) => &expr.attrs,
            syn::Expr::TryBlock(expr) => &expr.attrs,
            syn::Expr::Tuple(expr) => &expr.attrs,
            syn::Expr::Unary(expr) => &expr.attrs,
            syn::Expr::Unsafe(expr) => &expr.attrs,
            syn::Expr::While(expr) => &expr.attrs,
            syn::Expr::Yield(expr) => &expr.attrs,
            _ => &[],
        }
    }

    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::Expr::Array(expr) => Ok(&mut expr.attrs),
            syn::Expr::Assign(expr) => Ok(&mut expr.attrs),
            syn::Expr::Async(expr) => Ok(&mut expr.attrs),
            syn::Expr::Await(expr) => Ok(&mut expr.attrs),
            syn::Expr::Binary(expr) => Ok(&mut expr.attrs),
            syn::Expr::Block(expr) => Ok(&mut expr.attrs),
            syn::Expr::Break(expr) => Ok(&mut expr.attrs),
            syn::Expr::Call(expr) => Ok(&mut expr.attrs),
            syn::Expr::Cast(expr) => Ok(&mut expr.attrs),
            syn::Expr::Closure(expr) => Ok(&mut expr.attrs),
            syn::Expr::Const(expr) => Ok(&mut expr.attrs),
            syn::Expr::Continue(expr) => Ok(&mut expr.attrs),
            syn::Expr::Field(expr) => Ok(&mut expr.attrs),
            syn::Expr::ForLoop(expr) => Ok(&mut expr.attrs),
            syn::Expr::Group(expr) => Ok(&mut expr.attrs),
            syn::Expr::If(expr) => Ok(&mut expr.attrs),
            syn::Expr::Index(expr) => Ok(&mut expr.attrs),
            syn::Expr::Let(expr) => Ok(&mut expr.attrs),
            syn::Expr::Lit(expr) => Ok(&mut expr.attrs),
            syn::Expr::Loop(expr) => Ok(&mut expr.attrs),
            syn::Expr::Macro(expr) => Ok(&mut expr.attrs),
            syn::Expr::Match(expr) => Ok(&mut expr.attrs),
            syn::Expr::MethodCall(expr) => Ok(&mut expr.attrs),
            syn::Expr::Paren(expr) => Ok(&mut expr.attrs),
            syn::Expr::Path(expr) => Ok(&mut expr.attrs),
            syn::Expr::Range(expr) => Ok(&mut expr.attrs),
            syn::Expr::Reference(expr) => Ok(&mut expr.attrs),
            syn::Expr::Repeat(expr) => Ok(&mut expr.attrs),
            syn::Expr::Return(expr) => Ok(&mut expr.attrs),
            syn::Expr::Struct(expr) => Ok(&mut expr.attrs),
            syn::Expr::Try(expr) => Ok(&mut expr.attrs),
            syn::Expr::TryBlock(expr) => Ok(&mut expr.attrs),
            syn::Expr::Tuple(expr) => Ok(&mut expr.attrs),
            syn::Expr::Unary(expr) => Ok(&mut expr.attrs),
            syn::Expr::Unsafe(expr) => Ok(&mut expr.attrs),
            syn::Expr::While(expr) => Ok(&mut expr.attrs),
            syn::Expr::Yield(expr) => Ok(&mut expr.attrs),
            syn::Expr::Verbatim(_) => Err(syn::Error::new_spanned(
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
            syn::ForeignItem::Fn(item) => &item.attrs,
            syn::ForeignItem::Static(item) => &item.attrs,
            syn::ForeignItem::Type(item) => &item.attrs,
            syn::ForeignItem::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::ForeignItem::Fn(item) => Ok(&mut item.attrs),
            syn::ForeignItem::Static(item) => Ok(&mut item.attrs),
            syn::ForeignItem::Type(item) => Ok(&mut item.attrs),
            syn::ForeignItem::Macro(item) => Ok(&mut item.attrs),
            syn::ForeignItem::Verbatim(_) => Err(syn::Error::new_spanned(
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
            syn::ImplItem::Const(item) => &item.attrs,
            syn::ImplItem::Fn(item) => &item.attrs,
            syn::ImplItem::Type(item) => &item.attrs,
            syn::ImplItem::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::ImplItem::Const(item) => Ok(&mut item.attrs),
            syn::ImplItem::Fn(item) => Ok(&mut item.attrs),
            syn::ImplItem::Type(item) => Ok(&mut item.attrs),
            syn::ImplItem::Macro(item) => Ok(&mut item.attrs),
            syn::ImplItem::Verbatim(_) => Err(syn::Error::new_spanned(
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
            syn::Item::Const(expr) => &expr.attrs,
            syn::Item::Enum(expr) => &expr.attrs,
            syn::Item::ExternCrate(expr) => &expr.attrs,
            syn::Item::Fn(expr) => &expr.attrs,
            syn::Item::ForeignMod(expr) => &expr.attrs,
            syn::Item::Impl(expr) => &expr.attrs,
            syn::Item::Macro(expr) => &expr.attrs,
            syn::Item::Mod(expr) => &expr.attrs,
            syn::Item::Static(expr) => &expr.attrs,
            syn::Item::Struct(expr) => &expr.attrs,
            syn::Item::Trait(expr) => &expr.attrs,
            syn::Item::TraitAlias(expr) => &expr.attrs,
            syn::Item::Type(expr) => &expr.attrs,
            syn::Item::Union(expr) => &expr.attrs,
            syn::Item::Use(expr) => &expr.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::Item::Const(expr) => Ok(&mut expr.attrs),
            syn::Item::Enum(expr) => Ok(&mut expr.attrs),
            syn::Item::ExternCrate(expr) => Ok(&mut expr.attrs),
            syn::Item::Fn(expr) => Ok(&mut expr.attrs),
            syn::Item::ForeignMod(expr) => Ok(&mut expr.attrs),
            syn::Item::Impl(expr) => Ok(&mut expr.attrs),
            syn::Item::Macro(expr) => Ok(&mut expr.attrs),
            syn::Item::Mod(expr) => Ok(&mut expr.attrs),
            syn::Item::Static(expr) => Ok(&mut expr.attrs),
            syn::Item::Struct(expr) => Ok(&mut expr.attrs),
            syn::Item::Trait(expr) => Ok(&mut expr.attrs),
            syn::Item::TraitAlias(expr) => Ok(&mut expr.attrs),
            syn::Item::Type(expr) => Ok(&mut expr.attrs),
            syn::Item::Union(expr) => Ok(&mut expr.attrs),
            syn::Item::Use(expr) => Ok(&mut expr.attrs),
            syn::Item::Verbatim(_) => Err(syn::Error::new_spanned(
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
            syn::Pat::Const(expr) => &expr.attrs,
            syn::Pat::Ident(expr) => &expr.attrs,
            syn::Pat::Lit(expr) => &expr.attrs,
            syn::Pat::Macro(expr) => &expr.attrs,
            syn::Pat::Or(expr) => &expr.attrs,
            syn::Pat::Paren(expr) => &expr.attrs,
            syn::Pat::Path(expr) => &expr.attrs,
            syn::Pat::Range(expr) => &expr.attrs,
            syn::Pat::Reference(expr) => &expr.attrs,
            syn::Pat::Rest(expr) => &expr.attrs,
            syn::Pat::Slice(expr) => &expr.attrs,
            syn::Pat::Struct(expr) => &expr.attrs,
            syn::Pat::Tuple(expr) => &expr.attrs,
            syn::Pat::TupleStruct(expr) => &expr.attrs,
            syn::Pat::Type(expr) => &expr.attrs,
            syn::Pat::Wild(expr) => &expr.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::Pat::Const(expr) => Ok(&mut expr.attrs),
            syn::Pat::Ident(expr) => Ok(&mut expr.attrs),
            syn::Pat::Lit(expr) => Ok(&mut expr.attrs),
            syn::Pat::Macro(expr) => Ok(&mut expr.attrs),
            syn::Pat::Or(expr) => Ok(&mut expr.attrs),
            syn::Pat::Paren(expr) => Ok(&mut expr.attrs),
            syn::Pat::Path(expr) => Ok(&mut expr.attrs),
            syn::Pat::Range(expr) => Ok(&mut expr.attrs),
            syn::Pat::Reference(expr) => Ok(&mut expr.attrs),
            syn::Pat::Rest(expr) => Ok(&mut expr.attrs),
            syn::Pat::Slice(expr) => Ok(&mut expr.attrs),
            syn::Pat::Struct(expr) => Ok(&mut expr.attrs),
            syn::Pat::Tuple(expr) => Ok(&mut expr.attrs),
            syn::Pat::TupleStruct(expr) => Ok(&mut expr.attrs),
            syn::Pat::Type(expr) => Ok(&mut expr.attrs),
            syn::Pat::Wild(expr) => Ok(&mut expr.attrs),
            syn::Pat::Verbatim(_) => Err(syn::Error::new_spanned(
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
            syn::TraitItem::Const(item) => &item.attrs,
            syn::TraitItem::Fn(item) => &item.attrs,
            syn::TraitItem::Type(item) => &item.attrs,
            syn::TraitItem::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::TraitItem::Const(item) => Ok(&mut item.attrs),
            syn::TraitItem::Fn(item) => Ok(&mut item.attrs),
            syn::TraitItem::Type(item) => Ok(&mut item.attrs),
            syn::TraitItem::Macro(item) => Ok(&mut item.attrs),
            syn::TraitItem::Verbatim(_) => Err(syn::Error::new_spanned(
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

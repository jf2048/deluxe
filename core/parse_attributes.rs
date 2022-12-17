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
    ($ty:ty) => {
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
}

impl_has_attributes!(syn::Arm);
impl_has_attributes!(syn::BareFnArg);
impl_has_attributes!(syn::ConstParam);
impl_has_attributes!(syn::DeriveInput);

impl HasAttributes for syn::Expr {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            syn::Expr::Array(expr) => &expr.attrs,
            syn::Expr::Assign(expr) => &expr.attrs,
            syn::Expr::AssignOp(expr) => &expr.attrs,
            syn::Expr::Async(expr) => &expr.attrs,
            syn::Expr::Await(expr) => &expr.attrs,
            syn::Expr::Binary(expr) => &expr.attrs,
            syn::Expr::Block(expr) => &expr.attrs,
            syn::Expr::Box(expr) => &expr.attrs,
            syn::Expr::Break(expr) => &expr.attrs,
            syn::Expr::Call(expr) => &expr.attrs,
            syn::Expr::Cast(expr) => &expr.attrs,
            syn::Expr::Closure(expr) => &expr.attrs,
            syn::Expr::Continue(expr) => &expr.attrs,
            syn::Expr::Field(expr) => &expr.attrs,
            syn::Expr::ForLoop(expr) => &expr.attrs,
            syn::Expr::Group(expr) => &expr.attrs,
            syn::Expr::If(expr) => &expr.attrs,
            syn::Expr::Index(expr) => &expr.attrs,
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
            syn::Expr::Type(expr) => &expr.attrs,
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
            syn::Expr::AssignOp(expr) => Ok(&mut expr.attrs),
            syn::Expr::Async(expr) => Ok(&mut expr.attrs),
            syn::Expr::Await(expr) => Ok(&mut expr.attrs),
            syn::Expr::Binary(expr) => Ok(&mut expr.attrs),
            syn::Expr::Block(expr) => Ok(&mut expr.attrs),
            syn::Expr::Box(expr) => Ok(&mut expr.attrs),
            syn::Expr::Break(expr) => Ok(&mut expr.attrs),
            syn::Expr::Call(expr) => Ok(&mut expr.attrs),
            syn::Expr::Cast(expr) => Ok(&mut expr.attrs),
            syn::Expr::Closure(expr) => Ok(&mut expr.attrs),
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
            syn::Expr::Type(expr) => Ok(&mut expr.attrs),
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

impl_has_attributes!(syn::ExprArray);
impl_has_attributes!(syn::ExprAssign);
impl_has_attributes!(syn::ExprAssignOp);
impl_has_attributes!(syn::ExprAsync);
impl_has_attributes!(syn::ExprAwait);
impl_has_attributes!(syn::ExprBinary);
impl_has_attributes!(syn::ExprBlock);
impl_has_attributes!(syn::ExprBox);
impl_has_attributes!(syn::ExprBreak);
impl_has_attributes!(syn::ExprCall);
impl_has_attributes!(syn::ExprCast);
impl_has_attributes!(syn::ExprClosure);
impl_has_attributes!(syn::ExprContinue);
impl_has_attributes!(syn::ExprField);
impl_has_attributes!(syn::ExprForLoop);
impl_has_attributes!(syn::ExprGroup);
impl_has_attributes!(syn::ExprIf);
impl_has_attributes!(syn::ExprIndex);
impl_has_attributes!(syn::ExprLet);
impl_has_attributes!(syn::ExprLit);
impl_has_attributes!(syn::ExprLoop);
impl_has_attributes!(syn::ExprMacro);
impl_has_attributes!(syn::ExprMatch);
impl_has_attributes!(syn::ExprMethodCall);
impl_has_attributes!(syn::ExprParen);
impl_has_attributes!(syn::ExprPath);
impl_has_attributes!(syn::ExprRange);
impl_has_attributes!(syn::ExprReference);
impl_has_attributes!(syn::ExprRepeat);
impl_has_attributes!(syn::ExprReturn);
impl_has_attributes!(syn::ExprStruct);
impl_has_attributes!(syn::ExprTry);
impl_has_attributes!(syn::ExprTryBlock);
impl_has_attributes!(syn::ExprTuple);
impl_has_attributes!(syn::ExprType);
impl_has_attributes!(syn::ExprUnary);
impl_has_attributes!(syn::ExprUnsafe);
impl_has_attributes!(syn::ExprWhile);
impl_has_attributes!(syn::ExprYield);
impl_has_attributes!(syn::Field);
impl_has_attributes!(syn::FieldPat);
impl_has_attributes!(syn::FieldValue);
impl_has_attributes!(syn::File);

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

impl_has_attributes!(syn::ForeignItemFn);
impl_has_attributes!(syn::ForeignItemMacro);
impl_has_attributes!(syn::ForeignItemStatic);
impl_has_attributes!(syn::ForeignItemType);

impl HasAttributes for syn::ImplItem {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            syn::ImplItem::Const(item) => &item.attrs,
            syn::ImplItem::Method(item) => &item.attrs,
            syn::ImplItem::Type(item) => &item.attrs,
            syn::ImplItem::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::ImplItem::Const(item) => Ok(&mut item.attrs),
            syn::ImplItem::Method(item) => Ok(&mut item.attrs),
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

impl_has_attributes!(syn::ImplItemConst);
impl_has_attributes!(syn::ImplItemMacro);
impl_has_attributes!(syn::ImplItemMethod);
impl_has_attributes!(syn::ImplItemType);

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
            syn::Item::Macro2(expr) => &expr.attrs,
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
            syn::Item::Macro2(expr) => Ok(&mut expr.attrs),
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

impl_has_attributes!(syn::ItemConst);
impl_has_attributes!(syn::ItemEnum);
impl_has_attributes!(syn::ItemExternCrate);
impl_has_attributes!(syn::ItemFn);
impl_has_attributes!(syn::ItemForeignMod);
impl_has_attributes!(syn::ItemImpl);
impl_has_attributes!(syn::ItemMacro);
impl_has_attributes!(syn::ItemMacro2);
impl_has_attributes!(syn::ItemMod);
impl_has_attributes!(syn::ItemStatic);
impl_has_attributes!(syn::ItemStruct);
impl_has_attributes!(syn::ItemTrait);
impl_has_attributes!(syn::ItemTraitAlias);
impl_has_attributes!(syn::ItemType);
impl_has_attributes!(syn::ItemUnion);
impl_has_attributes!(syn::ItemUse);
impl_has_attributes!(syn::LifetimeDef);
impl_has_attributes!(syn::Local);

impl HasAttributes for syn::Pat {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            syn::Pat::Box(expr) => &expr.attrs,
            syn::Pat::Ident(expr) => &expr.attrs,
            syn::Pat::Lit(expr) => &expr.attrs,
            syn::Pat::Macro(expr) => &expr.attrs,
            syn::Pat::Or(expr) => &expr.attrs,
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
            syn::Pat::Box(expr) => Ok(&mut expr.attrs),
            syn::Pat::Ident(expr) => Ok(&mut expr.attrs),
            syn::Pat::Lit(expr) => Ok(&mut expr.attrs),
            syn::Pat::Macro(expr) => Ok(&mut expr.attrs),
            syn::Pat::Or(expr) => Ok(&mut expr.attrs),
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

impl_has_attributes!(syn::PatBox);
impl_has_attributes!(syn::PatIdent);
impl_has_attributes!(syn::PatLit);
impl_has_attributes!(syn::PatMacro);
impl_has_attributes!(syn::PatOr);
impl_has_attributes!(syn::PatPath);
impl_has_attributes!(syn::PatRange);
impl_has_attributes!(syn::PatReference);
impl_has_attributes!(syn::PatRest);
impl_has_attributes!(syn::PatSlice);
impl_has_attributes!(syn::PatStruct);
impl_has_attributes!(syn::PatTuple);
impl_has_attributes!(syn::PatTupleStruct);
impl_has_attributes!(syn::PatType);
impl_has_attributes!(syn::PatWild);
impl_has_attributes!(syn::Receiver);

impl HasAttributes for syn::TraitItem {
    fn attrs(&self) -> &[syn::Attribute] {
        match self {
            syn::TraitItem::Const(item) => &item.attrs,
            syn::TraitItem::Method(item) => &item.attrs,
            syn::TraitItem::Type(item) => &item.attrs,
            syn::TraitItem::Macro(item) => &item.attrs,
            _ => &[],
        }
    }
    fn attrs_mut(&mut self) -> Result<&mut Vec<syn::Attribute>> {
        match self {
            syn::TraitItem::Const(item) => Ok(&mut item.attrs),
            syn::TraitItem::Method(item) => Ok(&mut item.attrs),
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

impl_has_attributes!(syn::TraitItemConst);
impl_has_attributes!(syn::TraitItemMacro);
impl_has_attributes!(syn::TraitItemMethod);
impl_has_attributes!(syn::TraitItemType);
impl_has_attributes!(syn::TypeParam);
impl_has_attributes!(syn::Variadic);
impl_has_attributes!(syn::Variant);

/// Converts a [`HasAttributes`] type to a stored container value.
///
/// This trait is only called by the `#[deluxe(container)`] attribute on fields to properly handle
/// conversions to corresponding owned and [`Option`] types.
pub trait ContainerFrom<'t, T> {
    /// Converts a reference to a stored container `T`.
    #[inline]
    #[allow(unused)]
    fn container_from(t: &'t T) -> Self where Self: Sized {
        unimplemented!()
    }
    /// Converts a mutable reference to a stored container `T`.
    #[inline]
    #[allow(unused)]
    fn container_from_mut(t: &'t mut T) -> Self where Self: Sized {
        unimplemented!()
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

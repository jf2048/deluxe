use super::*;
use deluxe_core::{parse_helpers, ParseAttributes, Result};
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use std::collections::{BTreeSet, HashSet};
use syn::spanned::Spanned;

pub struct Enum<'e> {
    pub enum_: &'e syn::DataEnum,
    pub variants: Vec<Variant<'e>>,
    pub default: Option<FieldDefault>,
    pub crate_: Option<syn::Path>,
    pub attributes: Vec<syn::Path>,
    pub allow_unknown_fields: Option<bool>,
}

impl<'e> Enum<'e> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["default", "crate", "attributes"]
    }
    #[inline]
    pub fn to_inline_parsing_tokens(&self, crate_: &syn::Path, mode: TokenMode) -> TokenStream {
        let default = self.default.as_ref().map(|d| {
            let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
            d.to_expr(&parse_quote_mixed! { Self }, &priv_path)
        });
        Variant::to_parsing_tokens(
            &self.variants,
            crate_,
            mode,
            default.as_ref().map(|d| d.as_ref()),
            self.allow_unknown_fields.unwrap_or(false),
        )
    }
    pub fn to_accepts_all_tokens(&self, crate_: &syn::Path) -> Option<TokenStream> {
        if self.allow_unknown_fields.unwrap_or(false)
            || self
                .variants
                .iter()
                .any(|v| v.flatten.unwrap_or(false) && v.allow_unknown_fields.unwrap_or(false))
        {
            return Some(quote_mixed! { true });
        }
        let mut accepts_all = self
            .variants
            .iter()
            .filter_map(|v| {
                v.flatten
                    .unwrap_or(false)
                    .then(|| Field::to_accepts_all_tokens(&v.fields, crate_))
                    .flatten()
            })
            .peekable();
        accepts_all.peek().is_some().then(|| {
            quote_mixed! { #(#accepts_all)||* }
        })
    }
    pub fn to_field_names_tokens(&self, crate_: &syn::Path, priv_: &syn::Path) -> TokenStream {
        let any_flat_nested = self
            .variants
            .iter()
            .any(|v| v.flatten.unwrap_or(false) && v.fields.iter().any(|f| f.is_flat()));
        let field_names = self.variants.iter().flat_map(|v| {
            v.idents
                .iter()
                .filter_map(|ident| {
                    if v.flatten.unwrap_or(false) {
                        return None;
                    }
                    let ident = ident.to_string();
                    Some(if any_flat_nested {
                        quote_mixed! { vec.push(#ident); }
                    } else {
                        quote_mixed! { #ident }
                    })
                })
                .chain(v.fields.iter().filter_map(|field| {
                    if !v.flatten.unwrap_or(false) {
                        return None;
                    }
                    match &field.flatten {
                        Some(FieldFlatten {
                            value: true,
                            prefix: Some(prefix),
                            ..
                        }) => {
                            let ty = &field.field.ty;
                            let prefix = parse_helpers::path_to_string(prefix);
                            let names = quote_spanned! { ty.span() =>
                                <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                            };
                            Some(quote_mixed! {
                                vec.extend({
                                    static CELL: #priv_::SyncOnceCell<#priv_::Vec<#priv_::String>> = #priv_::SyncOnceCell::new();
                                    CELL.get_or_init(|| #priv_::parse_helpers::join_paths(#prefix, #names))
                                        .iter()
                                        .map(|ps| ps.as_str())
                                });
                            })
                        }
                        Some(FieldFlatten {
                            value: true,
                            prefix: None,
                            ..
                        }) => {
                            let ty = &field.field.ty;
                            let names = quote_spanned! { ty.span() =>
                                <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                            };
                            Some(quote_mixed! {
                                vec.extend_from_slice(#names);
                            })
                        },
                        _ => None,
                    }
                }))
                .chain(v.fields.iter().flat_map(|field| {
                    field.idents.iter().filter_map(|ident| {
                        if !v.flatten.unwrap_or(false) {
                            return None;
                        }
                        match &field.flatten {
                            Some(FieldFlatten {
                                value: true,
                                ..
                            }) => None,
                            _ => {
                                let ident = ident.to_string();
                                if any_flat_nested {
                                    Some(quote_mixed! { vec.push(#ident); })
                                } else {
                                    Some(quote_mixed! { #ident })
                                }
                            }
                        }
                    })
                }))
        });
        if any_flat_nested {
            quote_mixed! {
                {
                    static CELL: #priv_::SyncOnceCell<#priv_::Vec<&'static #priv_::str>> = #priv_::SyncOnceCell::new();
                    CELL.get_or_init(|| {
                        let mut vec = #priv_::Vec::new();
                        #(#field_names)*
                        vec
                    }).as_slice()
                }
            }
        } else {
            quote_mixed! {
                &[#(#field_names),*]
            }
        }
    }
}

impl<'e> ParseAttributes<'e, syn::DeriveInput> for Enum<'e> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(i: &'e syn::DeriveInput) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(
            parse_helpers::ref_tokens::<Self, _>(i),
            |inputs, _| {
                let enum_ = match &i.data {
                    syn::Data::Enum(e) => e,
                    _ => return Err(syn::Error::new_spanned(i, "wrong DeriveInput type")),
                };
                let errors = crate::Errors::new();
                let mut default = None;
                let mut crate_ = None;
                let mut attributes = Vec::new();
                let mut allow_unknown_fields = None;
                parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "default" => {
                            if default.is_some() {
                                errors.push(span, "duplicate attribute for `default`");
                            }
                            default = Some(parse_helpers::parse_named_meta_item::<FieldDefault>(
                                input, span,
                            )?);
                        }
                        "crate" => {
                            if crate_.is_some() {
                                errors.push(span, "duplicate attribute for `crate`");
                            }
                            crate_ = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "attributes" => {
                            let attrs = deluxe_core::parse_named_meta_item_with!(
                                input,
                                span,
                                deluxe_core::with::mod_path_vec
                            )?;
                            attributes.extend(attrs.into_iter());
                        }
                        "allow_unknown_fields" => {
                            if allow_unknown_fields.is_some() {
                                errors.push(span, "duplicate attribute for `allow_unknown_fields`");
                            }
                            allow_unknown_fields =
                                Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        _ => {
                            parse_helpers::check_unknown_attribute(
                                path,
                                span,
                                Self::field_names(),
                                &errors,
                            );
                            parse_helpers::skip_named_meta_item(input);
                        }
                    }
                    Ok(())
                })?;
                let variants = enum_
                    .variants
                    .iter()
                    .filter_map(|v| {
                        match <Variant as ParseAttributes<syn::Variant>>::parse_attributes(v) {
                            Ok(v) => Some(v),
                            Err(err) => {
                                errors.push_syn(err);
                                None
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                let mut all_idents = HashSet::new();
                let mut container = None;
                let mut variant_keys = BTreeSet::<BTreeSet<BTreeSet<String>>>::new();
                for variant in &variants {
                    if let Some(c) = variant
                        .fields
                        .iter()
                        .find_map(|f| f.container.as_ref().and_then(|c| c.value.then_some(c)))
                    {
                        if container.is_some() {
                            if let Some(lifetime) = c.lifetime.as_ref() {
                                errors.push_spanned(
                                lifetime,
                                "only the first `container` field can contain a `lifetime` parameter"
                            );
                            }
                            if let Some(ty) = c.ty.as_ref() {
                                errors.push_spanned(
                                ty,
                                "only the first `container` field can contain a `type` parameter",
                            );
                            }
                        } else {
                            container = Some(c);
                        }
                    }
                    if !variant.flatten.unwrap_or(false) {
                        variant_keys.insert(
                            [variant.idents.iter().map(|i| i.to_string()).collect()].into(),
                        );
                        for ident in &variant.idents {
                            if all_idents.contains(&ident) {
                                errors.push_spanned(
                                    ident,
                                    format_args!("duplicate variant name for `{}`", ident),
                                );
                            } else {
                                all_idents.insert(ident);
                            }
                        }
                    }
                }
                for variant in &variants {
                    if variant.flatten.unwrap_or(false) {
                        if matches!(variant.variant.fields, syn::Fields::Named(_)) {
                            let key = variant.field_key();
                            if variant_keys.contains(&key) {
                                errors.push_spanned(
                                variant.variant,
                                "additional flattened variants must have at least one unique non-flattened, non-default field",
                            );
                            } else {
                                variant_keys.insert(key);
                            }
                        } else {
                            errors.push_spanned(
                                variant.variant,
                                "only enum variants with named fields can have `flatten`",
                            );
                        }
                    }
                }
                if let Some(default) = &default {
                    if variant_keys.contains(&BTreeSet::new()) {
                        errors.push(
                            default.span(),
                            "`default` cannot be used when a flattened variant has no unique field",
                        );
                    }
                }
                errors.check()?;
                Ok(Self {
                    enum_,
                    variants,
                    default,
                    crate_,
                    attributes,
                    allow_unknown_fields,
                })
            },
        )
    }
}

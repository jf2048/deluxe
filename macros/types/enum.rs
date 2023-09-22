use super::*;
use deluxe_core::{
    parse_helpers::{self, FieldStatus},
    ParseAttributes, ParseMetaItem, Result,
};
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
    pub and_thens: Vec<TokenStream>,
    pub allow_unknown_fields: Option<bool>,
}

impl<'e> Enum<'e> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["default", "crate", "attributes", "and_then"]
    }
    #[inline]
    pub fn to_inline_parsing_tokens(&self, crate_: &syn::Path, mode: TokenMode) -> TokenStream {
        let default = self.default.as_ref().map(|d| {
            let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
            d.to_expr(
                Some(&syn::parse_quote_spanned! { d.span() => Self }),
                &priv_path,
            )
        });
        Variant::to_parsing_tokens(
            &self.variants,
            crate_,
            mode,
            default.as_ref().map(|d| d.as_ref()),
            &self.and_thens,
            self.allow_unknown_fields.unwrap_or(false),
        )
    }
    pub fn to_accepts_all_tokens(&self, crate_: &syn::Path) -> Option<TokenStream> {
        if self.allow_unknown_fields.unwrap_or(false)
            || self.variants.iter().any(|v| {
                !v.is_skipped()
                    && v.flatten.unwrap_or(false)
                    && v.allow_unknown_fields.unwrap_or(false)
            })
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
                    if v.flatten.unwrap_or(false) || v.is_skipped() {
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
                            let prefix = parse_helpers::key_to_string(prefix);
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

deluxe_core::define_with_collection!(mod mod_path_vec, deluxe_core::with::mod_path, Vec<syn::Path>);

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
                let mut default = FieldStatus::<FieldDefault>::None;
                let mut crate_ = FieldStatus::None;
                let mut and_thens = Vec::new();
                let mut attributes = Vec::new();
                let mut allow_unknown_fields = FieldStatus::None;
                errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "default" => default.parse_named_item("default", input, span, &errors),
                        "crate" => crate_.parse_named_item("crate", input, span, &errors),
                        "and_then" => {
                            match errors.push_result(<_>::parse_meta_item_named(input, path, span))
                            {
                                Some(e) => and_thens.push(e),
                                None => parse_helpers::skip_meta_item(input),
                            }
                        }
                        "attributes" => {
                            match errors
                                .push_result(mod_path_vec::parse_meta_item_named(input, path, span))
                            {
                                Some(attrs) => attributes.extend(attrs),
                                None => parse_helpers::skip_meta_item(input),
                            }
                        }
                        "allow_unknown_fields" => allow_unknown_fields.parse_named_item(
                            "allow_unknown_fields",
                            input,
                            span,
                            &errors,
                        ),
                        _ => {
                            parse_helpers::check_unknown_attribute(
                                path,
                                span,
                                Self::field_names(),
                                &errors,
                            );
                            parse_helpers::skip_meta_item(input);
                        }
                    }
                    Ok(())
                }));
                let variants = enum_
                    .variants
                    .iter()
                    .filter_map(|v| {
                        errors.push_result(
                            <Variant as ParseAttributes<syn::Variant>>::parse_attributes(v),
                        )
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
                                    format_args!("duplicate variant name for `{ident}`"),
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
                if let FieldStatus::Some(default) = &default {
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
                    default: default.into(),
                    crate_: crate_.into(),
                    attributes,
                    and_thens,
                    allow_unknown_fields: allow_unknown_fields.into(),
                })
            },
        )
    }
}

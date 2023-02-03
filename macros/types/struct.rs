use super::*;
use deluxe_core::{parse_helpers, ParseAttributes, ParseMetaItem, ParseMode, Result};
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use std::{borrow::Borrow, collections::HashSet};
use syn::{
    parse::{ParseBuffer, ParseStream},
    spanned::Spanned,
};

pub struct StructTransparent {
    pub span: Span,
    pub value: bool,
    pub flatten_named: Option<bool>,
    pub flatten_unnamed: Option<bool>,
    pub append: Option<bool>,
    pub rest: Option<bool>,
}

impl Spanned for StructTransparent {
    fn span(&self) -> Span {
        self.span
    }
}

impl ParseMetaItem for StructTransparent {
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            let value = input.parse::<syn::LitBool>()?;
            Ok(Self {
                span: value.span(),
                value: value.value(),
                flatten_named: None,
                flatten_unnamed: None,
                append: None,
                rest: None,
            })
        } else if lookahead.peek(syn::token::Brace) {
            <parse_helpers::Brace as parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                input, mode,
            )
        } else {
            Err(lookahead.error())
        }
    }
    fn parse_meta_item_inline<'s, S: Borrow<ParseBuffer<'s>>>(
        inputs: &[S],
        mode: ParseMode,
    ) -> Result<Self> {
        let mut transparent = Self {
            span: mode.to_full_span(inputs),
            value: true,
            flatten_named: None,
            flatten_unnamed: None,
            append: None,
            rest: None,
        };
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "flatten_named" => {
                    if transparent.flatten_named.is_some() {
                        errors.push(span, "duplicate attribute for `flatten_named`");
                    }
                    transparent.flatten_named =
                        Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                }
                "flatten_unnamed" => {
                    if transparent.flatten_unnamed.is_some() {
                        errors.push(span, "duplicate attribute for `flatten_unnamed`");
                    }
                    transparent.flatten_unnamed =
                        Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                }
                "append" => {
                    if transparent.append.is_some() {
                        errors.push(span, "duplicate attribute for `append`");
                    }
                    transparent.append = Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                }
                "rest" => {
                    if transparent.rest.is_some() {
                        errors.push(span, "duplicate attribute for `rest`");
                    }
                    transparent.rest = Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                }
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(
                        path,
                        span,
                        &["flatten_named", "flatten_unnamed", "append", "rest"],
                    ));
                    parse_helpers::skip_named_meta_item(input);
                }
            }
            Ok(())
        });
        if let Err(err) = res {
            errors.extend(err);
        }
        errors.check()?;
        Ok(transparent)
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self {
            span,
            value: true,
            flatten_named: None,
            flatten_unnamed: None,
            append: None,
            rest: None,
        })
    }
}

pub struct Struct<'s> {
    pub struct_: &'s syn::DataStruct,
    pub fields: Vec<Field<'s>>,
    pub default: Option<FieldDefault>,
    pub crate_: Option<syn::Path>,
    pub transparent: Option<StructTransparent>,
    pub allow_unknown_fields: Option<bool>,
    pub attributes: Vec<syn::Path>,
}

impl<'s> Struct<'s> {
    #[inline]
    pub fn is_transparent(&self) -> bool {
        self.transparent.as_ref().map(|t| t.value).unwrap_or(false)
            && self
                .fields
                .iter()
                .filter(|f| f.is_parsable() && !f.is_flat())
                .take(2)
                .count()
                == 1
    }
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &[
            "transparent",
            "default",
            "crate",
            "attributes",
            "allow_unknown_fields",
        ]
    }
    #[inline]
    pub fn to_accepts_all_tokens(&self, crate_: &syn::Path) -> Option<TokenStream> {
        if self.allow_unknown_fields.unwrap_or(false) {
            return Some(quote_mixed! { true });
        }
        Field::to_accepts_all_tokens(&self.fields, crate_)
    }
    #[inline]
    pub fn to_field_names_tokens(&self, crate_: &syn::Path, priv_: &syn::Path) -> TokenStream {
        Field::to_field_names_tokens(&self.fields, crate_, priv_)
    }
    pub fn to_parsing_tokens(
        &self,
        orig: &syn::DeriveInput,
        crate_: &syn::Path,
        mode: TokenMode,
        inline_expr: &syn::Expr,
        allowed_expr: &syn::Expr,
    ) -> ItemDef {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let target = self.default.as_ref().map(|_| parse_quote_mixed! { target });
        let target = target
            .as_ref()
            .map(ParseTarget::Var)
            .unwrap_or_else(|| ParseTarget::Init(None));
        let transparent = self.is_transparent();
        let allow_unknown_fields = self.allow_unknown_fields.unwrap_or(false);
        let field_data = FieldData {
            mode,
            target,
            inline_expr,
            allowed_expr,
            transparent,
            variant: false,
            allow_unknown_fields,
        };
        let orig_fields = match &orig.data {
            syn::Data::Struct(s) => &s.fields,
            _ => unreachable!(),
        };
        let (mut pre, post) =
            Field::to_pre_post_tokens(&self.fields, orig_fields, crate_, &field_data);
        let default_set = self.default.as_ref().map(|d| {
            let expr = d.to_expr(&syn::parse_quote! { Self }, priv_);
            quote_mixed! {
                let mut target: Self = #expr;
            }
        });
        if transparent {
            let (field, name) = self
                .fields
                .iter()
                .enumerate()
                .find_map(|(i, f)| {
                    (f.is_parsable() && !f.is_flat()).then(|| {
                        (
                            f,
                            quote::format_ident!("field{}", i, span = Span::mixed_site()),
                        )
                    })
                })
                .unwrap();
            let ty = &field.field.ty;
            let module = field
                .with
                .as_ref()
                .map(|m| quote_spanned! { m.span() => #m });
            let parse_ty = module.clone().unwrap_or_else(|| {
                quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaItem> }
            });
            pre.extend(default_set);
            if matches!(orig_fields, syn::Fields::Unnamed(_)) {
                pre.extend(quote_mixed! {
                    let index = 0usize;
                });
            }
            let parse = quote_mixed! {
                #pre
                #name = #priv_::Option::Some(#parse_ty::parse_meta_item(input, _mode)?);
                #post
            };
            let inline = Some(quote_mixed! {
                #pre
                #name = #priv_::Option::Some(#parse_ty::parse_meta_item_inline(inputs, _mode)?);
                #post
            });
            let flag = Some(quote_mixed! {
                #pre
                #name = #priv_::Option::Some(#parse_ty::parse_meta_item_flag(span)?);
                #post
            });
            let struct_ident = &orig.ident;
            let (impl_generics, type_generics, where_clause) = orig.generics.split_for_impl();
            let flatten_named = self
                .transparent
                .as_ref()
                .and_then(|t| t.flatten_named)
                .unwrap_or(false);
            let flatten_unnamed = self
                .transparent
                .as_ref()
                .and_then(|t| t.flatten_unnamed)
                .unwrap_or(false);
            let rest = self
                .transparent
                .as_ref()
                .and_then(|t| t.rest)
                .unwrap_or(false);
            let append = self
                .transparent
                .as_ref()
                .and_then(|t| t.append)
                .unwrap_or(false);

            let mut extra_traits = Vec::new();
            if flatten_named {
                let flat_ty = module.clone().unwrap_or_else(|| {
                    quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaFlatNamed> }
                });
                extra_traits.push(quote_mixed! {
                    impl #impl_generics #crate_::ParseMetaFlatNamed for #struct_ident #type_generics #where_clause {
                        const ACCEPTS_ALL: #priv_::bool = #flat_ty::ACCEPTS_ALL;
                        #[inline]
                        fn field_names() -> &'static [&'static #priv_::str] {
                            #flat_ty::field_names()
                        }
                        #[inline]
                        fn parse_meta_flat_named<'s, S: #priv_::Borrow<#priv_::ParseBuffer<'s>>>(
                            inputs: &[S],
                            mode: #crate_::ParseMode,
                            prefix: &#priv_::str,
                            validate: #priv_::bool,
                        ) -> #crate_::Result<Self> {
                            #pre
                            #name = #priv_::Option::Some(#flat_ty::parse_meta_flat_named(inputs, mode, prefix, validate)?);
                            #post
                        }
                    }
                });
            }
            if flatten_unnamed {
                let flat_ty = module.clone().unwrap_or_else(|| {
                    quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaFlatUnnamed> }
                });
                extra_traits.push(quote_mixed! {
                    impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn field_count() -> #priv_::Option<#priv_::usize> {
                            #flat_ty::field_count()
                        }
                        #[inline]
                        fn parse_meta_flat_unnamed<'s, S: #priv_::Borrow<#priv_::ParseBuffer<'s>>>(
                            inputs: &[S],
                            mode: #crate_::ParseMode,
                            index: #priv_::usize,
                        ) -> #crate_::Result<Self> {
                            #pre
                            #name = #priv_::Option::Some(#flat_ty::parse_meta_flat_unnamed(inputs, mode, index)?);
                            #post
                        }
                    }
                });
            }
            if rest {
                let rest_ty = module.clone().unwrap_or_else(|| {
                    quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaRest> }
                });
                extra_traits.push(quote_mixed! {
                    impl #impl_generics #crate_::ParseMetaRest for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn parse_meta_rest<'s, S: #priv_::Borrow<#priv_::ParseBuffer<'s>>>(
                            inputs: &[S],
                            exclude: &[&#priv_::str],
                        ) -> #crate_::Result<Self> {
                            #pre
                            #name = #priv_::Option::Some(#rest_ty::parse_meta_rest(inputs, exclude)?);
                            #post
                        }
                    }
                });
            }
            if append {
                let append_ty = module.unwrap_or_else(|| {
                    quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaAppend> }
                });
                extra_traits.push(quote_mixed! {
                    impl #impl_generics #crate_::ParseMetaAppend for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn parse_meta_append<'s, S, I, P>(inputs: &[S], paths: I) -> #crate_::Result<Self>
                        where
                            S: #priv_::Borrow<#priv_::ParseBuffer<'s>>,
                            I: #priv_::IntoIterator<Item = P>,
                            I::IntoIter: #priv_::Clone,
                            P: #priv_::AsRef<#priv_::str>
                        {
                            #pre
                            #name = #priv_::Option::Some(#append_ty::parse_meta_append(inputs, paths)?);
                            #post
                        }
                    }
                });
            }
            let extra_traits = (!extra_traits.is_empty()).then(|| {
                quote_mixed! {
                    #(#extra_traits)*
                }
            });
            ItemDef {
                parse,
                inline,
                flag,
                extra_traits,
            }
        } else {
            let mut def = Field::to_parsing_tokens(
                &self.fields,
                orig_fields,
                crate_,
                (&pre, &post),
                field_data,
            );
            def.inline = def.inline.map(|inline| {
                quote_mixed! {
                    #default_set
                    #inline
                }
            });
            def
        }
    }
}

deluxe_core::define_with_collection!(mod mod_path_vec, deluxe_core::with::mod_path, Vec<syn::Path>);

impl<'s> ParseAttributes<'s, syn::DeriveInput> for Struct<'s> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(i: &'s syn::DeriveInput) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(
            parse_helpers::ref_tokens::<Self, _>(i),
            |inputs, _| {
                let struct_ = match &i.data {
                    syn::Data::Struct(s) => s,
                    _ => return Err(syn::Error::new_spanned(i, "wrong DeriveInput type")),
                };
                let errors = crate::Errors::new();
                let mut transparent = None;
                let mut allow_unknown_fields = None;
                let mut default = None;
                let mut crate_ = None;
                let mut attributes = Vec::new();
                let fields = struct_
                    .fields
                    .iter()
                    .filter_map(|f| match Field::parse_attributes(f) {
                        Ok(f) => Some(f),
                        Err(err) => {
                            errors.push_syn(err);
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "transparent" => {
                            if transparent.is_some() {
                                errors.push(span, "duplicate attribute for `transparent`");
                            }
                            let mut iter = fields.iter().filter(|f| f.is_parsable());
                            if let Some(first) = iter.next() {
                                if iter.next().is_some() {
                                    errors.push(
                                        span,
                                        "`transparent` struct must have only one parseable field",
                                    );
                                } else if first.flatten.as_ref().map(|f| f.value).unwrap_or(false) {
                                    errors
                                        .push(span, "`transparent` struct field cannot be `flat`");
                                } else if first.append.map(|v| *v).unwrap_or(false) {
                                    errors.push(
                                        span,
                                        "`transparent` struct field cannot be `append`",
                                    );
                                }
                            }
                            transparent = Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                        }
                        "allow_unknown_fields" => {
                            if matches!(struct_.fields, syn::Fields::Unnamed(_)) {
                                errors.push(
                                    span,
                                    "`allow_unknown_fields` not allowed on tuple struct",
                                );
                            }
                            if allow_unknown_fields.is_some() {
                                errors.push(span, "duplicate attribute for `allow_unknown_fields`");
                            }
                            allow_unknown_fields =
                                Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                        }
                        "default" => {
                            if default.is_some() {
                                errors.push(span, "duplicate attribute for `default`");
                            }
                            if matches!(struct_.fields, syn::Fields::Unit) {
                                errors.push(span, "`default` not allowed on unit struct");
                            }
                            default = Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                        }
                        "crate" => {
                            if crate_.is_some() {
                                errors.push(span, "duplicate attribute for `crate`");
                            }
                            crate_ = Some(ParseMetaItem::parse_meta_item_named(input, span)?);
                        }
                        "attributes" => {
                            let attrs = mod_path_vec::parse_meta_item_named(input, span)?;
                            attributes.extend(attrs.into_iter());
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
                let fields = {
                    let mut fields = fields;
                    if default.is_none() {
                        for field in &mut fields {
                            if let Some(span) =
                                field.skip.and_then(|skip| (*skip).then_some(skip.span()))
                            {
                                if field.default.is_none() {
                                    field.default = Some(FieldDefault::Default(span));
                                }
                            }
                        }
                    }
                    fields
                };
                let mut all_idents = HashSet::new();
                let mut container = None;
                for field in &fields {
                    for ident in &field.idents {
                        if all_idents.contains(ident) {
                            errors.push_spanned(
                                ident,
                                format_args!("duplicate field name for `{}`", ident),
                            );
                        } else {
                            all_idents.insert(ident.clone());
                        }
                    }
                    if let Some(c) = field.container.as_ref() {
                        if container.is_some() {
                            errors.push(c.span(), "Duplicate `container` field")
                        } else {
                            container = Some(c);
                        }
                    }
                }
                if matches!(struct_.fields, syn::Fields::Unnamed(_)) {
                    let mut has_default_gap = false;
                    for field in fields.iter().rev() {
                        if field.is_parsable() && !field.is_flat() {
                            if let Some(default) = &field.default {
                                if has_default_gap {
                                    errors.push(
                                        default.span(),
                                        "`default` fields can only be at the end of a tuple struct",
                                    );
                                }
                            } else {
                                has_default_gap = true;
                            }
                        }
                    }
                }
                errors.check()?;
                Ok(Self {
                    struct_,
                    fields,
                    default,
                    crate_,
                    transparent,
                    allow_unknown_fields,
                    attributes,
                })
            },
        )
    }
}

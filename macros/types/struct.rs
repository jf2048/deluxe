use super::*;
use deluxe_core::{
    parse_helpers::{self, FieldStatus},
    ParseAttributes, ParseMetaItem, ParseMode, Result,
};
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

impl quote::ToTokens for StructTransparent {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        syn::LitBool::new(self.value, self.span).to_tokens(tokens);
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
        let errors = crate::Errors::new();
        let span = mode.to_full_span(inputs);
        let mut flatten_named = FieldStatus::None;
        let mut flatten_unnamed = FieldStatus::None;
        let mut append = FieldStatus::None;
        let mut rest = FieldStatus::None;
        errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "flatten_named" => {
                    flatten_named.parse_named_item("flatten_named", input, span, &errors)
                }
                "flatten_unnamed" => {
                    flatten_unnamed.parse_named_item("flatten_unnamed", input, span, &errors)
                }
                "append" => append.parse_named_item("append", input, span, &errors),
                "rest" => rest.parse_named_item("rest", input, span, &errors),
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(
                        path,
                        span,
                        &["flatten_named", "flatten_unnamed", "append", "rest"],
                    ));
                    parse_helpers::skip_meta_item(input);
                }
            }
            Ok(())
        }));
        errors.check()?;
        Ok(Self {
            span,
            value: true,
            flatten_named: flatten_named.into(),
            flatten_unnamed: flatten_unnamed.into(),
            append: append.into(),
            rest: rest.into(),
        })
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
    pub and_thens: Vec<TokenStream>,
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
            "and_then",
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
        inline_expr: &TokenStream,
        allowed_expr: &TokenStream,
    ) -> ItemDef {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let target = self.default.as_ref().map(|_| quote_mixed! { target });
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
        let post = if self.and_thens.is_empty() {
            post
        } else {
            let and_thens = &self.and_thens;
            quote_mixed! {
                let ret = {
                    #post
                };
                let errors = #crate_::Errors::new();
                let ret = ret.ok();
                #(let ret = ret.and_then(|v| {
                    let f = #and_thens;
                    errors.push_result(f(v))
                });)*
                errors.check()?;
                #crate_::Result::Ok(ret.unwrap())
            }
        };
        let default_set = self.default.as_ref().map(|d| {
            let expr = d.to_expr(Some(&syn::parse_quote_spanned! { d.span() => Self }), priv_);
            quote_mixed! {
                let mut target: Self = (#expr);
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
                            quote::format_ident!("field{i}", span = Span::mixed_site()),
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
                #name = #priv_::FieldStatus::Some(#parse_ty::parse_meta_item(input, _mode)?);
                #post
            };
            let inline = Some(quote_mixed! {
                #pre
                #name = #priv_::FieldStatus::Some(#parse_ty::parse_meta_item_inline(inputs, _mode)?);
                #post
            });
            let flag = Some(quote_mixed! {
                #pre
                #name = #priv_::FieldStatus::Some(#parse_ty::parse_meta_item_flag(span)?);
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
                            #name = #priv_::FieldStatus::Some(#flat_ty::parse_meta_flat_named(inputs, mode, prefix, validate)?);
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
                            #name = #priv_::FieldStatus::Some(#flat_ty::parse_meta_flat_unnamed(inputs, mode, index)?);
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
                            #name = #priv_::FieldStatus::Some(#rest_ty::parse_meta_rest(inputs, exclude)?);
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
                            #name = #priv_::FieldStatus::Some(#append_ty::parse_meta_append(inputs, paths)?);
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
                let mut transparent = FieldStatus::None;
                let mut allow_unknown_fields = FieldStatus::None;
                let mut default = FieldStatus::None;
                let mut crate_ = FieldStatus::None;
                let mut and_thens = Vec::new();
                let mut attributes = Vec::new();
                let fields = struct_
                    .fields
                    .iter()
                    .filter_map(|f| errors.push_result(Field::parse_attributes(f)))
                    .collect::<Vec<_>>();
                errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "transparent" => {
                            transparent.parse_named_item_with(
                                "transparent",
                                input,
                                span,
                                &errors,
                                |input, _, span| {
                                    let mut iter = fields.iter().filter(|f| f.is_parsable());
                                    if let Some(first) = iter.next() {
                                        if first.flatten.as_ref().map(|f| f.value).unwrap_or(false)
                                        {
                                            return Err(syn::Error::new(
                                                span,
                                                "`transparent` struct field cannot be `flat`",
                                            ));
                                        } else if first.append.map(|v| *v).unwrap_or(false) {
                                            return Err(syn::Error::new(
                                                span,
                                                "`transparent` struct field cannot be `append`",
                                            ));
                                        } else if iter.next().is_none() {
                                            return <_>::parse_meta_item_named(input, path, span);
                                        }
                                    }
                                    Err(syn::Error::new(
                                        span,
                                        "`transparent` struct must have only one parseable field",
                                    ))
                                },
                            );
                        }
                        "allow_unknown_fields" => {
                            if matches!(struct_.fields, syn::Fields::Unnamed(_)) {
                                errors.push(
                                    span,
                                    "`allow_unknown_fields` not allowed on tuple struct",
                                );
                                parse_helpers::skip_meta_item(input);
                            } else {
                                allow_unknown_fields.parse_named_item(
                                    "allow_unknown_fields",
                                    input,
                                    span,
                                    &errors,
                                );
                            }
                        }
                        "default" => {
                            if matches!(struct_.fields, syn::Fields::Unit) {
                                errors.push(span, "`default` not allowed on unit struct");
                                parse_helpers::skip_meta_item(input);
                            } else {
                                default.parse_named_item("default", input, span, &errors);
                            }
                        }
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
                                format_args!("duplicate field name for `{ident}`"),
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
                    default: default.into(),
                    crate_: crate_.into(),
                    transparent: transparent.into(),
                    allow_unknown_fields: allow_unknown_fields.into(),
                    attributes,
                    and_thens,
                })
            },
        )
    }
}

use deluxe_core::{
    parse_helpers::{self, FieldStatus},
    ParseAttributes, ParseMetaItem, ParseMode, Result, SpannedValue,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, TokenStreamExt};
use std::borrow::{Borrow, Cow};
use syn::{
    parse::{ParseBuffer, ParseStream},
    spanned::Spanned,
};

pub enum FieldDefault {
    Default(Span),
    Expr(TokenStream),
}

impl FieldDefault {
    pub fn to_expr(&self, ty: Option<&syn::Type>, priv_: &syn::Path) -> Cow<TokenStream> {
        match self {
            FieldDefault::Default(span) => {
                let ty = if let Some(ty) = ty {
                    quote_spanned! { ty.span() => #ty }
                } else {
                    quote_spanned! { *span => _ }
                };
                Cow::Owned(quote_spanned! { *span =>
                    <#ty as #priv_::Default>::default()
                })
            }
            FieldDefault::Expr(expr) => Cow::Borrowed(expr),
        }
    }
}

impl ParseMetaItem for FieldDefault {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(Self::Expr(input.parse()?))
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self::Default(span))
    }
}

impl quote::ToTokens for FieldDefault {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Default(span) => tokens.append_all(quote_spanned!(*span => Default::default())),
            Self::Expr(expr) => tokens.append_all(expr.clone()),
        }
    }
}

#[derive(Debug)]
pub struct FieldFlatten {
    pub span: Span,
    pub value: bool,
    pub prefix: Option<syn::Path>,
}

impl quote::ToTokens for FieldFlatten {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        syn::LitBool::new(self.value, self.span).to_tokens(tokens);
    }
}

impl ParseMetaItem for FieldFlatten {
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            Ok(Self {
                span: input.span(),
                value: input.parse::<syn::LitBool>()?.value(),
                prefix: None,
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
        let mut prefix = FieldStatus::None;
        errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "prefix" => prefix.parse_named_item_with(
                    "prefix",
                    input,
                    span,
                    &errors,
                    deluxe_core::with::any_path::parse_meta_item_named,
                ),
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(path, span, &["prefix"]));
                    parse_helpers::skip_meta_item(input);
                }
            }
            Ok(())
        }));
        errors.check()?;
        Ok(Self {
            span,
            value: true,
            prefix: prefix.into(),
        })
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self {
            span,
            value: true,
            prefix: None,
        })
    }
}

pub struct FieldContainer {
    pub span: Span,
    pub value: bool,
    pub lifetime: Option<syn::Lifetime>,
    pub ty: Option<syn::Type>,
}

impl quote::ToTokens for FieldContainer {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        syn::LitBool::new(self.value, self.span).to_tokens(tokens);
    }
}

impl ParseMetaItem for FieldContainer {
    fn parse_meta_item(input: ParseStream, mode: ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            let value = input.parse::<syn::LitBool>()?;
            Ok(Self {
                span: value.span(),
                value: value.value(),
                lifetime: None,
                ty: None,
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
        let mut lifetime = FieldStatus::None;
        let mut ty = FieldStatus::None;
        errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "lifetime" => lifetime.parse_named_item("lifetime", input, span, &errors),
                "ty" => ty.parse_named_item("ty", input, span, &errors),
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(
                        path,
                        span,
                        &["lifetime", "ty"],
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
            lifetime: lifetime.into(),
            ty: ty.into(),
        })
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self {
            span,
            value: true,
            lifetime: None,
            ty: None,
        })
    }
}

pub enum Transform {
    Map(TokenStream),
    AndThen(TokenStream),
}

pub struct Field<'f> {
    pub field: &'f syn::Field,
    pub idents: Vec<syn::Ident>,
    pub default: Option<FieldDefault>,
    pub with: Option<syn::Path>,
    pub flatten: Option<FieldFlatten>,
    pub append: Option<SpannedValue<bool>>,
    pub rest: Option<SpannedValue<bool>>,
    pub container: Option<FieldContainer>,
    pub skip: Option<SpannedValue<bool>>,
    pub transforms: Vec<Transform>,
}

pub struct ItemDef {
    pub parse: TokenStream,
    pub inline: Option<TokenStream>,
    pub flag: Option<TokenStream>,
    pub extra_traits: Option<TokenStream>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenMode {
    ParseMetaItem,
    ParseAttributes,
    ExtractAttributes,
}

pub enum ParseTarget<'t> {
    Init(Option<&'t syn::Ident>),
    Var(&'t TokenStream),
}

pub(super) struct FieldData<'t, 'i, 'a> {
    pub mode: TokenMode,
    pub target: ParseTarget<'t>,
    pub inline_expr: &'i TokenStream,
    pub allowed_expr: &'a TokenStream,
    pub transparent: bool,
    pub variant: bool,
    pub allow_unknown_fields: bool,
}

impl<'f> Field<'f> {
    #[inline]
    pub fn is_flat(&self) -> bool {
        self.flatten.as_ref().map(|f| f.value).unwrap_or(false)
            || self.append.map(|v| *v).unwrap_or(false)
            || self.rest.map(|v| *v).unwrap_or(false)
    }
    #[inline]
    pub fn is_container(&self) -> bool {
        self.container.as_ref().map(|f| f.value).unwrap_or(false)
    }
    #[inline]
    pub fn is_parsable(&self) -> bool {
        !self.is_container() && !self.skip.map(|v| *v).unwrap_or(false)
    }
    #[inline]
    pub fn constraint_ty(&self) -> TokenStream {
        let ty = &self.field.ty;
        if self.transforms.is_empty() {
            quote_spanned! { ty.span() => #ty }
        } else {
            quote_spanned! { ty.span() => _ }
        }
    }
    pub fn parse_path(&self, crate_: &syn::Path, trait_: &str) -> TokenStream {
        self.with
            .as_ref()
            .map(|m| quote_spanned! { m.span() => #m })
            .unwrap_or_else(|| {
                let ty = self.constraint_ty();
                let trait_ = syn::Ident::new(trait_, Span::mixed_site());
                quote_spanned! { ty.span() => <#ty as #crate_::#trait_> }
            })
    }
    pub fn field_names() -> &'static [&'static str] {
        &[
            "rename",
            "flatten",
            "append",
            "rest",
            "default",
            "alias",
            "with",
            "container",
            "skip",
            "map",
            "and_then",
        ]
    }
    pub(super) fn to_accepts_all_tokens(
        fields: &[Self],
        crate_: &syn::Path,
    ) -> Option<TokenStream> {
        if fields.iter().any(|f| f.rest.map(|v| *v).unwrap_or(false)) {
            return Some(quote_mixed! { true });
        }
        let mut flat_tys = fields
            .iter()
            .filter(|f| f.flatten.as_ref().map(|f| f.value).unwrap_or(false))
            .map(|f| &f.field.ty)
            .peekable();
        flat_tys.peek().is_some().then(|| {
            quote_mixed! { #(<#flat_tys as #crate_::ParseMetaFlatNamed>::ACCEPTS_ALL)||* }
        })
    }
    pub(super) fn to_field_names_tokens(
        fields: &[Self],
        crate_: &syn::Path,
        priv_: &syn::Path,
    ) -> TokenStream {
        if fields
            .iter()
            .any(|f| f.flatten.as_ref().map(|f| f.value).unwrap_or(false))
        {
            let names = fields.iter().filter(|f| f.is_parsable()).map(|f| match &f.flatten {
                Some(FieldFlatten {
                    value: true,
                    prefix: Some(prefix),
                    ..
                }) => {
                    let ty = f.constraint_ty();
                    let prefix = parse_helpers::key_to_string(prefix);
                    let names = quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                    };
                    quote_mixed! {
                        #priv_::parse_helpers::extend_from_owned(
                            &mut vec,
                            {
                                static CELL: #priv_::SyncOnceCell<#priv_::Vec<#priv_::Cow<'static, #priv_::str>>>
                                    = #priv_::SyncOnceCell::new();
                                CELL.get_or_init(|| #priv_::parse_helpers::join_paths(#prefix, #names))
                            },
                        );
                    }
                }
                Some(FieldFlatten {
                    value: true,
                    prefix: None,
                    ..
                }) => {
                    let ty = f.constraint_ty();
                    let names = quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                    };
                    quote_mixed! {
                        vec.extend_from_slice(#names);
                    }
                }
                _ => {
                    let names = f.idents.iter().map(|i| i.to_string());
                    quote_mixed! {
                        #(vec.push(#names);)*
                    }
                }
            });
            quote_mixed! {
                {
                    static CELL: #priv_::SyncOnceCell<#priv_::Vec<&'static #priv_::str>> = #priv_::SyncOnceCell::new();
                    CELL.get_or_init(|| {
                        let mut vec = #priv_::Vec::new();
                        #(#names)*
                        vec
                    }).as_slice()
                }
            }
        } else {
            let names = fields.iter().filter_map(|f| {
                if !f.is_parsable() {
                    return None;
                }
                let names = f.idents.iter().map(|i| i.to_string());
                Some(quote_mixed! { #(#names),* })
            });
            quote_mixed! {
                &[#(#names),*]
            }
        }
    }
    fn to_parse_call_tokens(
        &self,
        inputs_expr: &TokenStream,
        allowed_expr: &TokenStream,
        crate_: &syn::Path,
        priv_: &syn::Path,
    ) -> TokenStream {
        let ty = &self.field.ty;
        if self.append.map(|v| *v).unwrap_or(false) {
            let path = self.parse_path(crate_, "ParseMetaAppend");
            let idents = self.idents.iter().map(|i| i.to_string());
            quote_mixed! {
                #path::parse_meta_append(
                    #inputs_expr,
                    &#priv_::parse_helpers::join_paths(prefix, &[#(#idents),*]),
                )
            }
        } else if self.rest.map(|v| *v).unwrap_or(false) {
            let path = self.parse_path(crate_, "ParseMetaRest");
            quote_mixed! {
                #path::parse_meta_rest(#inputs_expr, #allowed_expr)
            }
        } else if let Some(FieldFlatten {
            value: true,
            prefix,
            ..
        }) = self.flatten.as_ref()
        {
            if self.field.ident.is_some() {
                let prefix = match prefix {
                    Some(prefix) => {
                        let prefix = parse_helpers::key_to_string(prefix);
                        quote_mixed! {
                            &#priv_::parse_helpers::join_prefix(prefix, #prefix)
                        }
                    }
                    None => quote_mixed! { "" },
                };
                let path = self.parse_path(crate_, "ParseMetaFlatNamed");
                quote_mixed! {
                    #path::parse_meta_flat_named(
                        #inputs_expr,
                        #crate_::ParseMode::Named(span),
                        #prefix,
                        false
                    )
                }
            } else {
                let path = self.parse_path(crate_, "ParseMetaFlatUnnamed");
                quote_mixed! {
                    #path::parse_meta_flat_unnamed(inputs, #crate_::ParseMode::Unnamed, index)
                }
            }
        } else if self.field.ident.is_some() {
            let path = self.parse_path(crate_, "ParseMetaItem");
            // named field
            match &self.with {
                Some(m) => {
                    let value_ident = syn::Ident::new("value", Span::mixed_site());
                    let input_ident = syn::Ident::new("input", Span::mixed_site());
                    let p_ident = syn::Ident::new("p", Span::mixed_site());
                    let span_ident = syn::Ident::new("span", Span::mixed_site());
                    // bind the return to a variable to span a type conversion error properly
                    quote_spanned! { m.span() =>
                        {
                            let #value_ident = #path::parse_meta_item_named(#input_ident, #p_ident, #span_ident);
                            #value_ident
                        }
                    }
                }
                None => {
                    let func = quote_spanned! { ty.span() => #path::parse_meta_item_named };
                    quote_mixed! {
                        #func(input, p, span)
                    }
                }
            }
        } else {
            let path = self.parse_path(crate_, "ParseMetaItem");
            // unnamed field
            match &self.with {
                Some(m) => {
                    let value_ident = syn::Ident::new("value", Span::mixed_site());
                    let input_ident = syn::Ident::new("input", Span::mixed_site());
                    quote_spanned! { m.span() =>
                        {
                            let #value_ident = #path::parse_meta_item(#input_ident, #crate_::ParseMode::Unnamed);
                            #value_ident
                        }
                    }
                }
                None => {
                    quote_mixed! {
                        #path::parse_meta_item(input, #crate_::ParseMode::Unnamed)
                    }
                }
            }
        }
    }
    pub(super) fn to_pre_post_tokens(
        fields: &[Self],
        orig: &syn::Fields,
        crate_: &syn::Path,
        data: &FieldData,
    ) -> (TokenStream, TokenStream) {
        let FieldData {
            mode,
            target,
            allowed_expr,
            transparent,
            variant,
            ..
        } = data;
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let is_unnamed = matches!(orig, syn::Fields::Unnamed(_));
        let names = fields
            .iter()
            .enumerate()
            .map(|(i, _)| quote::format_ident!("field{i}", span = Span::mixed_site()))
            .collect::<Vec<_>>();
        let container_def = fields.iter().enumerate().filter_map(|(i, f)| {
            (f.is_container() && *mode != TokenMode::ParseMetaItem).then(|| {
                let name = names[i].clone();
                let func = match mode {
                    TokenMode::ParseAttributes => quote! { container_from },
                    TokenMode::ExtractAttributes => quote! { container_from_mut },
                    _ => unreachable!(),
                };
                quote_mixed! {
                    #name = #priv_::FieldStatus::Some(#crate_::ContainerFrom::#func(obj));
                }
            })
        });
        let field_errors = {
            let mut cur_index = 0usize;
            let mut extra_counts = quote! {};
            let last_flat = is_unnamed
                .then(|| {
                    fields
                        .iter()
                        .enumerate()
                        .filter(|(_, f)| f.is_parsable() && f.is_flat())
                        .last()
                        .map(|(i, _)| i)
                })
                .flatten();
            fields
                .iter()
                .enumerate()
                .filter_map(|(i, f)| {
                    if f.is_container() || *transparent {
                        return None;
                    }
                    let push = (f.default.is_none() && !matches!(target, ParseTarget::Var(_)))
                        .then(|| {
                            let name = &names[i];
                            if is_unnamed {
                                if f.is_flat() {
                                    let inputs_expr = if Some(i) == last_flat {
                                        quote_mixed! { inputs }
                                    } else {
                                        quote_mixed! {
                                            &#priv_::parse_helpers::fork_inputs(inputs)
                                        }
                                    };
                                    let call = f.to_parse_call_tokens(&inputs_expr, allowed_expr, crate_, priv_);
                                    let span = if *variant {
                                        quote_mixed! { span }
                                    } else {
                                        quote_mixed! { #priv_::Span::call_site() }
                                    };
                                    let flat_path = f.parse_path(crate_, "ParseMetaFlatUnnamed");
                                    quote_mixed! {
                                        if #name.is_none() {
                                            let _mode = #crate_::ParseMode::Unnamed;
                                            #priv_::parse_helpers::parse_empty(#span, |input| {
                                                let inputs = &[input];
                                                match errors.push_result(#call) {
                                                    #priv_::Option::Some(v) => #name = #priv_::FieldStatus::Some(v),
                                                    #priv_::Option::None => #name = #priv_::FieldStatus::ParseError,
                                                }
                                                #crate_::Result::Ok(())
                                            })?;
                                            if let #priv_::Option::Some(count) = #flat_path::field_count() {
                                                index += count;
                                            }
                                        }
                                    }
                                } else {
                                    let path = f.parse_path(crate_, "ParseMetaItem");
                                    let name_format = quote_mixed! { #priv_::format!("{}", name) };
                                    let name_format = if *mode == TokenMode::ParseMetaItem {
                                        name_format
                                    } else {
                                        quote_mixed! {
                                            if let #priv_::Option::Some(path_name) = path_name {
                                                #priv_::format!("{} on #[{}]", name, path_name)
                                            } else {
                                                #name_format
                                            }
                                        }
                                    };
                                    quote_mixed! {
                                        if #name.is_none() {
                                            let span = _mode.to_full_span(inputs);
                                            let name = index + #cur_index #extra_counts;
                                            let name = #name_format;
                                            if let #priv_::Option::Some(v) = errors.push_result(
                                                #path::missing_meta_item(
                                                    &name,
                                                    span,
                                                ),
                                            ) {
                                                #name = #priv_::FieldStatus::Some(v);
                                            }
                                        }
                                    }
                                }
                            } else if !f.is_flat() {
                                let field = f.field.ident.as_ref().unwrap().to_string();
                                let path = f.parse_path(crate_, "ParseMetaItem");
                                let name_format = quote_mixed! { #priv_::format!("`{}`", name) };
                                let name_format = if *mode == TokenMode::ParseMetaItem {
                                    name_format
                                } else {
                                    quote_mixed! {
                                        if let #priv_::Option::Some(path_name) = path_name {
                                            #priv_::format!("#[{}({})]", path_name, name)
                                        } else {
                                            #name_format
                                        }
                                    }
                                };
                                quote_mixed! {
                                    if #name.is_none() {
                                        let name = #priv_::parse_helpers::join_path(prefix, #field);
                                        let name = #name_format;
                                        if let #priv_::Option::Some(v) = errors.push_result(
                                            #path::missing_meta_item(
                                                &name,
                                                span,
                                            ),
                                        ) {
                                            #name = #priv_::FieldStatus::Some(v);
                                        }
                                    }
                                }
                            } else {
                                quote! {}
                            }
                        });
                    if is_unnamed {
                        let ty = f.constraint_ty();
                        if f.flatten.as_ref().map(|f| f.value).unwrap_or(false) {
                            extra_counts.extend(quote_spanned! { ty.span() =>
                                + <#ty as #crate_::ParseMetaFlatUnnamed>::field_count().unwrap_or(0)
                            });
                        } else {
                            cur_index += 1;
                        }
                    }
                    push
                })
                .collect::<Vec<_>>()
        };
        let field_sets = fields.iter().enumerate().map(|(i, f)| {
            let name = &names[i];
            let ty = &f.field.ty;
            let transforms = f.transforms.iter().enumerate().map(|(ti, t)| {
                let constraint = match ti + 1 == f.transforms.len() {
                    true => quote_spanned! { ty.span() => #ty },
                    false => quote_spanned! { ty.span() => _ },
                };
                match t {
                    Transform::Map(expr) => quote_mixed! {
                        let #name: #priv_::FieldStatus<#constraint> = #name.map((#expr));
                    },
                    Transform::AndThen(expr) => quote_mixed! {
                        let #name: #priv_::FieldStatus<#constraint> = #name.and_then(|v| {
                            match errors.push_result((#expr)(v)) {
                                #priv_::Option::Some(v) => #priv_::FieldStatus::Some(v),
                                _ => #priv_::FieldStatus::ParseError,
                            }
                        });
                    },
                }
            });
            let set_default = f.default.as_ref().map(|def| {
                let expr = def.to_expr(f.transforms.is_empty().then_some(ty), priv_);
                quote_mixed! {
                    let #name = #name.or_else(|| #priv_::FieldStatus::Some((#expr)));
                }
            });
            quote_mixed! {
                #set_default
                #(#transforms)*
            }
        });
        let field_unwraps = fields.iter().enumerate().filter_map(|(i, _)| {
            (!matches!(target, ParseTarget::Var(_))).then(|| {
                let name = &names[i];
                quote_mixed! {
                    let #name = #name.unwrap_or_else(|| #priv_::unreachable!());
                }
            })
        });

        let option_inits = fields.iter().enumerate().map(|(i, f)| {
            let name = &names[i];
            let ty = f.constraint_ty();
            quote_mixed! {
                let mut #name: #priv_::FieldStatus::<#ty> = #priv_::FieldStatus::None;
            }
        });
        let errors_init = (!transparent).then(|| {
            quote_mixed! { let errors = #crate_::Errors::new(); }
        });
        let errors_check = (!transparent).then(|| {
            quote_mixed! {
                errors.check()?;
            }
        });

        match orig {
            syn::Fields::Named(_) => {
                let last_flat = fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.is_parsable() && f.is_flat())
                    .last()
                    .map(|(i, _)| i);
                let flat_fields = fields.iter().enumerate().filter_map(|(i, f)| {
                    if !f.is_flat() || !f.is_parsable() {
                        return None;
                    }
                    let inputs_expr = if Some(i) == last_flat {
                        quote_mixed! { inputs }
                    } else {
                        quote_mixed! {
                            &#priv_::parse_helpers::fork_inputs(inputs)
                        }
                    };
                    let call = f.to_parse_call_tokens(&inputs_expr, allowed_expr, crate_, priv_);
                    let name = &names[i];
                    Some(quote_mixed! {
                        match errors.push_result(#call) {
                            #priv_::Option::Some(val) => #name = #priv_::FieldStatus::Some(val),
                            #priv_::Option::None => #name = #priv_::FieldStatus::ParseError,
                        }
                    })
                });
                let ret = match target {
                    ParseTarget::Init(variant) => {
                        let field_defs = fields.iter().enumerate().map(|(i, f)| {
                            let ident = f.field.ident.as_ref().unwrap();
                            let name = &names[i];
                            quote_mixed! { #ident: #name }
                        });
                        let variant = variant.iter();
                        quote_mixed! {
                            #crate_::Result::Ok(Self #(::#variant)* {
                                #(#field_defs),*
                            })
                        }
                    }
                    ParseTarget::Var(target) => {
                        let field_defs = fields.iter().enumerate().map(|(i, f)| {
                            let ident = f.field.ident.as_ref().unwrap();
                            let name = &names[i];
                            quote_mixed! {
                                if let #priv_::FieldStatus::Some(val) = #name {
                                    #target.#ident = val;
                                }
                            }
                        });
                        quote_mixed! {
                            #(#field_defs)*
                            #crate_::Result::Ok(#target)
                        }
                    }
                };
                let pre = quote_mixed! {
                    #(#option_inits)*
                    #errors_init
                };
                let post = quote_mixed! {
                    #(#flat_fields)*
                    #(#container_def)*
                    #(#field_errors)*
                    #(#field_sets)*
                    #errors_check
                    #(#field_unwraps)*
                    #ret
                };
                (pre, post)
            }
            syn::Fields::Unnamed(_) => {
                let ret = match target {
                    ParseTarget::Init(variant) => {
                        let variant = variant.iter();
                        quote_mixed! {
                            #crate_::Result::Ok(Self #(::#variant)* (#(#names),*))
                        }
                    }
                    ParseTarget::Var(target) => {
                        let field_defs = fields.iter().enumerate().map(|(i, _)| {
                            let name = &names[i];
                            let i = syn::Index::from(i);
                            quote_mixed! {
                                if let #priv_::FieldStatus::Some(val) = #name {
                                    #target.#i = val;
                                }
                            }
                        });
                        quote_mixed! {
                            #(#field_defs)*
                            #crate_::Result::Ok(#target)
                        }
                    }
                };
                let pre = quote_mixed! {
                    #(#option_inits)*
                    #errors_init
                };
                let post = quote_mixed! {
                    #(#container_def)*
                    #(#field_errors)*
                    #(#field_sets)*
                    #errors_check
                    #(#field_unwraps)*
                    #ret
                };
                (pre, post)
            }
            syn::Fields::Unit => {
                let variant = match target {
                    ParseTarget::Init(variant) => *variant,
                    _ => None,
                }
                .into_iter();
                let pre = quote_mixed! {};
                let post = quote_mixed! {
                    #crate_::Result::Ok(Self #(::#variant)*)
                };
                (pre, post)
            }
        }
    }
    pub(super) fn to_parsing_tokens(
        fields: &[Self],
        orig: &syn::Fields,
        crate_: &syn::Path,
        (pre, post): (&TokenStream, &TokenStream),
        data: FieldData,
    ) -> ItemDef {
        let FieldData {
            mode,
            target,
            inline_expr,
            allowed_expr,
            allow_unknown_fields,
            ..
        } = data;
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let pub_fields = fields.iter().filter(|f| f.is_parsable());
        let any_flat = pub_fields.clone().any(|f| f.is_flat());
        let names = fields
            .iter()
            .enumerate()
            .map(|(i, _)| quote::format_ident!("field{i}", span = Span::mixed_site()))
            .collect::<Vec<_>>();
        let inputs_expr = any_flat
            .then(|| {
                quote_mixed! {
                    &#priv_::parse_helpers::fork_inputs(inputs)
                }
            })
            .unwrap_or_else(|| {
                quote_mixed! {
                    inputs
                }
            });
        let (parse, inline, flag) = match orig {
            syn::Fields::Named(_) => {
                let field_matches = fields.iter().enumerate().filter_map(|(i, f)| {
                    if f.is_flat() || !f.is_parsable() {
                        return None;
                    }
                    let name = names[i].clone();
                    let first_ident = f.idents.first().unwrap().to_string();
                    let idents = f.idents.iter().map(|i| i.to_string());
                    let call = f.to_parse_call_tokens(&inputs_expr, allowed_expr, crate_, priv_);
                    Some(quote_mixed! {
                        #(#priv_::Option::Some(#idents))|* => {
                            #name.parse_named_item_with(#first_ident, input, span, &errors, |input, p, span| {
                                #call
                            });
                        }
                    })
                });
                let validate = (!allow_unknown_fields).then(|| {
                    quote_mixed! {
                        if validate {
                            #priv_::parse_helpers::check_unknown_attribute(
                                p, span, #allowed_expr, &errors
                            );
                        }
                    }
                });
                (
                    quote_mixed! {
                        <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            move |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(quote_mixed! {
                        #pre
                        errors.push_result(#priv_::parse_helpers::parse_struct(
                            #inputs_expr,
                            |input, p, span| {
                                match p.strip_prefix(prefix) {
                                    #(#field_matches)*
                                    _ => {
                                        #validate
                                        #priv_::parse_helpers::skip_meta_item(input);
                                    }
                                }
                                #crate_::Result::Ok(())
                            },
                        ));
                        #post
                    }),
                    Some(quote_mixed! {
                        let _mode = #crate_::ParseMode::Named(span);
                        #priv_::parse_helpers::parse_empty(span, move |input| {
                            #inline_expr
                        })
                    }),
                )
            }
            syn::Fields::Unnamed(_) => {
                let field_matches = fields.iter().enumerate().filter(|(_, f)| {
                    f.is_parsable() && !f.append.map(|v| *v).unwrap_or(false) && !f.rest.map(|v| *v).unwrap_or(false)
                }).enumerate().map(|(index, (real_index, f))| {
                    let name = &names[real_index];
                    let call = f.to_parse_call_tokens(&inputs_expr, allowed_expr, crate_, priv_);
                    let increment = any_flat.then(|| {
                        let ty = f.constraint_ty();
                        match f.is_flat() {
                            true => quote_mixed! {
                                if let #priv_::Option::Some(count) = <#ty as #crate_::ParseMetaFlatUnnamed>::field_count() {
                                    index += count;
                                }
                            },
                            false => quote_mixed! {
                                index += 1;
                            }
                        }
                    });
                    quote_mixed! {
                        #index => {
                            #name.parse_unnamed_item_with(input, &errors, |input, mode| {
                                #call
                            });
                            #increment
                        }
                    }
                });
                let parse_fields = pub_fields.clone().next().is_some().then(|| {
                    let field_count = pub_fields.clone().count();
                    quote_mixed! {
                        errors.push_result(#priv_::parse_helpers::parse_tuple_struct(inputs, #field_count, |input, inputs, i| {
                            match i {
                                #(#field_matches)*
                                _ => #priv_::unreachable!(),
                            }
                            #crate_::Result::Ok(())
                        }));
                    }
                }).unwrap_or_else(|| {
                    quote_mixed! {
                        for input in inputs {
                            #priv_::parse_helpers::parse_eof_or_trailing_comma(
                                #priv_::Borrow::borrow(input),
                            )?;
                        }
                    }
                });
                let allows_empty = matches!(target, ParseTarget::Var(_))
                    || pub_fields
                        .clone()
                        .all(|f| f.is_flat() || f.default.is_some());
                (
                    quote_mixed! {
                        <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            move |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(quote_mixed! {
                        #pre
                        #parse_fields
                        #post
                    }),
                    allows_empty.then(|| {
                        quote_mixed! {
                            let _mode = #crate_::ParseMode::Unnamed;
                            #priv_::parse_helpers::parse_empty(span, move |input| {
                                #inline_expr
                            })
                        }
                    }),
                )
            }
            syn::Fields::Unit => {
                let inline = if mode == TokenMode::ParseMetaItem {
                    quote_mixed! {
                        #pre
                        <() as #crate_::ParseMetaItem>::parse_meta_item_inline(inputs, _mode)?;
                        #post
                    }
                } else {
                    quote_mixed! {
                        #pre
                        for input in inputs {
                            #priv_::parse_helpers::parse_eof_or_trailing_comma(
                                #priv_::Borrow::borrow(input),
                            )?;
                        }
                        #post
                    }
                };
                (
                    quote_mixed! {
                        <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            move |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(inline),
                    Some(quote_mixed! {
                        #pre
                        #post
                    }),
                )
            }
        };
        ItemDef {
            parse,
            inline,
            flag,
            extra_traits: None,
        }
    }
}

impl<'f> ParseAttributes<'f, syn::Field> for Field<'f> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(field: &'f syn::Field) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(
            parse_helpers::ref_tokens::<Self, _>(field),
            |inputs, _| {
                let named = field.ident.is_some();
                let errors = crate::Errors::new();
                let mut alias_span = FieldStatus::None;
                let mut idents = Vec::new();
                let mut default = FieldStatus::None;
                let mut with = FieldStatus::None;
                let mut flatten = FieldStatus::<FieldFlatten>::None;
                let mut append = FieldStatus::None;
                let mut rest = FieldStatus::None;
                let mut rename = FieldStatus::None;
                let mut container = FieldStatus::None;
                let mut skip = FieldStatus::None;
                let mut transforms = Vec::new();
                errors.push_result(parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "flatten" => flatten.parse_named_item("flatten", input, span, &errors),
                        "append" => {
                            if !named {
                                errors.push(span, "`append` not allowed on tuple struct field");
                                parse_helpers::skip_meta_item(input);
                            } else {
                                append.parse_named_item("append", input, span, &errors);
                            }
                        }
                        "rest" => {
                            if !named {
                                errors.push(span, "`rest` not allowed on tuple struct field");
                                parse_helpers::skip_meta_item(input);
                            } else {
                                rest.parse_named_item("rest", input, span, &errors);
                            }
                        }
                        "default" => default.parse_named_item("default", input, span, &errors),
                        "with" => with.parse_named_item("with", input, span, &errors),
                        "rename" => {
                            if !named {
                                errors.push(span, "`rename` not allowed on tuple struct field");
                            } else {
                                rename.parse_named_item_with(
                                    "rename",
                                    input,
                                    span,
                                    &errors,
                                    |input, _, span| {
                                        let name = <_>::parse_meta_item_named(input, path, span)?;
                                        if field.ident.as_ref() == Some(&name) {
                                            Err(syn::Error::new(
                                                span,
                                                "cannot rename field to its own name",
                                            ))
                                        } else if idents.contains(&name) {
                                            Err(syn::Error::new(
                                                span,
                                                format_args!("alias already given for `{name}`"),
                                            ))
                                        } else {
                                            idents.insert(0, name);
                                            Ok(span)
                                        }
                                    },
                                );
                            }
                        }
                        "alias" => {
                            if !named {
                                errors.push(span, "`alias` not allowed on tuple struct field");
                            } else {
                                match errors
                                    .push_result(<_>::parse_meta_item_named(input, path, span))
                                {
                                    Some(alias) => {
                                        if field.ident.as_ref() == Some(&alias) {
                                            errors.push(span, "cannot alias field to its own name");
                                        } else if idents.contains(&alias) {
                                            errors.push(
                                                span,
                                                format_args!("duplicate alias for `{alias}`"),
                                            );
                                        } else {
                                            idents.push(alias);
                                            if alias_span.is_none() {
                                                alias_span = FieldStatus::Some(span);
                                            }
                                        }
                                    }
                                    None => parse_helpers::skip_meta_item(input),
                                }
                            }
                        }
                        "container" => {
                            container.parse_named_item("container", input, span, &errors)
                        }
                        "skip" => skip.parse_named_item("container", input, span, &errors),
                        "map" => {
                            match errors.push_result(<_>::parse_meta_item_named(input, path, span))
                            {
                                Some(e) => transforms.push(Transform::Map(e)),
                                None => parse_helpers::skip_meta_item(input),
                            }
                        }
                        "and_then" => {
                            match errors.push_result(<_>::parse_meta_item_named(input, path, span))
                            {
                                Some(e) => transforms.push(Transform::AndThen(e)),
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
                deluxe_core::only_one!("", &errors, flatten, append, rest);
                deluxe_core::only_one!("", &errors, container, flatten);
                deluxe_core::only_one!("", &errors, container, rename);
                deluxe_core::only_one!("", &errors, container, with);
                deluxe_core::only_one!("", &errors, container, ("alias", alias_span.as_ref()));
                deluxe_core::only_one!("", &errors, default, flatten);
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                if rename.is_none() && !flatten.as_ref().map(|f| f.value).unwrap_or(false) {
                    if let Some(ident) = field.ident.as_ref() {
                        idents.insert(0, syn::ext::IdentExt::unraw(ident));
                    }
                }
                errors.check()?;
                Ok(Self {
                    field,
                    idents: idents.into_iter().collect(),
                    default: default.into(),
                    with: with.into(),
                    flatten: flatten.into(),
                    append: append.into(),
                    rest: rest.into(),
                    container: container.into(),
                    skip: skip.into(),
                    transforms,
                })
            },
        )
    }
}

use deluxe_core::{parse_helpers, ParseAttributes, ParseMode, Result, SpannedValue};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use std::{
    borrow::{Borrow, Cow},
    collections::{BTreeMap, BTreeSet, HashSet},
};
use syn::{
    parse::{ParseBuffer, ParseStream},
    parse_quote_spanned,
    spanned::Spanned,
};

pub enum FieldDefault {
    Default(Span),
    Expr(Box<syn::Expr>),
}

impl FieldDefault {
    pub fn to_expr(&self, ty: &syn::Type, priv_: &syn::Path) -> Cow<syn::Expr> {
        match self {
            FieldDefault::Default(span) => Cow::Owned(parse_quote_spanned! { *span =>
                <#ty as #priv_::Default>::default()
            }),
            FieldDefault::Expr(expr) => Cow::Borrowed(expr),
        }
    }
}

impl deluxe_core::ParseMetaItem for FieldDefault {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: ParseMode) -> Result<Self> {
        Ok(Self::Expr(input.parse()?))
    }
    #[inline]
    fn parse_meta_item_flag(span: Span) -> Result<Self> {
        Ok(Self::Default(span))
    }
}

impl Spanned for FieldDefault {
    #[inline]
    fn span(&self) -> Span {
        match self {
            Self::Default(span) => *span,
            Self::Expr(expr) => expr.span(),
        }
    }
}

pub struct FieldFlatten {
    pub span: Span,
    pub value: bool,
    pub prefix: Option<syn::Path>,
}

impl Spanned for FieldFlatten {
    fn span(&self) -> Span {
        self.span
    }
}

impl deluxe_core::ParseMetaItem for FieldFlatten {
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
        let mut flatten = Self {
            span: mode.to_full_span(inputs),
            value: true,
            prefix: None,
        };
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "prefix" => {
                    if flatten.prefix.is_some() {
                        errors.push(span, "duplicate attribute for `prefix`");
                    }
                    flatten.prefix = Some(deluxe_core::parse_named_meta_item_with!(
                        input,
                        span,
                        deluxe_core::with::mod_path
                    )?);
                }
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(path, span, &["prefix"]));
                    parse_helpers::skip_named_meta_item(input);
                }
            }
            Ok(())
        });
        if let Err(err) = res {
            errors.extend(err);
        }
        errors.check()?;
        Ok(flatten)
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

impl Spanned for FieldContainer {
    fn span(&self) -> Span {
        self.span
    }
}

impl deluxe_core::ParseMetaItem for FieldContainer {
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
        let mut container = Self {
            span: mode.to_full_span(inputs),
            value: true,
            lifetime: None,
            ty: None,
        };
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(inputs, |input, path, span| {
            match path {
                "lifetime" => {
                    if container.lifetime.is_some() {
                        errors.push(span, "duplicate attribute for `lifetime`");
                    }
                    container.lifetime = Some(parse_helpers::parse_named_meta_item(input, span)?);
                }
                "type" => {
                    if container.ty.is_some() {
                        errors.push(span, "duplicate attribute for `type`");
                    }
                    container.ty = Some(parse_helpers::parse_named_meta_item(input, span)?);
                }
                _ => {
                    errors.push_syn(parse_helpers::unknown_error(
                        path,
                        span,
                        &["lifetime", "type"],
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
        Ok(container)
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
    Var(&'t syn::Expr),
}

struct FieldData<'t, 'i, 'a> {
    mode: TokenMode,
    target: ParseTarget<'t>,
    inline_expr: &'i syn::Expr,
    allowed_expr: &'a syn::Expr,
    transparent: bool,
    allow_unknown_fields: bool,
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
        ]
    }
    fn to_pre_post_tokens(
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
            ..
        } = data;
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let is_unnamed = matches!(orig, syn::Fields::Unnamed(_));
        let names = fields
            .iter()
            .enumerate()
            .map(|(i, _)| quote::format_ident!("field{}", i, span = Span::mixed_site()))
            .collect::<Vec<_>>();
        let container_def = fields.iter().enumerate().filter_map(|(i, f)| {
            (f.is_container() && *mode != TokenMode::ParseMetaItem).then(|| {
                let name = names[i].clone();
                let func = match mode {
                    TokenMode::ParseAttributes => quote! { to_container },
                    TokenMode::ExtractAttributes => quote! { to_container_mut },
                    _ => unreachable!(),
                };
                quote_spanned! { Span::mixed_site() =>
                    #name = #priv_::Option::Some(#crate_::ToContainer::#func(obj));
                }
            })
        });
        let field_errors = {
            let mut cur_index = 0usize;
            let mut extra_counts = quote! {};
            fields
                .iter()
                .enumerate()
                .filter_map(|(i, f)| {
                    if f.is_container() || *transparent {
                        return None;
                    }
                    let push = f.default.is_none().then(|| {
                        let name = &names[i];
                        if is_unnamed {
                            quote_spanned! { Span::mixed_site() =>
                                if #name.is_none() {
                                    errors.push_call_site(#priv_::format_args!(
                                        "missing required field {}",
                                        index + #cur_index #extra_counts,
                                    ));
                                }
                            }
                        } else if !f.is_flat() {
                            let field = f.field.ident.as_ref().unwrap().to_string();
                            quote_spanned! { Span::mixed_site() =>
                                if #name.is_none() {
                                    errors.push_call_site(#priv_::format_args!(
                                        "missing required field `{}`",
                                        #priv_::parse_helpers::join_path(prefix, #field)
                                    ));
                                }
                            }
                        } else {
                            quote! {}
                        }
                    });
                    if is_unnamed {
                        let ty = &f.field.ty;
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
        let field_unwraps = fields.iter().enumerate().filter_map(|(i, f)| {
            let name = &names[i];
            let ty = &f.field.ty;
            if matches!(target, ParseTarget::Var(_)) {
                f.default.as_ref().map(|def| {
                    let expr = def.to_expr(ty, priv_);
                    quote_spanned! { Span::mixed_site() =>
                        if #name.is_none() {
                            #name = #priv_::Option::Some(#expr);
                        }
                    }
                })
            } else {
                Some(match &f.default {
                    Some(def) => {
                        let expr = def.to_expr(ty, priv_);
                        quote_spanned! { Span::mixed_site() =>
                            let #name = #name.unwrap_or_else(|| #expr);
                        }
                    }
                    None => quote_spanned! { Span::mixed_site() =>
                        let #name = #name.unwrap();
                    },
                })
            }
        });

        let option_inits = fields.iter().enumerate().map(|(i, f)| {
            let name = &names[i];
            let ty = &f.field.ty;
            quote_spanned! { Span::mixed_site() =>
                let mut #name: #priv_::Option::<#ty> = #priv_::Option::None;
            }
        });
        let errors_init = (!transparent).then(|| {
            quote_spanned! { Span::mixed_site() =>
                let errors = #crate_::Errors::new();
            }
        });
        let errors_check = (!transparent).then(|| {
            quote_spanned! { Span::mixed_site() =>
                errors.check()?;
            }
        });

        match orig {
            syn::Fields::Named(_) => {
                let last_flat = fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.is_parsable() && f.flatten.is_some())
                    .last()
                    .map(|(i, _)| i);
                let flat_fields = fields.iter().enumerate().filter_map(|(i, f)| {
                    if !f.is_parsable() {
                        return None;
                    }
                    let inputs_expr = if Some(i) == last_flat {
                        quote_spanned! { Span::mixed_site() => inputs }
                    } else {
                        quote_spanned! { Span::mixed_site() =>
                            &#priv_::parse_helpers::fork_inputs(inputs)
                        }
                    };
                    let func = if f.append.map(|v| *v).unwrap_or(false) {
                        let ty = &f.field.ty;
                        let func = quote_spanned! { ty.span() =>
                            <#ty as #crate_::ParseMetaAppend>::parse_meta_append
                        };
                        let idents = f.idents.iter().map(|i| i.to_string());
                        quote_spanned! { Span::mixed_site() =>
                            #func(
                                #inputs_expr,
                                &#priv_::parse_helpers::join_paths(prefix, &[#(#idents),*]),
                            )
                        }
                    } else if f.rest.map(|v| *v).unwrap_or(false) {
                        let ty = &f.field.ty;
                        let func = quote_spanned! { ty.span() =>
                            <#ty as #crate_::ParseMetaRest>::parse_meta_rest
                        };
                        quote_spanned! { Span::mixed_site() =>
                            #func(#inputs_expr, #allowed_expr)
                        }
                    } else if let Some(flatten) = f.flatten.as_ref() {
                        let ty = &f.field.ty;
                        let prefix = match &flatten.prefix {
                            Some(prefix) => {
                                let prefix = parse_helpers::path_to_string(prefix);
                                quote_spanned! { Span::mixed_site() =>
                                    &#priv_::parse_helpers::join_prefix(prefix, #prefix)
                                }
                            }
                            None => quote_spanned! { Span::mixed_site() => "" },
                        };
                        let func = quote_spanned! { ty.span() =>
                            <#ty as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named
                        };
                        quote_spanned! { Span::mixed_site() =>
                            #func(#inputs_expr, #prefix, false)
                        }
                    } else {
                        return None;
                    };
                    let name = &names[i];
                    Some(quote_spanned! { Span::mixed_site() =>
                        match #func {
                            #crate_::Result::Ok(val) => #name = #priv_::Option::Some(val),
                            #crate_::Result::Err(err) => errors.push_syn(err),
                        }
                    })
                });
                let ret = match target {
                    ParseTarget::Init(variant) => {
                        let field_defs = fields.iter().enumerate().map(|(i, f)| {
                            let ident = f.field.ident.as_ref().unwrap();
                            let name = &names[i];
                            quote_spanned! { Span::mixed_site() => #ident: #name }
                        });
                        let variant = variant.into_iter();
                        quote_spanned! { Span::mixed_site() =>
                            #crate_::Result::Ok(Self #(::#variant)* {
                                #(#field_defs),*
                            })
                        }
                    }
                    ParseTarget::Var(target) => {
                        let field_defs = fields.iter().enumerate().map(|(i, f)| {
                            let ident = f.field.ident.as_ref().unwrap();
                            let name = &names[i];
                            quote_spanned! { Span::mixed_site() =>
                                if let #priv_::Option::Some(val) = #name {
                                    #target.#ident = val;
                                }
                            }
                        });
                        quote_spanned! { Span::mixed_site() =>
                            #(#field_defs)*
                            #crate_::Result::Ok(#target)
                        }
                    }
                };
                let pre = quote_spanned! { Span::mixed_site() =>
                    #(#option_inits)*
                    #errors_init
                };
                let post = quote_spanned! { Span::mixed_site() =>
                    #(#flat_fields)*
                    #(#container_def)*
                    #(#field_errors)*
                    #errors_check
                    #(#field_unwraps)*
                    #ret
                };
                (pre, post)
            }
            syn::Fields::Unnamed(_) => {
                let ret = match target {
                    ParseTarget::Init(variant) => {
                        let variant = variant.into_iter();
                        quote_spanned! { Span::mixed_site() =>
                            #crate_::Result::Ok(Self #(::#variant)* (#(#names),*))
                        }
                    }
                    ParseTarget::Var(target) => {
                        let field_defs = fields.iter().enumerate().map(|(i, _)| {
                            let name = &names[i];
                            quote_spanned! { Span::mixed_site() => #target.#i = #name; }
                        });
                        quote_spanned! { Span::mixed_site() =>
                            #(#field_defs)*
                            #crate_::Result::Ok(#target)
                        }
                    }
                };
                let pre = quote_spanned! { Span::mixed_site() =>
                    #(#option_inits)*
                };
                let post = quote_spanned! { Span::mixed_site() =>
                    #(#container_def)*
                    #errors_init
                    #(#field_errors)*
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
                let pre = quote_spanned! { Span::mixed_site() => };
                let post = quote_spanned! { Span::mixed_site() =>
                    #crate_::Result::Ok(Self #(::#variant)*)
                };
                (pre, post)
            }
        }
    }
    fn to_parsing_tokens(
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
            .map(|(i, _)| quote::format_ident!("field{}", i, span = Span::mixed_site()))
            .collect::<Vec<_>>();
        let inputs_expr = any_flat
            .then(|| {
                quote_spanned! { Span::mixed_site() =>
                    &#priv_::parse_helpers::fork_inputs(inputs)
                }
            })
            .unwrap_or_else(|| {
                quote_spanned! { Span::mixed_site() =>
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
                    let error = format!("duplicate attribute for `{}`", f.idents.first().unwrap());
                    let idents = f.idents.iter().map(|i| i.to_string());
                    let call = match f.with.as_ref() {
                        Some(m) => {
                            let input_ident = syn::Ident::new("input", Span::mixed_site());
                            let span_ident = syn::Ident::new("span", Span::mixed_site());
                            // bind the return to a variable to span a type conversion error properly
                            quote_spanned! { m.span() =>
                                {
                                    let ____value = #crate_::parse_named_meta_item_with!(#input_ident, #span_ident, #m)?;
                                    ____value
                                }
                            }
                        }
                        None => {
                            let ty = &f.field.ty;
                            let func = quote_spanned! { ty.span() =>
                                #priv_::parse_helpers::parse_named_meta_item::<#ty>
                            };
                            quote_spanned! { Span::mixed_site() =>
                                #func(input, span)?
                            }
                        }
                    };
                    let set = quote_spanned! { f.field.ty.span() =>
                        #name = #priv_::Option::Some(#call);
                    };
                    Some(quote_spanned! { Span::mixed_site() =>
                        #(#priv_::Option::Some(#idents))|* => {
                            if #name.is_some() {
                                errors.push(span, #error);
                            }
                            #set
                        }
                    })
                });
                let validate = (!allow_unknown_fields).then(|| {
                    quote_spanned! { Span::mixed_site() =>
                        if validate {
                            #priv_::parse_helpers::check_unknown_attribute(
                                p, span, #allowed_expr, &errors
                            );
                        }
                    }
                });
                (
                    quote_spanned! { Span::mixed_site() =>
                        <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(quote_spanned! { Span::mixed_site() =>
                        #pre
                        #priv_::parse_helpers::parse_struct(
                            #inputs_expr,
                            |input, p, span| {
                                match p.strip_prefix(prefix) {
                                    #(#field_matches)*
                                    _ => {
                                        #validate
                                        #priv_::parse_helpers::skip_named_meta_item(input);
                                    }
                                }
                                #crate_::Result::Ok(())
                            },
                        )?;
                        #post
                    }),
                    Some(quote_spanned! { Span::mixed_site() =>
                        #priv_::parse_helpers::parse_empty_meta_item(span, #crate_::ParseMode::Named(span))
                    }),
                )
            }
            syn::Fields::Unnamed(_) => {
                let field_matches = fields.iter().enumerate().filter_map(|(index, f)| {
                    if !f.is_parsable() || f.append.map(|v| *v).unwrap_or(false) || f.rest.map(|v| *v).unwrap_or(false) {
                        return None;
                    }
                    let name = &names[index];
                    let ty = &f.field.ty;
                    let call = match (f.is_flat(), f.with.as_ref()) {
                        (true, Some(m)) => quote_spanned! { Span::mixed_site() =>
                            #m::parse_meta_flat_unnamed(inputs, index)
                        },
                        (false, Some(m)) => quote_spanned! { Span::mixed_site() =>
                            #m::parse_meta_item(input, #crate_::ParseMode::Unnamed)
                        },
                        (true, None) => {
                            let ty = quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaFlatUnnamed> };
                            quote_spanned! { Span::mixed_site() =>
                                #ty::parse_meta_flat_unnamed(inputs, index)
                            }
                        },
                        (false, None) => {
                            let ty = quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaItem> };
                            quote_spanned! { Span::mixed_site() =>
                                #ty::parse_meta_item(input, #crate_::ParseMode::Unnamed)
                            }
                        },
                    };
                    let increment = any_flat.then(|| {
                        match f.is_flat() {
                            true => quote_spanned! { Span::mixed_site() =>
                                if let #priv_::Option::Some(count) = <#ty as #crate_::ParseMetaFlatUnnamed>::field_count() {
                                    index += count;
                                }
                            },
                            false => quote_spanned! { Span::mixed_site() =>
                                index += 1;
                            }
                        }
                    });
                    Some(quote_spanned! { Span::mixed_site() =>
                        #index => {
                            #name = #priv_::Option::Some(#call?);
                            #increment
                        }
                    })
                });
                let parse_fields = pub_fields.clone().next().is_some().then(|| {
                    let field_count = pub_fields.clone().filter(|f| !f.is_flat()).count();
                    quote_spanned! { Span::mixed_site() =>
                        #priv_::parse_helpers::parse_tuple_struct(inputs, #field_count, |input, inputs, i| {
                            match i {
                                #(#field_matches)*
                                _ => #priv_::unreachable!(),
                            }
                            #crate_::Result::Ok(())
                        })?;
                    }
                }).unwrap_or_else(|| {
                    quote_spanned! { Span::mixed_site() =>
                        for input in inputs {
                            #priv_::parse_helpers::parse_eof_or_trailing_comma(
                                #priv_::Borrow::borrow(input),
                            )?;
                        }
                    }
                });
                (
                    quote_spanned! { Span::mixed_site() =>
                        <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(quote_spanned! { Span::mixed_site() =>
                        #pre
                        #parse_fields
                        #post
                    }),
                    None,
                )
            }
            syn::Fields::Unit => {
                let variant = match target {
                    ParseTarget::Init(variant) => variant,
                    _ => None,
                }
                .into_iter();
                let variant2 = variant.clone();
                let inline = if mode == TokenMode::ParseMetaItem {
                    quote_spanned! { Span::mixed_site() =>
                        <() as #crate_::ParseMetaItem>::parse_meta_item_inline(inputs, _mode)?;
                        #crate_::Result::Ok(Self #(::#variant)*)
                    }
                } else {
                    quote_spanned! { Span::mixed_site() =>
                        for input in inputs {
                            #priv_::parse_helpers::parse_eof_or_trailing_comma(
                                #priv_::Borrow::borrow(input),
                            )?;
                        }
                        #crate_::Result::Ok(Self #(::#variant)*)
                    }
                };
                (
                    quote_spanned! { Span::mixed_site() =>
                        <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_with(
                            input,
                            |input| {
                                #inline_expr
                            },
                        )
                    },
                    Some(inline),
                    Some(quote_spanned! { Span::mixed_site() =>
                        #crate_::Result::Ok(Self #(::#variant2)*)
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
                let mut alias_span = None;
                let mut idents = Vec::new();
                let mut default = None;
                let mut with = None;
                let mut flatten = None;
                let mut append = None;
                let mut rest = None;
                let mut rename = None;
                let mut container = None;
                let mut skip = None;
                parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "flatten" => {
                            if flatten.is_some() {
                                errors.push(span, "duplicate attribute for `flatten`");
                            }
                            flatten = Some(parse_helpers::parse_named_meta_item::<FieldFlatten>(
                                input, span,
                            )?);
                        }
                        "append" => {
                            if !named {
                                errors.push(span, "`append` not allowed on tuple struct field");
                            }
                            if append.is_some() {
                                errors.push(span, "duplicate attribute for `append`");
                            }
                            append = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "rest" => {
                            if !named {
                                errors.push(span, "`rest` not allowed on tuple struct field");
                            }
                            if rest.is_some() {
                                errors.push(span, "duplicate attribute for `rest`");
                            }
                            rest = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "default" => {
                            if default.is_some() {
                                errors.push(span, "duplicate attribute for `default`");
                            }
                            default = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "with" => {
                            if with.is_some() {
                                errors.push(span, "duplicate attribute for `with`");
                            }
                            with = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "rename" => {
                            if !named {
                                errors.push(span, "`rename` not allowed on tuple struct field");
                            }
                            if rename.is_some() {
                                errors.push(span, "duplicate attribute for `rename`");
                            } else {
                                rename = Some(span);
                            }
                            let name = parse_helpers::parse_named_meta_item(input, span)?;
                            if field.ident.as_ref() == Some(&name) {
                                errors.push(span, "cannot rename field to its own name");
                            } else if idents.contains(&name) {
                                errors
                                    .push(span, format_args!("alias already given for `{}`", name));
                            } else {
                                idents.insert(0, name);
                            }
                        }
                        "alias" => {
                            if !named {
                                errors.push(span, "`alias` not allowed on tuple struct field");
                            }
                            let alias = parse_helpers::parse_named_meta_item(input, span)?;
                            if field.ident.as_ref() == Some(&alias) {
                                errors.push(span, "cannot alias field to its own name");
                            } else if idents.contains(&alias) {
                                errors.push(span, format_args!("duplicate alias for `{}`", alias));
                            } else {
                                alias_span = Some(span);
                                idents.push(alias);
                            }
                        }
                        "container" => {
                            if container.is_some() {
                                errors.push(span, "duplicate attribute for `container`");
                            }
                            container = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "skip" => {
                            if skip.is_some() {
                                errors.push(span, "duplicate attribute for `skip`");
                            }
                            skip = Some(parse_helpers::parse_named_meta_item(input, span)?);
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
                        idents.insert(0, ident.clone());
                    }
                }
                errors.check()?;
                Ok(Self {
                    field,
                    idents: idents.into_iter().collect(),
                    default,
                    with,
                    flatten,
                    append,
                    rest,
                    container,
                    skip,
                })
            },
        )
    }
}

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

impl deluxe_core::ParseMetaItem for StructTransparent {
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
                        Some(parse_helpers::parse_named_meta_item(input, span)?);
                }
                "flatten_unnamed" => {
                    if transparent.flatten_unnamed.is_some() {
                        errors.push(span, "duplicate attribute for `flatten_unnamed`");
                    }
                    transparent.flatten_unnamed =
                        Some(parse_helpers::parse_named_meta_item(input, span)?);
                }
                "append" => {
                    if transparent.append.is_some() {
                        errors.push(span, "duplicate attribute for `append`");
                    }
                    transparent.append = Some(parse_helpers::parse_named_meta_item(input, span)?);
                }
                "rest" => {
                    if transparent.rest.is_some() {
                        errors.push(span, "duplicate attribute for `rest`");
                    }
                    transparent.rest = Some(parse_helpers::parse_named_meta_item(input, span)?);
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
        &["transparent", "default", "crate", "attributes"]
    }
    pub fn to_field_names_tokens(&self, crate_: &syn::Path, priv_: &syn::Path) -> TokenStream {
        if self
            .fields
            .iter()
            .any(|f| f.flatten.as_ref().map(|f| f.value).unwrap_or(false))
        {
            let names = self.fields.iter().filter(|f| f.is_parsable()).map(|f| match &f.flatten {
                Some(FieldFlatten {
                    value: true,
                    prefix: Some(prefix),
                    ..
                }) => {
                    let ty = &f.field.ty;
                    let prefix = parse_helpers::path_to_string(prefix);
                    let names = quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                    };
                    quote_spanned! { Span::mixed_site() =>
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
                    let ty = &f.field.ty;
                    let names = quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                    };
                    quote_spanned! { Span::mixed_site() =>
                        vec.extend_from_slice(#names);
                    }
                }
                _ => {
                    let names = f.idents.iter().map(|i| i.to_string());
                    quote_spanned! { Span::mixed_site() =>
                        #(vec.push(#names);)*
                    }
                }
            });
            quote_spanned! { Span::mixed_site() =>
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
            let names = self.fields.iter().filter_map(|f| {
                if !f.is_parsable() {
                    return None;
                }
                let names = f.idents.iter().map(|i| i.to_string());
                Some(quote_spanned! { Span::mixed_site() => #(#names),* })
            });
            quote_spanned! { Span::mixed_site() =>
                &[#(#names),*]
            }
        }
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
        let target = self
            .default
            .as_ref()
            .map(|_| syn::parse_quote_spanned! { Span::mixed_site() => target });
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
            quote_spanned! { Span::mixed_site() =>
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
            let parse_ty = quote_spanned! { ty.span() =>
                <#ty as #crate_::ParseMetaItem>
            };
            pre.extend(default_set);
            if matches!(orig_fields, syn::Fields::Unnamed(_)) {
                pre.extend(quote_spanned! { Span::mixed_site() =>
                    let index = 0usize;
                });
            }
            let parse = quote_spanned! { Span::mixed_site() =>
                #pre
                #name = #priv_::Option::Some(#parse_ty::parse_meta_item(input, _mode)?);
                #post
            };
            let inline = Some(quote_spanned! { Span::mixed_site() =>
                #pre
                #name = #priv_::Option::Some(#parse_ty::parse_meta_item_inline(inputs, _mode)?);
                #post
            });
            let flag = Some(quote_spanned! { Span::mixed_site() =>
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
                let flat_ty = quote_spanned! { ty.span() =>
                    <#ty as #crate_::ParseMetaFlatNamed>
                };
                extra_traits.push(quote_spanned! { Span::mixed_site() =>
                    impl #impl_generics #crate_::ParseMetaFlatNamed for #struct_ident #type_generics #where_clause {
                        const ACCEPTS_ALL: #priv_::bool = #flat_ty::ACCEPTS_ALL;
                        #[inline]
                        fn field_names() -> &'static [&'static #priv_::str] {
                            #flat_ty::field_names()
                        }
                        #[inline]
                        fn parse_meta_flat_named<'s, S: #priv_::Borrow<#priv_::ParseBuffer<'s>>>(
                            inputs: &[S],
                            prefix: &#priv_::str,
                            validate: #priv_::bool,
                        ) -> #crate_::Result<Self> {
                            #pre
                            #name = #priv_::Option::Some(#flat_ty::parse_meta_flat_named(inputs, prefix, validate)?);
                            #post
                        }
                    }
                });
            }
            if flatten_unnamed {
                let flat_ty = quote_spanned! { ty.span() =>
                    <#ty as #crate_::ParseMetaFlatUnnamed>
                };
                extra_traits.push(quote_spanned! { Span::mixed_site() =>
                    impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn field_count() -> #priv_::Option<#priv_::usize> {
                            #flat_ty::field_count()
                        }
                        #[inline]
                        fn parse_meta_flat_unnamed<'s, S: #priv_::Borrow<#priv_::ParseBuffer<'s>>>(
                            inputs: &[S],
                            index: #priv_::usize,
                        ) -> #crate_::Result<Self> {
                            #pre
                            #name = #priv_::Option::Some(#flat_ty::parse_meta_flat_unnamed(inputs, index)?);
                            #post
                        }
                    }
                });
            }
            if rest {
                let rest_ty = quote_spanned! { ty.span() =>
                    <#ty as #crate_::ParseMetaRest>
                };
                extra_traits.push(quote_spanned! { Span::mixed_site() =>
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
                let append_ty = quote_spanned! { ty.span() =>
                    <#ty as #crate_::ParseMetaAppend>
                };
                extra_traits.push(quote_spanned! { Span::mixed_site() =>
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
                quote_spanned! { Span::mixed_site() =>
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
                quote_spanned! { Span::mixed_site() =>
                    #default_set
                    #inline
                }
            });
            def
        }
    }
}

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
                            transparent = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "allow_unknown_fields" => {
                            if allow_unknown_fields.is_some() {
                                errors.push(span, "duplicate attribute for `allow_unknown_fields`");
                            }
                            allow_unknown_fields =
                                Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "default" => {
                            if default.is_some() {
                                errors.push(span, "duplicate attribute for `default`");
                            }
                            if matches!(struct_.fields, syn::Fields::Unit) {
                                errors.push(span, "`default` not allowed on unit struct");
                            }
                            default = Some(parse_helpers::parse_named_meta_item(input, span)?);
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

pub struct Variant<'v> {
    pub variant: &'v syn::Variant,
    pub fields: Vec<Field<'v>>,
    pub idents: Vec<syn::Ident>,
    pub flatten: Option<bool>,
    pub transparent: Option<bool>,
    pub allow_unknown_fields: Option<bool>,
}

impl<'v> Variant<'v> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["rename", "flatten", "alias"]
    }
    fn field_key(&self) -> BTreeSet<BTreeSet<String>> {
        self.fields
            .iter()
            .map(|f| {
                f.idents
                    .iter()
                    .filter_map(|i| {
                        if f.default.is_some() || f.is_flat() || !f.is_parsable() {
                            return None;
                        }
                        Some(i.to_string())
                    })
                    .collect()
            })
            .collect()
    }
    fn to_field_parsing_tokens(
        &self,
        crate_: &syn::Path,
        priv_: &syn::Path,
        mode: TokenMode,
        flat: bool,
    ) -> TokenStream {
        let field_data = FieldData {
            mode: if flat { mode } else { TokenMode::ParseMetaItem },
            target: ParseTarget::Init(Some(&self.variant.ident)),
            inline_expr: &syn::parse_quote_spanned! { Span::mixed_site() => inline(input, _mode) },
            allowed_expr: &syn::parse_quote_spanned! { Span::mixed_site() => &[] },
            transparent: self.transparent.unwrap_or(false),
            allow_unknown_fields: self.allow_unknown_fields.unwrap_or(false),
        };
        let (pre, post) =
            Field::to_pre_post_tokens(&self.fields, &self.variant.fields, crate_, &field_data);
        let ItemDef {
            parse,
            inline,
            flag,
            ..
        } = Field::to_parsing_tokens(
            &self.fields,
            &self.variant.fields,
            crate_,
            (&pre, &post),
            field_data,
        );
        let pre = match &self.variant.fields {
            syn::Fields::Named(_) => Some(quote_spanned! { Span::mixed_site() =>
                let prefix = "";
                let validate = false;
            }),
            syn::Fields::Unnamed(_) => Some(quote_spanned! { Span::mixed_site() =>
                let mut index = 0;
            }),
            _ => None,
        };
        if flat {
            quote_spanned! { Span::mixed_site() =>
                #pre
                let ret = (|| {
                    #inline
                })();
                match ret {
                    #crate_::Result::Ok(v) => {
                        value = #priv_::Option::Some(v);
                    }
                    #crate_::Result::Err(err) => {
                        errors.push_syn(err);
                    }
                }
            }
        } else {
            quote_spanned! { Span::mixed_site() =>
                #pre
                let inline = |input, _mode| {
                    let inputs = [input];
                    let inputs = inputs.as_slice();
                    #inline
                };
                match #priv_::parse_helpers::parse_named_meta_item_with(
                    input,
                    span,
                    |input, _mode| {
                        #parse
                    },
                    inline,
                    |span| {
                        #flag
                    },
                ) {
                    #crate_::Result::Ok(v) => {
                        value = #priv_::Option::Some(v);
                    }
                    #crate_::Result::Err(err) => {
                        errors.push_syn(err);
                    }
                }
            }
        }
    }
    fn to_parsing_tokens(
        variants: &[Self],
        crate_: &syn::Path,
        mode: TokenMode,
        target: Option<&syn::Expr>,
        allow_unknown_fields: bool,
    ) -> TokenStream {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let variant_matches = variants.iter().filter_map(|v| {
            if v.flatten.unwrap_or(false) {
                return None;
            }
            let idents = v.idents.iter().map(|i| i.to_string());
            let parse = v.to_field_parsing_tokens(crate_, priv_, mode, false);
            Some(quote_spanned! { Span::mixed_site() =>
                #(#priv_::Option::Some(k @ #idents))|* => {
                    if let #priv_::Option::Some(key) = key {
                        #priv_::parse_helpers::only_one_variant(
                            span,
                            prefix,
                            (key, k),
                            &errors
                        );
                    } else {
                        key = #priv_::Option::Some(k);
                        #parse
                    }
                },
            })
        });
        let any_flat = variants.iter().any(|v| v.flatten.unwrap_or(false));
        let paths_ident = any_flat.then(|| syn::Ident::new("paths", Span::mixed_site()));
        let paths_ident = paths_ident.as_ref().into_iter();
        let paths_ident2 = paths_ident.clone();
        let inputs_expr = any_flat
            .then(|| {
                quote_spanned! { Span::mixed_site() =>
                    &#priv_::parse_helpers::fork_inputs(inputs)
                }
            })
            .unwrap_or_else(|| {
                quote_spanned! { Span::mixed_site() =>
                    inputs
                }
            });
        let mut flat_matches = variants
            .iter()
            .filter_map(|v| {
                if !v.flatten.unwrap_or(false) {
                    return None;
                }
                let key = v.field_key();
                let paths_key = key.iter().map(|idents| {
                    quote_spanned! { Span::mixed_site() => &[#(#idents),*] }
                });
                let cond = (!key.is_empty()).then(|| {
                    quote_spanned! { Span::mixed_site() =>
                        if #priv_::parse_helpers::has_paths(&paths, &[#(#paths_key),*])
                    }
                });
                let parse = v.to_field_parsing_tokens(crate_, priv_, mode, true);
                Some((
                    key,
                    quote_spanned! { Span::mixed_site() =>
                        #cond {
                            #parse
                        }
                    },
                ))
            })
            .collect::<BTreeMap<_, _>>();
        let empty_match = flat_matches.remove(&BTreeSet::new()).unwrap_or_else(|| {
            if let Some(target) = target {
                return quote_spanned! { Span::mixed_site() =>
                    {
                        value = #priv_::Option::Some(#target);
                    }
                };
            }
            let variant_keys = variants.iter().filter_map(|v| {
                if v.flatten.unwrap_or(false) {
                    return None;
                }
                v.idents.first().map(|i| i.to_string())
            });
            let flat_keys = flat_matches.keys().map(|key| {
                let key_exprs = key.iter().map(|idents| {
                    quote_spanned! { Span::mixed_site() => &[#(#idents),*] }
                });
                quote_spanned! { Span::mixed_site() => &[#(#key_exprs),*] }
            });
            quote_spanned! { Span::mixed_site() =>
                {
                    #priv_::parse_helpers::variant_required(
                        span,
                        prefix,
                        &[#(&[&[#variant_keys]],)* #(#flat_keys,)*],
                        &errors
                    );
                }
            }
        });
        let flat_matches = flat_matches.values();
        let validate = (!allow_unknown_fields).then(|| {
            quote_spanned! { Span::mixed_site() =>
                if validate {
                    #priv_::parse_helpers::check_unknown_attribute(p, span, allowed, &errors);
                }
            }
        });
        quote_spanned! { Span::mixed_site() =>
            let mut key = #priv_::Option::None;
            let mut value = #priv_::Option::None;
            let errors = #crate_::Errors::new();
            #(let #paths_ident = #priv_::HashSet::new();)*
            #priv_::parse_helpers::parse_struct(#inputs_expr, |input, p, span| {
                let inputs = [input];
                let inputs = inputs.as_slice();
                let cur = p.strip_prefix(prefix);
                #(if let #priv_::Option::Some(cur) = cur {
                    #paths_ident2.insert(cur);
                })*
                match cur {
                    #(#variant_matches)*
                    _ => {
                        #validate
                        #priv_::parse_helpers::skip_named_meta_item(input);
                    }
                }
                #crate_::Result::Ok(())
            })?;
            if value.is_none() {
                #(#flat_matches else)*
                #empty_match
            }
            errors.check()?;
            #crate_::Result::Ok(value.unwrap())
        }
    }
}

impl<'v> ParseAttributes<'v, syn::Variant> for Variant<'v> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    #[inline]
    fn parse_attributes(variant: &'v syn::Variant) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(
            parse_helpers::ref_tokens::<Self, _>(variant),
            |inputs, _| {
                let errors = crate::Errors::new();
                let mut alias_span = None;
                let mut idents = Vec::new();
                let mut flatten = None;
                let mut rename = None;
                let mut transparent = None;
                let mut allow_unknown_fields = None;
                let fields = variant
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
                                        "`transparent` variant must have only one parseable field",
                                    );
                                } else if first.flatten.as_ref().map(|f| f.value).unwrap_or(false) {
                                    errors
                                        .push(span, "`transparent` variant field cannot be `flat`");
                                } else if first.append.map(|v| *v).unwrap_or(false) {
                                    errors.push(
                                        span,
                                        "`transparent` variant field cannot be `append`",
                                    );
                                }
                            }
                            transparent = Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "allow_unknown_fields" => {
                            if allow_unknown_fields.is_some() {
                                errors.push(span, "duplicate attribute for `allow_unknown_fields`");
                            }
                            allow_unknown_fields =
                                Some(parse_helpers::parse_named_meta_item(input, span)?);
                        }
                        "flatten" => {
                            if flatten.is_some() {
                                errors.push(span, "duplicate attribute for `flatten`");
                            }
                            flatten = Some(parse_helpers::parse_named_meta_item::<syn::LitBool>(
                                input, span,
                            )?);
                        }
                        "rename" => {
                            if rename.is_some() {
                                errors.push(span, "duplicate attribute for `rename`");
                            } else {
                                rename = Some(span);
                            }
                            let name = parse_helpers::parse_named_meta_item(input, span)?;
                            if variant.ident == name {
                                errors.push(span, "cannot rename field to its own name");
                            } else if idents.contains(&name) {
                                errors
                                    .push(span, format_args!("alias already given for `{}`", name));
                            } else {
                                idents.insert(0, name);
                            }
                        }
                        "alias" => {
                            let alias = parse_helpers::parse_named_meta_item(input, span)?;
                            if variant.ident == alias {
                                errors.push(span, "cannot alias field to its own name");
                            } else if idents.contains(&alias) {
                                errors.push(span, format_args!("duplicate alias for `{}`", alias));
                            } else {
                                alias_span = Some(span);
                                idents.push(alias);
                            }
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
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                let flatten = flatten.map(|f| f.value());
                if rename.is_none() && !flatten.unwrap_or(false) {
                    let ident =
                        heck::ToSnakeCase::to_snake_case(variant.ident.to_string().as_str());
                    idents.insert(0, syn::Ident::new(&ident, variant.ident.span()));
                }
                let fields = {
                    let mut fields = fields;
                    for field in &mut fields {
                        if let Some(span) =
                            field.skip.and_then(|skip| (*skip).then_some(skip.span()))
                        {
                            if field.default.is_none() {
                                field.default = Some(FieldDefault::Default(span));
                            }
                        }
                    }
                    fields
                };
                let mut container = None;
                for field in &fields {
                    if let Some(c) = field.container.as_ref() {
                        if container.is_some() {
                            errors.push(c.span(), "Duplicate `container` field")
                        } else {
                            container = Some(c);
                        }
                    }
                }
                errors.check()?;
                Ok(Self {
                    variant,
                    fields,
                    idents: idents.into_iter().collect(),
                    flatten,
                    transparent,
                    allow_unknown_fields,
                })
            },
        )
    }
}

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
            d.to_expr(
                &syn::parse_quote_spanned! { Span::mixed_site() => Self },
                &priv_path,
            )
        });
        Variant::to_parsing_tokens(
            &self.variants,
            crate_,
            mode,
            default.as_ref().map(|d| d.as_ref()),
            self.allow_unknown_fields.unwrap_or(false),
        )
    }
    pub fn to_field_names_tokens(&self, crate_: &syn::Path, priv_: &syn::Path) -> TokenStream {
        let any_flat_nested = self.variants.iter().any(|v| {
            v.fields
                .iter()
                .any(|f| v.flatten.unwrap_or(false) && (f.is_flat()))
        });
        let field_names = self.variants.iter().flat_map(|v| {
            v.idents
                .iter()
                .map(|ident| {
                    let ident = ident.to_string();
                    quote_spanned! { Span::mixed_site() => #ident }
                })
                .chain(v.fields.iter().flat_map(|field| {
                    field.idents.iter().filter_map(|ident| {
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
                                Some(quote_spanned! { Span::mixed_site() =>
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
                                Some(quote_spanned! { Span::mixed_site() =>
                                    vec.extend_from_slice(#names);
                                })
                            },
                            _ => {
                                let ident = ident.to_string();
                                if any_flat_nested {
                                    Some(quote_spanned! { Span::mixed_site() => vec.push(#ident); })
                                } else {
                                    Some(quote_spanned! { Span::mixed_site() => #ident })
                                }
                            }
                        }
                    })
                }))
        });
        if any_flat_nested {
            quote_spanned! { Span::mixed_site() =>
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
            quote_spanned! { Span::mixed_site() =>
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
                                &variant.variant,
                                "additional flattened variants must have at least one unique non-flattened, non-default field",
                            );
                            } else {
                                variant_keys.insert(key);
                            }
                        } else {
                            errors.push_spanned(
                                &variant.variant,
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

use deluxe_core::{parse_helpers, ParseAttributes, ParseMode, Result, SpannedValue};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use std::collections::HashSet;
use syn::{parse::ParseStream, spanned::Spanned};

pub enum FieldDefault {
    Default(Span),
    Expr(syn::Expr),
}

impl deluxe_core::ParseMetaItem for FieldDefault {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
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
    fn parse_meta_item(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            Ok(Self {
                span: input.span(),
                value: input.parse::<syn::LitBool>()?.value(),
                prefix: None,
            })
        } else if lookahead.peek(syn::token::Brace) {
            <parse_helpers::Brace as parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                input,
                ParseMode::Named,
            )
        } else {
            Err(lookahead.error())
        }
    }
    fn parse_meta_item_inline(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        let mut flatten = Self {
            span: input.span(),
            value: true,
            prefix: None,
        };
        if input.peek(syn::LitBool) {
            flatten.value = input.parse::<syn::LitBool>()?.value();
            return Ok(flatten);
        }
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(&[input], |input, path, span| {
            match path {
                "prefix" => {
                    if flatten.prefix.is_some() {
                        errors.push_spanned(&path, "duplicate attribute for `prefix`");
                    }
                    flatten.prefix = Some(deluxe_core::parse_named_meta_item_with!(
                        input,
                        deluxe_core::mod_path
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
    fn parse_meta_item(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            Ok(Self {
                span: input.span(),
                value: input.parse::<syn::LitBool>()?.value(),
                lifetime: None,
                ty: None,
            })
        } else if lookahead.peek(syn::token::Brace) {
            <parse_helpers::Brace as parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                input,
                ParseMode::Named,
            )
        } else {
            Err(lookahead.error())
        }
    }
    fn parse_meta_item_inline(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        let mut container = Self {
            span: input.span(),
            value: true,
            lifetime: None,
            ty: None,
        };
        if input.peek(syn::LitBool) {
            container.value = input.parse::<syn::LitBool>()?.value();
            return Ok(container);
        }
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(&[input], |input, path, span| {
            match path {
                "lifetime" => {
                    if container.lifetime.is_some() {
                        errors.push_spanned(&path, "duplicate attribute for `lifetime`");
                    }
                    container.lifetime = Some(parse_helpers::parse_named_meta_item(input)?);
                }
                "type" => {
                    if container.ty.is_some() {
                        errors.push_spanned(&path, "duplicate attribute for `type`");
                    }
                    container.ty = Some(parse_helpers::parse_named_meta_item(input)?);
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
    pub container: Option<FieldContainer>,
    pub skip: Option<SpannedValue<bool>>,
}

pub struct ItemDef {
    pub parse: TokenStream,
    pub inline: Option<TokenStream>,
    pub flag: Option<TokenStream>,
}

#[derive(PartialEq, Clone, Copy)]
pub enum TokenMode {
    ParseMetaItem,
    ParseAttributes,
    ExtractAttributes,
}

impl<'f> Field<'f> {
    #[inline]
    pub fn is_flat(&self) -> bool {
        self.flatten.as_ref().map(|f| f.value).unwrap_or(false)
    }
    #[inline]
    pub fn is_container(&self) -> bool {
        self.container.as_ref().map(|f| f.value).unwrap_or(false)
    }
    #[inline]
    pub fn is_parseable(&self) -> bool {
        !self.is_container() && !self.skip.map(|v| *v).unwrap_or(false)
    }
    pub fn to_parsing_tokens(
        fields: &[Self],
        orig: &syn::Fields,
        crate_: &syn::Path,
        mode: TokenMode,
    ) -> ItemDef {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let is_unnamed = matches!(orig, syn::Fields::Unnamed(_));
        let pub_fields = fields.iter().filter(|f| f.is_parseable());
        let any_flat = pub_fields.clone().any(|f| f.is_flat());
        let names = fields
            .iter()
            .enumerate()
            .map(|(i, _)| quote::format_ident!("field{}", i, span = Span::mixed_site()))
            .collect::<Vec<_>>();
        let container_def = fields.iter().enumerate().filter_map(|(i, f)| {
            (f.is_container() && mode != TokenMode::ParseMetaItem).then(|| {
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
                        } else {
                            let error = format!(
                                "missing required field `{}`",
                                f.field.ident.as_ref().unwrap()
                            );
                            quote_spanned! { Span::mixed_site() =>
                                if #name.is_none() {
                                    errors.push_call_site(#error);
                                }
                            }
                        }
                    });
                    if is_unnamed {
                        let ty = &f.field.ty;
                        if f.is_flat() {
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
        let field_unwraps = fields.iter().enumerate().map(|(i, f)| {
            let name = &names[i];
            let ty = &f.field.ty;
            match &f.default {
                Some(FieldDefault::Default(_)) => quote_spanned! { Span::mixed_site() =>
                    let #name = #name.unwrap_or_else(|| <#ty as #priv_::Default>::default());
                },
                Some(FieldDefault::Expr(expr)) => quote_spanned! { Span::mixed_site() =>
                    let #name = #name.unwrap_or_else(|| #expr);
                },
                None => quote_spanned! { Span::mixed_site() =>
                    let #name = #name.unwrap();
                },
            }
        });

        let (parse, inline, flag) = match orig {
            syn::Fields::Named(_) => {
                let field_matches = fields.iter().enumerate().flat_map(|(i, f)| {
                    let name = names[i].clone();
                    let error = format!("duplicate attribute for `{}`", f.idents.first().unwrap());
                    f.idents.iter().filter_map(move |ident| {
                        if f.is_flat() || !f.is_parseable() {
                            return None;
                        }
                        let call = match f.with.as_ref() {
                            Some(m) => quote_spanned! { Span::mixed_site() =>
                                #crate_::parse_named_meta_item_with!(input, #m)
                            },
                            None => quote_spanned! { Span::mixed_site() =>
                                #priv_::parse_helpers::parse_named_meta_item(input)?
                            },
                        };
                        let ident = ident.to_string();
                        Some(quote_spanned! { Span::mixed_site() =>
                            #priv_::Option::Some(#ident) => {
                                if #name.is_some() {
                                    errors.push_spanned(&p, #error);
                                }
                                #name = #priv_::Option::Some(#call);
                            }
                        })
                    })
                });
                let flat_fields = fields.iter().enumerate().filter_map(|(i, f)| {
                    if !f.is_flat() || !f.is_parseable()  {
                        return None;
                    }
                    f.flatten.as_ref().map(|flatten| {
                        let name = &names[i];
                        let ty = &f.field.ty;
                        let call = match &flatten.prefix {
                            Some(prefix) => {
                                let prefix = deluxe_core::parse_helpers::path_to_string(prefix);
                                quote_spanned! { Span::mixed_site() =>
                                    <#ty as #crate_::ParseMetaFlatPrefixed>::parse_meta_flat_prefixed(
                                        inputs,
                                        #priv_::parse_helpers::join_path(prefix, #prefix),
                                        #priv_::Option::None,
                                    )
                                }
                            }
                            None => quote_spanned! { Span::mixed_site() =>
                                (if <#ty as #crate_::ParseMetaFlatNamed>::requires_path() {
                                    <#ty as #crate_::ParseMetaFlatNamed>::parse_meta_flat_for_path(
                                        inputs,
                                        #priv_::parse_helpers::join_path(prefix, #name),
                                        #priv_::Option::None,
                                    )
                                } else {
                                    <#ty as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                                        inputs,
                                        #priv_::Option::None,
                                    )
                                })
                            },
                        };
                        Some(quote_spanned! { Span::mixed_site() =>
                            match #call {
                                #crate_::Result::Ok(val) => #name = #priv_::Option::Some(val),
                                #crate_::Result::Err(err) => errors.push_syn(err),
                            }
                        })
                    })
                });
                let field_defs = fields.iter().enumerate().map(|(i, f)| {
                    let ident = f.field.ident.as_ref().unwrap();
                    let name = &names[i];
                    quote_spanned! { Span::mixed_site() => #ident: #name }
                });
                (
                    quote_spanned! { Span::mixed_site() =>
                        #(let mut #names = #priv_::Option::None;)*
                        let errors = #crate_::Errors::new();
                        #priv_::parse_helpers::parse_struct(inputs, |input, p, span| {
                            match p.strip_prefix(prefix) {
                                #(#field_matches)*
                                _ => {
                                    #priv_::parse_helpers::check_unknown_attribute(p, span, allowed, &errors);
                                    #priv_::parse_helpers::skip_named_meta_item(input);
                                }
                            }
                            #crate_::Result::Ok(())
                        })?;
                        #(#flat_fields)*
                        #(#container_def)*
                        #(#field_errors)*
                        errors.check()?;
                        #(#field_unwraps)*
                        #crate_::Result::Ok(Self {
                            #(#field_defs),*
                        })
                    },
                    Some(quote_spanned! { Span::mixed_site() =>
                        <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                            &[input],
                            #priv_::Option::Some(<Self as #crate_::ParseMetaFlatNamed>::field_names()),
                        )
                    }),
                    Some(quote_spanned! { Span::mixed_site() =>
                        #priv_::parse_helpers::parse_empty_meta_item(span, #crate_::ParseMode::Named)
                    }),
                )
            }
            syn::Fields::Unnamed(_) => {
                let field_count = pub_fields.clone().filter(|f| !f.is_flat()).count();
                let field_matches = fields.iter().enumerate().filter_map(|(index, f)| {
                    if !f.is_parseable() {
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
                        (true, None) => quote_spanned! { Span::mixed_site() =>
                            <#ty as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(inputs, index)
                        },
                        (false, None) => quote_spanned! { Span::mixed_site() =>
                            <#ty as #crate_::ParseMetaItem>::parse_meta_item(input, #crate_::ParseMode::Unnamed)
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
                (
                    quote_spanned! { Span::mixed_site() =>
                        #(let mut #names = #priv_::Option::None;)*
                        #priv_::parse_helpers::parse_tuple_struct(inputs, #field_count, |input, inputs, i| {
                            match i {
                                #(#field_matches)*
                                _ => #priv_::unreachable!(),
                            }
                            #crate_::Result::Ok(())
                        })?;
                        #(#container_def)*
                        let errors = #crate_::Errors::new();
                        #(#field_errors)*
                        errors.check()?;
                        #(#field_unwraps)*
                        #crate_::Result::Ok(Self(#(#names),*))
                    },
                    Some(quote_spanned! { Span::mixed_site() =>
                        <Self as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(&[input], 0)
                    }),
                    None,
                )
            }
            syn::Fields::Unit => (
                quote_spanned! { Span::mixed_site() =>
                    <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                        input, #crate_::ParseMode::Unnamed
                    )
                },
                Some(quote_spanned! { Span::mixed_site() =>
                    <() as #crate_::ParseMetaItem>::parse_meta_item_inline(input, _mode)?;
                    #crate_::Result::Ok(Self)
                }),
                Some(quote_spanned! { Span::mixed_site() =>
                    #crate_::Result::Ok(Self)
                }),
            ),
        };
        ItemDef {
            parse,
            inline,
            flag,
        }
    }
    pub fn field_names(field: &syn::Field) -> &'static [&'static str] {
        if field.ident.is_some() {
            &[
                "rename",
                "flatten",
                "default",
                "alias",
                "with",
                "container",
                "skip",
            ]
        } else {
            &["flatten", "default", "with", "container", "skip"]
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
            |inputs| {
                let named = field.ident.is_some();
                let errors = crate::Errors::new();
                let mut alias_span = None;
                let mut idents = Vec::new();
                let mut default = None;
                let mut with = None;
                let mut flatten = None;
                let mut rename = None;
                let mut container = None;
                let mut skip = None;
                parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "flatten" => {
                            if flatten.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `flatten`");
                            }
                            flatten = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        "default" => {
                            if default.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `default`");
                            }
                            default = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        "with" => {
                            if with.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `with`");
                            }
                            with = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        "rename" if named => {
                            if rename.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `rename`");
                            } else {
                                rename = Some(span);
                            }
                            let name = parse_helpers::parse_named_meta_item(input)?;
                            if field.ident.as_ref() == Some(&name) {
                                errors.push_spanned(&path, "cannot rename field to its own name");
                            } else if idents.contains(&name) {
                                errors.push_spanned(
                                    &path,
                                    format_args!("alias already given for `{}`", name),
                                );
                            } else {
                                idents.insert(0, name);
                            }
                        }
                        "alias" if named => {
                            let alias = parse_helpers::parse_named_meta_item(input)?;
                            if field.ident.as_ref() == Some(&alias) {
                                errors.push_spanned(&path, "cannot alias field to its own name");
                            } else if idents.contains(&alias) {
                                errors.push_spanned(
                                    &path,
                                    format_args!("duplicate alias for `{}`", alias),
                                );
                            } else {
                                alias_span = Some(span);
                                idents.push(alias);
                            }
                        }
                        "container" => {
                            if container.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `container`");
                            }
                            container = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        "skip" => {
                            if skip.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `skip`");
                            }
                            skip = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        _ => {
                            parse_helpers::check_unknown_attribute(
                                path,
                                span,
                                Some(Self::field_names(field)),
                                &errors,
                            );
                            parse_helpers::skip_named_meta_item(input);
                        }
                    }
                    Ok(())
                })?;
                deluxe_core::only_one!("", &errors, default, flatten);
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, container, flatten);
                deluxe_core::only_one!("", &errors, container, rename);
                deluxe_core::only_one!("", &errors, container, with);
                deluxe_core::only_one!("", &errors, container, ("alias", alias_span.as_ref()));
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                if rename.is_none() {
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
                    container,
                    skip,
                })
            },
        )
    }
}

pub struct Struct<'s> {
    pub struct_: &'s syn::DataStruct,
    pub fields: Vec<Field<'s>>,
    pub default: Option<FieldDefault>,
    pub crate_: Option<syn::Path>,
    pub attributes: Vec<syn::Path>,
}

impl<'s> Struct<'s> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["default", "crate", "attributes"]
    }
}

impl<'s> ParseAttributes<'s, syn::DeriveInput> for Struct<'s> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(i: &'s syn::DeriveInput) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(parse_helpers::ref_tokens::<Self, _>(i), |inputs| {
            let struct_ = match &i.data {
                syn::Data::Struct(s) => s,
                _ => return Err(syn::Error::new_spanned(i, "wrong DeriveInput type")),
            };
            let errors = crate::Errors::new();
            let mut default = None;
            let mut crate_ = None;
            let mut attributes = Vec::new();
            parse_helpers::parse_struct(inputs, |input, path, span| {
                match path {
                    "default" => {
                        if default.is_some() {
                            errors.push_spanned(&path, "duplicate attribute for `default`");
                        }
                        default = Some(parse_helpers::parse_named_meta_item(input)?);
                    }
                    "crate" => {
                        if crate_.is_some() {
                            errors.push_spanned(&path, "duplicate attribute for `crate`");
                        }
                        crate_ = Some(parse_helpers::parse_named_meta_item(input)?);
                    }
                    "attributes" => {
                        let attrs = deluxe_core::parse_named_meta_item_with!(
                            input,
                            deluxe_core::mod_path_vec
                        )?;
                        attributes.extend(attrs.into_iter());
                    }
                    _ => {
                        parse_helpers::check_unknown_attribute(
                            path,
                            span,
                            Some(Self::field_names()),
                            &errors,
                        );
                        parse_helpers::skip_named_meta_item(input);
                    }
                }
                Ok(())
            })?;
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
                attributes,
            })
        })
    }
}

pub struct Variant<'v> {
    pub variant: &'v syn::Variant,
    pub fields: Vec<Field<'v>>,
    pub idents: Vec<syn::Ident>,
    pub flatten: Option<bool>,
}

impl<'v> Variant<'v> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["rename", "flatten", "alias"]
    }
    pub fn to_parsing_tokens(
        variants: &[Self],
        crate_: &syn::Path,
        mode: TokenMode,
    ) -> TokenStream {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let variant_matches = variants.iter().flat_map(|v| {
            v.idents.iter().filter_map(|ident| {
                if v.flatten.unwrap_or(false) {
                    return None;
                }
                let ident = ident.to_string();
                let field_count = v.variant.fields.len();
                let ItemDef {
                    parse,
                    inline,
                    flag,
                } = Field::to_parsing_tokens(&v.fields, &v.variant.fields, crate_, mode);
                let pre = match &v.variant.fields {
                    syn::Fields::Named(_) if field_count > 0 => {
                        Some(quote_spanned! { Span::mixed_site() =>
                            let prefix = "";
                            let allowed = None;
                        })
                    }
                    syn::Fields::Unnamed(_) if field_count > 1 => {
                        Some(quote_spanned! { Span::mixed_site() =>
                            let mut index = 0;
                        })
                    }
                    _ => None,
                };
                Some(quote_spanned! { Span::mixed_site() =>
                    #priv_::Option::Some(#ident) => {
                        if let #priv_::Option::Some(key) = key {
                            #priv_::parse_helpers::only_one_variant(
                                span,
                                prefix,
                                &[key, #ident],
                                &errors
                            );
                        } else {
                            key = #priv_::Option::Some(#ident);
                            #pre
                            match #priv_::parse_helpers::parse_named_meta_item_with(
                                input,
                                |input, _mode| {
                                    #parse
                                },
                                |input, _mode| {
                                    #inline
                                }
                                |span| {
                                    #flag
                                }
                            ) {
                                #crate_::Result::Ok(v) => {
                                    value = #priv_::Option::Some(v);
                                }
                                #crate_::Result::Err(err) => {
                                    errors.push_syn(err);
                                }
                            }
                        }
                    },
                })
            })
        });
        quote_spanned! { Span::mixed_site() =>
            let mut key = #priv_::Option::None;
            let mut value = #priv_::Option::None;
            let errors = #crate_::Errors::new();
            #priv_::parse_helpers::parse_struct(inputs, |input, p, span| {
                let inputs = [input];
                let inputs = inputs.as_slice();
                match p.strip_prefix(prefix) {
                    #(#variant_matches)*
                    _ => {
                        #priv_::parse_helpers::check_unknown_attribute(p, span, allowed, &errors);
                        #priv_::parse_helpers::skip_named_meta_item(input);
                    }
                }
                #crate_::Result::Ok(())
            })?;
            if value.is_none() {
                // TODO - flattening
                errors.push_call_site("TODO");
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
            |inputs| {
                let errors = crate::Errors::new();
                let mut alias_span = None;
                let mut idents = Vec::new();
                let mut flatten: Option<syn::LitBool> = None;
                let mut rename = None;
                parse_helpers::parse_struct(inputs, |input, path, span| {
                    match path {
                        "flatten" => {
                            if flatten.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `flatten`");
                            }
                            flatten = Some(parse_helpers::parse_named_meta_item(input)?);
                        }
                        "rename" => {
                            if rename.is_some() {
                                errors.push_spanned(&path, "duplicate attribute for `rename`");
                            } else {
                                rename = Some(span);
                            }
                            let name = parse_helpers::parse_named_meta_item(input)?;
                            if variant.ident == name {
                                errors.push_spanned(&path, "cannot rename field to its own name");
                            } else if idents.contains(&name) {
                                errors.push_spanned(
                                    &path,
                                    format_args!("alias already given for `{}`", name),
                                );
                            } else {
                                idents.insert(0, name);
                            }
                        }
                        "alias" => {
                            let alias = parse_helpers::parse_named_meta_item(input)?;
                            if variant.ident == alias {
                                errors.push_spanned(&path, "cannot alias field to its own name");
                            } else if idents.contains(&alias) {
                                errors.push_spanned(
                                    &path,
                                    format_args!("duplicate alias for `{}`", alias),
                                );
                            } else {
                                alias_span = Some(span);
                                idents.push(alias);
                            }
                        }
                        _ => {
                            parse_helpers::check_unknown_attribute(
                                path,
                                span,
                                Some(Self::field_names()),
                                &errors,
                            );
                            parse_helpers::skip_named_meta_item(input);
                        }
                    }
                    Ok(())
                })?;
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                if rename.is_none() {
                    idents.insert(0, variant.ident.clone());
                }
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
                    flatten: flatten.map(|f| f.value()),
                })
            },
        )
    }
}

pub struct Enum<'e> {
    pub enum_: &'e syn::DataEnum,
    pub variants: Vec<Variant<'e>>,
    pub crate_: Option<syn::Path>,
    pub attributes: Vec<syn::Path>,
}

impl<'e> Enum<'e> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &["crate", "attributes"]
    }
}

impl<'e> ParseAttributes<'e, syn::DeriveInput> for Enum<'e> {
    #[inline]
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(i: &'e syn::DeriveInput) -> Result<Self> {
        parse_helpers::parse_struct_attr_tokens(parse_helpers::ref_tokens::<Self, _>(i), |inputs| {
            let enum_ = match &i.data {
                syn::Data::Enum(e) => e,
                _ => return Err(syn::Error::new_spanned(i, "wrong DeriveInput type")),
            };
            let errors = crate::Errors::new();
            let mut crate_ = None;
            let mut attributes = Vec::new();
            parse_helpers::parse_struct(inputs, |input, path, span| {
                match path {
                    "crate" => {
                        if crate_.is_some() {
                            errors.push_spanned(&path, "duplicate attribute for `crate`");
                        }
                        crate_ = Some(parse_helpers::parse_named_meta_item(input)?);
                    }
                    "attributes" => {
                        let attrs = deluxe_core::parse_named_meta_item_with!(
                            input,
                            deluxe_core::mod_path_vec
                        )?;
                        attributes.extend(attrs.into_iter());
                    }
                    _ => {
                        parse_helpers::check_unknown_attribute(
                            path,
                            span,
                            Some(Self::field_names()),
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
                .filter_map(
                    |v| match <Variant as ParseAttributes<syn::Variant>>::parse_attributes(v) {
                        Ok(v) => Some(v),
                        Err(err) => {
                            errors.push_syn(err);
                            None
                        }
                    },
                )
                .collect::<Vec<_>>();
            // TODO - check flatten on only named variant
            // TODO - verify only one flatten
            let mut all_idents = HashSet::new();
            let mut container = None;
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
                                "only the first `container` field can contain a `type` parameter"
                            );
                        }
                    } else {
                        container = Some(c);
                    }
                }
                for ident in &variant.idents {
                    if all_idents.contains(ident) {
                        errors.push_spanned(
                            ident,
                            format_args!("duplicate variant name for `{}`", ident),
                        );
                    } else {
                        all_idents.insert(ident.clone());
                    }
                }
            }
            errors.check()?;
            Ok(Self {
                enum_,
                variants,
                crate_,
                attributes,
            })
        })
    }
}

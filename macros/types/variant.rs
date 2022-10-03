use super::*;
use deluxe_core::{parse_helpers, ParseAttributes, Result, SpannedValue};
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use std::collections::{BTreeMap, BTreeSet};
use syn::spanned::Spanned;

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
        &[
            "rename",
            "flatten",
            "alias",
            "transparent",
            "allow_unknown_fields",
        ]
    }
    pub(super) fn field_key(&self) -> BTreeSet<BTreeSet<String>> {
        self.fields
            .iter()
            .filter_map(|f| {
                let idents = f
                    .idents
                    .iter()
                    .filter_map(|i| {
                        if f.default.is_some() || f.is_flat() || !f.is_parsable() {
                            return None;
                        }
                        Some(i.to_string())
                    })
                    .collect::<BTreeSet<_>>();
                (!idents.is_empty()).then_some(idents)
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
        let allow_unknown_fields = self.allow_unknown_fields.unwrap_or(false);
        let field_data = FieldData {
            mode: if flat { mode } else { TokenMode::ParseMetaItem },
            target: ParseTarget::Init(Some(&self.variant.ident)),
            inline_expr: &parse_quote_mixed! { inline(&[input], _mode) },
            allowed_expr: &parse_quote_mixed! { allowed },
            transparent: self.transparent.unwrap_or(false),
            allow_unknown_fields,
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
            syn::Fields::Named(_) if mode != TokenMode::ParseMetaItem => {
                let field_names = Field::to_field_names_tokens(&self.fields, crate_, priv_);
                let accepts_all = if allow_unknown_fields {
                    quote_mixed! { true }
                } else {
                    Field::to_accepts_all_tokens(&self.fields, crate_)
                        .unwrap_or_else(|| quote_mixed! { false })
                };
                Some(quote_mixed! {
                    let allowed = #field_names;
                    let validate = !(#accepts_all);
                })
            }
            syn::Fields::Unnamed(_) => Some(quote_mixed! {
                let mut index = 0;
            }),
            _ => None,
        };
        if flat {
            quote_mixed! {
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
            let flag = flag.unwrap_or_else(|| {
                quote_mixed! {
                    #crate_::Result::Err(#priv_::parse_helpers::flag_disallowed_error(span))
                }
            });
            quote_mixed! {
                #pre
                let inline = |inputs: &[#priv_::ParseStream<'_>], _mode: #crate_::ParseMode| {
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
    pub(super) fn to_parsing_tokens(
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
            let idents = v.idents.iter().map(|i| i.to_string()).collect::<Vec<_>>();
            let parse = v.to_field_parsing_tokens(crate_, priv_, mode, false);
            Some(quote_mixed! {
                #(#priv_::Option::Some(k @ #idents))|* => {
                    if let #priv_::Option::Some(key) = key {
                        #priv_::parse_helpers::only_one_variant(
                            span,
                            prefix,
                            (key, k),
                            &errors
                        );
                    } else {
                        match k {
                            #(#idents => {
                                key = #priv_::Option::Some(#idents);
                            })*
                            _ => {}
                        }
                        #parse
                    }
                },
            })
        });
        let any_flat = variants.iter().any(|v| v.flatten.unwrap_or(false));
        let paths_ident = any_flat.then(|| syn::Ident::new("paths", Span::mixed_site()));
        let paths_ident = paths_ident.as_ref().into_iter().collect::<Vec<_>>();
        let mut flat_matches = variants
            .iter()
            .filter_map(|v| {
                if !v.flatten.unwrap_or(false) {
                    return None;
                }
                let key = v.field_key();
                let paths_key = key.iter().map(|idents| {
                    quote_mixed! { &[#(#idents),*] }
                });
                let cond = (!key.is_empty()).then(|| {
                    quote_mixed! {
                        if #priv_::parse_helpers::has_paths(&paths, &[#(#paths_key),*])
                    }
                });
                let parse = v.to_field_parsing_tokens(crate_, priv_, mode, true);
                let disallow_paths = paths_ident.iter().filter_map(|paths_ident| {
                    if allow_unknown_fields || v.allow_unknown_fields.unwrap_or(false) {
                        return None;
                    }
                    let ident = v.variant.ident.to_string();
                    let cur_flat_names = v.to_flat_field_names_tokens(crate_);
                    let other_flat_names = variants.iter().flat_map(|ov| {
                        if std::ptr::eq(ov, v) {
                            return Vec::new().into_iter();
                        }
                        ov.to_flat_field_names_tokens(crate_).into_iter()
                    });
                    // only check these if the parent is validating, because it cannot know which
                    // individual flattened fields are allowed
                    Some(quote_mixed! {
                        if !validate {
                            #priv_::parse_helpers::remove_paths(
                                &mut #paths_ident,
                                &[#(#cur_flat_names),*],
                            );
                            #priv_::parse_helpers::disallow_paths(
                                &#paths_ident,
                                &[#(#other_flat_names),*],
                                #ident,
                                &errors,
                            );
                        }
                    })
                });
                Some((
                    key,
                    quote_mixed! {
                        #cond {
                            #parse
                            #(#disallow_paths)*
                        }
                    },
                ))
            })
            .collect::<BTreeMap<_, _>>();
        let validate = (!allow_unknown_fields).then(|| {
            quote_mixed! {
                if validate {
                    let _ = #priv_::parse_helpers::parse_struct(inputs, |input, p, span| {
                        #priv_::parse_helpers::check_unknown_attribute(p, span, allowed, &errors);
                        #priv_::parse_helpers::skip_named_meta_item(input);
                        #crate_::Result::Ok(())
                    });
                }
            }
        });
        let inputs_expr = (any_flat || validate.is_some())
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
        let empty_match = flat_matches.remove(&BTreeSet::new()).unwrap_or_else(|| {
            if let Some(target) = target {
                return quote_mixed! {
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
                    quote_mixed! { &[#(#idents),*] }
                });
                quote_mixed! { &[#(#key_exprs),*] }
            });
            quote_mixed! {
                {
                    #validate
                    #priv_::parse_helpers::variant_required(
                        span,
                        prefix,
                        &[#(&[&[#variant_keys]],)* #(#flat_keys,)*],
                        &errors
                    );
                }
            }
        });
        let disallow_flats = paths_ident.iter().filter_map(|paths_ident| {
            if allow_unknown_fields {
                return None;
            }
            let flat_names = variants
                .iter()
                .flat_map(|v| v.to_flat_field_names_tokens(crate_).into_iter());
            Some(quote_mixed! {
                if !validate {
                    #paths_ident.remove(key.unwrap());
                    #priv_::parse_helpers::disallow_paths(
                        &#paths_ident,
                        &[#(#flat_names),*],
                        key.unwrap(),
                        &errors,
                    );
                }
            })
        });
        let flat_matches = flat_matches.values();
        quote_mixed! {
            let mut key: #priv_::Option<&'static #priv_::str> = #priv_::Option::None;
            let mut value = #priv_::Option::None;
            let errors = #crate_::Errors::new();
            #(let mut #paths_ident = #priv_::HashMap::<#priv_::String, #priv_::Span>::new();)*
            #priv_::parse_helpers::parse_struct(#inputs_expr, |input, p, span| {
                let inputs = [input];
                let inputs = inputs.as_slice();
                let cur = p.strip_prefix(prefix);
                #(if let #priv_::Option::Some(cur) = cur {
                    #paths_ident.insert(#priv_::ToOwned::to_owned(cur), span);
                })*
                match cur {
                    #(#variant_matches)*
                    _ => {
                        #priv_::parse_helpers::skip_named_meta_item(input);
                    }
                }
                #crate_::Result::Ok(())
            })?;
            errors.check()?;
            let errors = #crate_::Errors::new();
            if value.is_some() {
                #(#disallow_flats)*
                #validate
            } else {
                #(#flat_matches else)*
                #empty_match
            }
            errors.check()?;
            #crate_::Result::Ok(value.unwrap())
        }
    }
    pub fn to_flat_field_names_tokens(&self, crate_: &syn::Path) -> Vec<TokenStream> {
        if !self.flatten.unwrap_or(false) {
            return Vec::new();
        }
        self.fields
            .iter()
            .map(|field| match &field.flatten {
                Some(FieldFlatten {
                    value: true,
                    prefix,
                    ..
                }) => {
                    let ty = &field.field.ty;
                    let names = quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatNamed>::field_names()
                    };
                    let prefix = prefix
                        .as_ref()
                        .map(parse_helpers::path_to_string)
                        .unwrap_or_default();
                    quote_mixed! { (#prefix, #names) }
                }
                _ => {
                    let idents = field.idents.iter().map(|i| i.to_string());
                    quote_mixed! { ("", &[#(#idents),*]) }
                }
            })
            .collect()
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
                            transparent = Some(parse_helpers::parse_named_meta_item::<
                                Option<SpannedValue<bool>>,
                            >(input, span)?);
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
                            flatten = Some(parse_helpers::parse_named_meta_item::<
                                Option<SpannedValue<bool>>,
                            >(input, span)?);
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
                let transparent = transparent.flatten();
                let flatten = flatten.flatten();
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                if rename.is_none() && !flatten.map(|v| *v).unwrap_or(false) {
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
                    flatten: flatten.map(|v| *v),
                    transparent: transparent.map(|v| *v),
                    allow_unknown_fields,
                })
            },
        )
    }
}
use super::*;
use deluxe_core::{
    parse_helpers::{self, FieldStatus},
    ParseAttributes, ParseMetaItem, Result, SpannedValue,
};
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
    pub skip: Option<SpannedValue<bool>>,
    pub allow_unknown_fields: Option<bool>,
}

impl<'v> Variant<'v> {
    #[inline]
    pub fn field_names() -> &'static [&'static str] {
        &[
            "skip",
            "rename",
            "flatten",
            "alias",
            "transparent",
            "allow_unknown_fields",
        ]
    }
    #[inline]
    pub fn is_skipped(&self) -> bool {
        self.skip.map(|v| *v).unwrap_or(false)
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
        flat: Option<FlatMode>,
    ) -> TokenStream {
        let allow_unknown_fields = self.allow_unknown_fields.unwrap_or(false);
        let mut field_data = FieldData {
            mode,
            target: ParseTarget::Init(Some(&self.variant.ident)),
            inline_expr: &parse_quote_mixed! { inline(&[input], _mode) },
            allowed_expr: &parse_quote_mixed! { allowed },
            transparent: self.transparent.unwrap_or(false),
            variant: true,
            allow_unknown_fields,
        };
        let (pre, post) =
            Field::to_pre_post_tokens(&self.fields, &self.variant.fields, crate_, &field_data);
        if field_data.transparent {
            let name = self
                .fields
                .iter()
                .enumerate()
                .find_map(|(i, f)| {
                    (f.is_parsable() && !f.is_flat())
                        .then(|| quote::format_ident!("field{i}", span = Span::mixed_site()))
                })
                .unwrap();
            return quote_mixed! {
                #pre
                if let #priv_::Option::Some(v) = errors.push_result(
                    #crate_::ParseMetaItem::parse_meta_item_named(input, p, span).and_then(|v| {
                        #name = #priv_::FieldStatus::Some(v);
                        #post
                    })
                ) {
                    if !value.is_some() {
                        value = #priv_::FieldStatus::Some(v);
                    }
                } else {
                    #priv_::parse_helpers::skip_meta_item(input);
                    if value.is_none() {
                        value = #priv_::FieldStatus::ParseError;
                    }
                }
            };
        }
        if flat.is_none() {
            field_data.mode = TokenMode::ParseMetaItem;
        }
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
            syn::Fields::Named(_) => {
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
                let mut index = 0usize;
            }),
            _ => None,
        };
        if let Some(flat) = flat {
            // unknown fields in an empty key really means the variant is unknown,
            // so error out early and don't continue parsing
            let validate_empty = match flat {
                FlatMode::Empty { all_keys } if !allow_unknown_fields => Some(quote_mixed! {
                    if validate {
                        if !errors.is_empty() {
                            return #crate_::Result::Err(errors.check().unwrap_err());
                        }
                        let _ = #priv_::parse_helpers::parse_struct(
                            &#priv_::parse_helpers::fork_inputs(inputs),
                            |input, p, span| {
                                #priv_::parse_helpers::check_unknown_attribute(
                                    p, span, allowed, &errors,
                                );
                                #priv_::parse_helpers::skip_meta_item(input);
                                #crate_::Result::Ok(())
                            },
                        );
                        if !errors.is_empty() {
                            #priv_::parse_helpers::variant_required(
                                span,
                                prefix,
                                &#all_keys,
                                &errors
                            );
                            return #crate_::Result::Err(errors.check().unwrap_err());
                        }
                    }
                }),
                _ => None,
            };
            let unset_validate = validate_empty.as_ref().map(|_| {
                quote_mixed! {
                    let validate = false;
                }
            });
            quote_mixed! {
                #pre
                #validate_empty
                let ret = (|| {
                    #unset_validate
                    #inline
                })();
                if let #priv_::Option::Some(v) = errors.push_result(ret) {
                    value = #priv_::FieldStatus::Some(v);
                } else if value.is_none() {
                    value = #priv_::FieldStatus::ParseError;
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
                let mut inline = |inputs: &[#priv_::ParseStream<'_>], _mode: #crate_::ParseMode| {
                    #inline
                };
                let res = match #priv_::parse_helpers::try_parse_named_meta_item(input) {
                    #crate_::Result::Ok(#priv_::parse_helpers::NamedParse::Equals) => {
                        let _mode = #crate_::ParseMode::Named(span);
                        #parse
                    },
                    #crate_::Result::Ok(#priv_::parse_helpers::NamedParse::Paren(buffer)) => {
                        inline(&[&buffer], #crate_::ParseMode::Named(span))
                            .and_then(|v| {
                                #priv_::parse_helpers::parse_eof_or_trailing_comma(&buffer)?;
                                #crate_::Result::Ok(v)
                        })
                    },
                    #crate_::Result::Ok(#priv_::parse_helpers::NamedParse::Flag) => {
                        #flag
                    },
                    #crate_::Result::Err(e) => #crate_::Result::Err(e),
                };
                if let #priv_::Option::Some(v) = errors.push_result(res) {
                    value = #priv_::FieldStatus::Some(v);
                } else {
                    #priv_::parse_helpers::skip_meta_item(input);
                    if value.is_none() {
                        value = #priv_::FieldStatus::ParseError;
                    }
                }
            }
        }
    }
    fn all_variant_key_messages(
        variants: &[Self],
        all_keys: &[Option<BTreeSet<BTreeSet<String>>>],
    ) -> TokenStream {
        let msgs = variants
            .iter()
            .filter(|v| !v.is_skipped())
            .enumerate()
            .map(|(i, v)| {
                if v.flatten.unwrap_or(false) {
                    let keys = all_keys[i].as_ref().unwrap();
                    if keys.is_empty() {
                        let ident = &v.variant.ident;
                        let msg = format!("fields from `{ident}`");
                        quote_mixed! { &[&[#msg]] }
                    } else {
                        let key_exprs = keys.iter().map(|idents| {
                            let idents = idents.iter().map(|i| format!("`{i}`"));
                            quote_mixed! { &[#(#idents),*] }
                        });
                        quote_mixed! { &[#(#key_exprs),*] }
                    }
                } else {
                    let ident = v.idents.first().map(|i| format!("`{i}`"));
                    quote_mixed! { &[&[#ident]] }
                }
            });
        quote_mixed! { [#(#msgs,)*] }
    }
    pub(super) fn to_parsing_tokens(
        variants: &[Self],
        crate_: &syn::Path,
        mode: TokenMode,
        target: Option<&TokenStream>,
        and_thens: &[TokenStream],
        allow_unknown_fields: bool,
    ) -> TokenStream {
        let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
        let priv_ = &priv_path;
        let variant_matches = variants.iter().filter_map(|v| {
            if v.flatten.unwrap_or(false) || v.is_skipped() {
                return None;
            }
            let idents = v.idents.iter().map(|i| i.to_string()).collect::<Vec<_>>();
            let parse = v.to_field_parsing_tokens(crate_, priv_, mode, None);
            Some(quote_mixed! {
                #(#priv_::Option::Some(k @ #idents))|* => {
                    if let #priv_::Option::Some(key) = key {
                        #priv_::parse_helpers::only_one_variant(
                            span,
                            prefix,
                            (key, k),
                            &errors
                        );
                    }
                    if key.is_none() {
                        match k {
                            #(#idents => {
                                key = #priv_::Option::Some(#idents);
                            })*
                            _ => {}
                        }
                    }
                    #parse
                },
            })
        });
        let any_flat = variants
            .iter()
            .any(|v| v.flatten.unwrap_or(false) && !v.is_skipped());
        let paths_ident = any_flat.then(|| syn::Ident::new("paths", Span::mixed_site()));
        let paths_ident = paths_ident.as_ref().into_iter().collect::<Vec<_>>();
        let all_flat_keys = variants
            .iter()
            .filter(|v| !v.is_skipped())
            .map(|v| v.flatten.unwrap_or(false).then(|| v.field_key()))
            .collect::<Vec<_>>();
        let mut flat_matches = variants
            .iter()
            .filter(|v| !v.is_skipped())
            .enumerate()
            .filter_map(|(i, v)| {
                if !v.flatten.unwrap_or(false) {
                    return None;
                }
                let key = all_flat_keys[i].as_ref().unwrap();
                let paths_key = key.iter().map(|idents| {
                    quote_mixed! { &[#(#idents),*] }
                });
                let cond = (!key.is_empty()).then(|| {
                    quote_mixed! {
                        if #priv_::parse_helpers::has_paths(&paths, &[#(#paths_key),*])
                    }
                });
                let flat_mode = if key.is_empty() {
                    let all_keys = Self::all_variant_key_messages(variants, &all_flat_keys);
                    FlatMode::Empty { all_keys }
                } else {
                    FlatMode::Tagged
                };
                let parse = v.to_field_parsing_tokens(crate_, priv_, mode, Some(flat_mode));
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
                        #priv_::parse_helpers::skip_meta_item(input);
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
                        value = #priv_::FieldStatus::Some((#target));
                    }
                };
            }

            let all_keys = Self::all_variant_key_messages(variants, &all_flat_keys);
            quote_mixed! {
                {
                    #validate
                    #priv_::parse_helpers::variant_required(
                        span,
                        prefix,
                        &#all_keys,
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
            let mut value = #priv_::FieldStatus::None;
            #(let mut #paths_ident = #priv_::HashMap::<#priv_::SmallString<'static>, #priv_::Span>::new();)*
            let errors = #crate_::Errors::new();
            errors.push_result(#priv_::parse_helpers::parse_struct(#inputs_expr, |input, p, span| {
                let inputs = [input];
                let inputs = inputs.as_slice();
                let cur = p.strip_prefix(prefix);
                #(if let #priv_::Option::Some(cur) = cur {
                    #paths_ident.insert(<#priv_::SmallString as #priv_::From<_>>::from(cur).into_owned(), span);
                })*
                match cur {
                    #(#variant_matches)*
                    _ => {
                        #priv_::parse_helpers::skip_meta_item(input);
                    }
                }
                #crate_::Result::Ok(())
            }));
            if value.is_some() {
                #(#disallow_flats)*
                #validate
            } else if value.is_none() {
                #(#flat_matches else)*
                #empty_match
            }
            let value = value.into_option();
            #(let value = value.and_then(|v| {
                let f = #and_thens;
                errors.push_result(f(v))
            });)*
            #priv_::parse_helpers::skip_all(inputs);
            errors.check()?;
            #crate_::Result::Ok(value.unwrap_or_else(|| #priv_::unreachable!()))
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
                        .map(parse_helpers::key_to_string)
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
                let mut alias_span = FieldStatus::None;
                let mut idents = Vec::new();
                let mut flatten = FieldStatus::<Option<SpannedValue<bool>>>::None;
                let mut rename = FieldStatus::None;
                let mut transparent = FieldStatus::<Option<SpannedValue<bool>>>::None;
                let mut skip = FieldStatus::None;
                let mut allow_unknown_fields = FieldStatus::None;
                let fields = variant
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
                                                "`transparent` variant field cannot be `flat`",
                                            ));
                                        } else if first.append.map(|v| *v).unwrap_or(false) {
                                            return Err(syn::Error::new(
                                                span,
                                                "`transparent` variant field cannot be `append`",
                                            ));
                                        } else if iter.next().is_none() {
                                            return <_>::parse_meta_item_named(input, path, span);
                                        }
                                    }
                                    Err(syn::Error::new(
                                        span,
                                        "`transparent` variant must have only one parseable field",
                                    ))
                                },
                            );
                        }
                        "allow_unknown_fields" => {
                            if matches!(variant.fields, syn::Fields::Unnamed(_)) {
                                errors.push(
                                    span,
                                    "`allow_unknown_fields` not allowed on tuple variant",
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
                        "flatten" => flatten.parse_named_item("flatten", input, span, &errors),
                        "rename" => {
                            rename.parse_named_item_with(
                                "rename",
                                input,
                                span,
                                &errors,
                                |input, _, span| {
                                    let name = <_>::parse_meta_item_named(input, path, span)?;
                                    if variant.ident == name {
                                        Err(syn::Error::new(
                                            span,
                                            "cannot rename variant to its own name",
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
                        "alias" => {
                            match errors.push_result(<_>::parse_meta_item_named(input, path, span))
                            {
                                Some(alias) => {
                                    if variant.ident == alias {
                                        errors.push(span, "cannot alias variant to its own name");
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
                        "skip" => skip.parse_named_item("skip", input, span, &errors),
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
                let transparent = transparent.flatten();
                let flatten = flatten.flatten();
                deluxe_core::only_one!("", &errors, flatten, rename);
                deluxe_core::only_one!("", &errors, flatten, ("alias", alias_span.as_ref()));
                if rename.is_none() && !flatten.map(|v| *v).unwrap_or(false) {
                    let ident = heck::ToSnakeCase::to_snake_case(
                        syn::ext::IdentExt::unraw(&variant.ident)
                            .to_string()
                            .as_str(),
                    );
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
                if matches!(variant.fields, syn::Fields::Unnamed(_)) {
                    let mut has_default_gap = false;
                    for field in fields.iter().rev() {
                        if field.is_parsable() && !field.is_flat() {
                            if let Some(default) = &field.default {
                                if has_default_gap {
                                    errors.push(
                                        default.span(),
                                        "`default` fields can only be at the end of a tuple variant",
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
                    variant,
                    fields,
                    idents: idents.into_iter().collect(),
                    flatten: flatten.map(|v| *v).into(),
                    transparent: transparent.map(|v| *v).into(),
                    skip: skip.into(),
                    allow_unknown_fields: allow_unknown_fields.into(),
                })
            },
        )
    }
}

pub enum FlatMode {
    Tagged,
    Empty { all_keys: TokenStream },
}

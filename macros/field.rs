use deluxe_core::{parse_helpers, ParseMode, Result};
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashSet;
use syn::parse::ParseStream;

pub enum FieldDefault {
    Default,
    Expr(syn::Expr),
}

impl deluxe_core::ParseMetaItem for FieldDefault {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        Ok(Self::Expr(input.parse()?))
    }
    #[inline]
    fn parse_meta_item_flag(_span: proc_macro2::Span) -> Result<Self> {
        Ok(Self::Default)
    }
}

#[derive(Default)]
pub struct FieldFlatten {
    pub prefix: Option<syn::Path>,
}

impl deluxe_core::ParseMetaItem for FieldFlatten {
    #[inline]
    fn parse_meta_item(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        <parse_helpers::Brace as parse_helpers::ParseDelimited>::parse_delimited_meta_item(
            input,
            ParseMode::Named,
        )
    }
    #[inline]
    fn parse_meta_item_inline(input: ParseStream, _mode: deluxe_core::ParseMode) -> Result<Self> {
        let mut flatten = Self::default();
        let errors = crate::Errors::new();
        let res = parse_helpers::parse_struct(&[input], |input, path, span| {
            match path {
                "prefix" => {
                    if flatten.prefix.is_some() {
                        errors.push_spanned(&path, "Duplicate attribute for `prefix`");
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
    fn parse_meta_item_flag(_span: proc_macro2::Span) -> Result<Self> {
        Ok(Self::default())
    }
}

pub struct Field<'f> {
    pub field: &'f syn::Field,
    pub idents: Vec<syn::Ident>,
    pub default: Option<FieldDefault>,
    pub with: Option<syn::Path>,
    pub flatten: Option<FieldFlatten>,
}

impl<'f> Field<'f> {
    fn parse_tokens(&self, crate_: &syn::Path) -> TokenStream {
        quote! {}
    }
}

impl<'f> deluxe_core::ParseAttributes<'f, syn::Field> for Field<'f> {
    fn path_matches(path: &syn::Path) -> bool {
        path.is_ident("deluxe")
    }
    fn parse_attributes(field: &'f syn::Field) -> Result<Self> {
        const NAMED_ATTRS: &[&str] = &["rename", "flatten", "default", "alias"];
        const UNNAMED_ATTRS: &[&str] = &["flatten", "default"];
        let named = field.ident.is_some();
        let attr_names = if named { NAMED_ATTRS } else { UNNAMED_ATTRS };
        let errors = crate::Errors::new();
        let mut idents = HashSet::new();
        let mut default = None;
        let mut with = None;
        let mut flatten = None;
        let mut rename = false;
        let attrs = deluxe_core::HasAttributes::attrs(field);
        for attr in attrs {
            if Self::path_matches(&attr.path) {
                let res = parse_helpers::parse_struct_attr_tokens([&attr.tokens], |input| {
                    parse_helpers::parse_struct(input, |input, path, span| {
                        match path {
                            "flatten" => {
                                if flatten.is_some() {
                                    errors.push_spanned(&path, "Duplicate attribute for `flatten`");
                                }
                                if flatten.is_none() && !named && default.is_some() {
                                    errors
                                        .push(span, "Only one of `default`, `flatten` is allowed");
                                }
                                flatten = Some(parse_helpers::parse_named_meta_item(input)?);
                            }
                            "default" => {
                                if default.is_some() {
                                    errors.push_spanned(&path, "Duplicate attribute for `default`");
                                }
                                if default.is_none() && !named && flatten.is_some() {
                                    errors
                                        .push(span, "Only one of `default`, `flatten` is allowed");
                                }
                                default = Some(parse_helpers::parse_named_meta_item(input)?);
                            }
                            "with" => {
                                if with.is_some() {
                                    errors.push_spanned(&path, "Duplicate attribute for `with`");
                                }
                                with = Some(parse_helpers::parse_named_meta_item(input)?);
                            }
                            "rename" if named => {
                                if rename {
                                    errors.push_spanned(&path, "Duplicate attribute for `rename`");
                                }
                                rename = true;
                                idents.insert(parse_helpers::parse_named_meta_item(input)?);
                            }
                            "alias" if named => {
                                idents.insert(parse_helpers::parse_named_meta_item(input)?);
                            }
                            _ => {
                                errors
                                    .push_syn(parse_helpers::unknown_error(path, span, attr_names));
                                parse_helpers::skip_named_meta_item(input);
                            }
                        }
                        Ok(())
                    })
                });
                if let Err(err) = res {
                    errors.extend(err);
                }
            }
        }
        if !rename {
            if let Some(ident) = field.ident.clone() {
                idents.insert(ident);
            }
        }
        Ok(Self {
            field,
            idents: idents.into_iter().collect(),
            default,
            with,
            flatten,
        })
    }
}

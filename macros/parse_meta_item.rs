use deluxe_core::Errors;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::spanned::Spanned;

use crate::types::*;

struct MetaDef {
    pub parse: TokenStream,
    pub inline: Option<TokenStream>,
    pub flag: Option<TokenStream>,
    pub default: Option<TokenStream>,
    pub extra: Option<TokenStream>,
    pub crate_path: syn::Path,
    pub priv_path: syn::Path,
}

#[inline]
fn impl_for_struct(
    input: &syn::DeriveInput,
    struct_: &syn::DataStruct,
    errors: &Errors,
) -> Option<MetaDef> {
    let mut struct_attr = errors.push_result(<Struct as deluxe_core::ParseAttributes<
        syn::DeriveInput,
    >>::parse_attributes(input));

    let crate_path = super::get_crate_path(struct_attr.as_ref().map(|s| s.crate_.clone()), errors)?;
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;

    let any_flat = struct_attr
        .as_ref()
        .map(|s| s.fields.iter().any(|f| f.is_flat()))
        .unwrap_or(false);
    let (parse, parse_flat, inline, flag, field_names, mut extra) = struct_attr
        .as_mut()
        .map(|struct_attr| {
            for f in &mut struct_attr.fields {
                if let Some(span) = f.container.as_ref().and_then(|c| c.value.then_some(c.span)) {
                    if f.default.is_none() {
                        f.default = Some(FieldDefault::Default(span));
                    }
                }
            }

            let fields = struct_attr.fields.as_slice();

            let make_inline_expr = |inputs_expr: TokenStream| -> TokenStream {
                match &struct_.fields {
                    syn::Fields::Named(_) => quote_mixed! {
                        <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                            #inputs_expr,
                            _mode,
                            "",
                            !<Self as #crate_::ParseMetaFlatNamed>::ACCEPTS_ALL,
                        )
                    },
                    syn::Fields::Unnamed(_) => quote_mixed! {
                        <Self as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(#inputs_expr, _mode, 0)
                    },
                    syn::Fields::Unit => quote_mixed! {
                        <Self as #crate_::ParseMetaItem>::parse_meta_item_inline(
                            #inputs_expr, #crate_::ParseMode::Unnamed,
                        )
                    },
                }
            };

            let ItemDef {
                parse,
                inline,
                flag,
                extra_traits,
            } = struct_attr.to_parsing_tokens(
                input,
                crate_,
                TokenMode::ParseMetaItem,
                &make_inline_expr(quote_mixed! { &[input] }),
                &parse_quote_mixed! {
                    <Self as #crate_::ParseMetaFlatNamed>::field_names()
                },
            );
            let (parse_flat, inline) =
                if struct_attr.is_transparent() || matches!(&struct_.fields, syn::Fields::Unit) {
                    (None, inline)
                } else {
                    (inline, Some(make_inline_expr(quote_mixed! { inputs }).into_token_stream()))
                };
            let field_names = match &struct_.fields {
                syn::Fields::Named(_) => Some(struct_attr.to_field_names_tokens(crate_, priv_)),
                syn::Fields::Unnamed(_) => {
                    let field_count = fields.iter().filter(|f| !f.is_flat()).count();
                    let extra_counts = fields.iter().filter_map(|f| {
                        if !f.is_flat() {
                            return None;
                        }
                        let ty = &f.field.ty;
                        Some(quote_spanned! { ty.span() =>
                            <#ty as #crate_::ParseMetaFlatUnnamed>::field_count().unwrap_or(0)
                        })
                    });
                    Some(quote_mixed! {
                        #priv_::Option::Some(#field_count #( +  #extra_counts)*)
                    })
                }
                _ => None,
            };
            (parse, parse_flat, inline, flag, field_names, extra_traits)
        })
        .unwrap_or_else(|| {
            let fail = quote_mixed! {
                #priv_::unreachable!()
            };
            (
                fail.clone(),
                Some(fail.clone()),
                Some(fail.clone()),
                Some(fail.clone()),
                Some(fail),
                None,
            )
        });

    match (&struct_.fields, parse_flat) {
        (syn::Fields::Named(_), Some(parse_flat)) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

            let accepts_all = struct_attr
                .as_ref()
                .and_then(|s| s.to_accepts_all_tokens(crate_))
                .into_iter();
            extra.get_or_insert_with(TokenStream::new).extend(quote_mixed! {
                impl #impl_generics #crate_::ParseMetaFlatNamed for #struct_ident #type_generics #where_clause {
                    #(const ACCEPTS_ALL: #priv_::bool = #accepts_all;)*
                    #[inline]
                    fn field_names() -> &'static [&'static #priv_::str] {
                        #field_names
                    }
                    #[inline]
                    fn parse_meta_flat_named<'____s, ____S>(
                        inputs: &[____S],
                        _mode: #crate_::ParseMode,
                        prefix: &#priv_::str,
                        validate: #priv_::bool,
                    ) -> #crate_::Result<Self>
                    where
                        ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
                    {
                        let span = _mode.to_full_span(inputs);
                        #parse_flat
                    }
                }

                impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                    #[inline]
                    fn field_count() -> #priv_::Option<#priv_::usize> {
                        #priv_::Option::None
                    }
                    #[inline]
                    fn parse_meta_flat_unnamed<'____s, ____S>(
                        inputs: &[____S],
                        mode: #crate_::ParseMode,
                        _index: #priv_::usize,
                    ) -> #crate_::Result<Self>
                    where
                        ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
                    {
                        <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(inputs, mode, "", true)
                    }
                }
            });
        }
        (syn::Fields::Unnamed(_), Some(parse_flat)) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
            let index_mut = any_flat.then(|| quote! { mut });
            extra.get_or_insert_with(TokenStream::new).extend(quote_mixed! {
                impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                    #[inline]
                    fn field_count() -> #priv_::Option<#priv_::usize> {
                        #field_names
                    }
                    #[inline]
                    fn parse_meta_flat_unnamed<'____s, ____S>(
                        inputs: &[____S],
                        _mode: #crate_::ParseMode,
                        #index_mut index: #priv_::usize,
                    ) -> #crate_::Result<Self>
                    where
                        ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
                    {
                        #parse_flat
                    }
                }
            });
        }
        _ => {}
    }
    let default = struct_attr.and_then(|s| s.default).map(|d| {
        d.to_expr(Some(&syn::parse_quote_spanned! { d.span() => Self }), priv_)
            .into_owned()
    });
    Some(MetaDef {
        parse,
        inline,
        flag,
        default,
        extra,
        crate_path,
        priv_path,
    })
}

#[inline]
fn impl_for_enum(input: &syn::DeriveInput, errors: &Errors) -> Option<MetaDef> {
    let enum_attr = errors.push_result(
        <Enum as deluxe_core::ParseAttributes<syn::DeriveInput>>::parse_attributes(input),
    );

    let crate_path = super::get_crate_path(enum_attr.as_ref().map(|e| e.crate_.clone()), errors)?;
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;

    let enum_ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let (parse, field_names) = enum_attr
        .as_ref()
        .map(|e| {
            for v in &e.variants {
                for f in &v.fields {
                    if let Some(container) = f.container.as_ref() {
                        if f.is_container() && f.default.is_none() {
                            errors.push(
                                container.span(),
                                "derive(ParseMetaItem) requires container field to have `default`",
                            );
                        }
                    }
                }
            }
            let parse = e.to_inline_parsing_tokens(crate_, TokenMode::ParseMetaItem);
            let field_names = e.to_field_names_tokens(crate_, priv_);
            (
                quote_mixed! {
                    let allowed = <Self as #crate_::ParseMetaFlatNamed>::field_names();
                    #parse
                },
                field_names,
            )
        })
        .unwrap_or_else(|| {
            let fail = quote_mixed! {
                #priv_::unreachable!()
            };
            (fail.clone(), fail)
        });
    Some(MetaDef {
        parse: quote_mixed! {
            <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                input, _mode.to_named(input),
            )
        },
        inline: Some(quote_mixed! {
            <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                inputs,
                _mode,
                "",
                !<Self as #crate_::ParseMetaFlatNamed>::ACCEPTS_ALL,
            )
        }),
        flag: Some(quote_mixed! {
            #priv_::parse_helpers::parse_empty_meta_item(span, #crate_::ParseMode::Named(span))
        }),
        default: enum_attr.and_then(|s| s.default).map(|d| {
            d.to_expr(Some(&syn::parse_quote_spanned! { d.span() => Self }), priv_)
                .into_owned()
        }),
        extra: Some(quote_mixed! {
            impl #impl_generics #crate_::ParseMetaFlatNamed for #enum_ident #type_generics #where_clause {
                #[inline]
                fn field_names() -> &'static [&'static #priv_::str] {
                    #field_names
                }
                #[inline]
                fn parse_meta_flat_named<'____s, ____S>(
                    inputs: &[____S],
                    _mode: #crate_::ParseMode,
                    prefix: &#priv_::str,
                    validate: #priv_::bool,
                ) -> #crate_::Result<Self>
                where
                    ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
                {
                    let span = _mode.to_full_span(inputs);
                    #parse
                }
            }

            impl #impl_generics #crate_::ParseMetaFlatUnnamed for #enum_ident #type_generics #where_clause {
                #[inline]
                fn field_count() -> #priv_::Option<#priv_::usize> {
                    #priv_::Option::None
                }
                #[inline]
                fn parse_meta_flat_unnamed<'____s, ____S>(
                    inputs: &[____S],
                    mode: #crate_::ParseMode,
                    _index: #priv_::usize,
                ) -> #crate_::Result<Self>
                where
                    ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
                {
                    <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(inputs, mode, "", true)
                }
            }
        }),
        crate_path,
        priv_path,
    })
}

pub fn impl_parse_meta_item(input: syn::DeriveInput, errors: &Errors) -> TokenStream {
    let meta = match &input.data {
        syn::Data::Struct(struct_) => impl_for_struct(&input, struct_, errors),
        syn::Data::Enum(_) => impl_for_enum(&input, errors),
        syn::Data::Union(union) => {
            errors.push_spanned(
                union.union_token,
                "union not supported with derive(ParseMetaItem)",
            );
            return Default::default();
        }
    };
    let MetaDef {
        parse,
        inline,
        flag,
        default,
        extra,
        crate_path: crate_,
        priv_path: priv_,
    } = match meta {
        Some(m) => m,
        None => return Default::default(),
    };

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let inline = inline.map(|inline| {
        quote_mixed! {
            #[inline]
            fn parse_meta_item_inline<'____s, ____S>(
                inputs: &[____S],
                _mode: #crate_::ParseMode,
            ) -> #crate_::Result<Self>
            where
                ____S: #priv_::Borrow<#priv_::ParseBuffer<'____s>>,
            {
                #inline
            }
        }
    });
    let flag = flag.map(|flag| {
        quote_mixed! {
            #[inline]
            fn parse_meta_item_flag(span: #priv_::Span) -> #crate_::Result<Self> {
                #flag
            }
        }
    });
    let missing = default.map(|default| {
        quote_mixed! {
            #[inline]
            fn missing_meta_item(name: &#priv_::str, span: #priv_::Span) -> #crate_::Result<Self> {
                #crate_::Result::Ok((#default))
            }
        }
    });
    quote_mixed! {
        impl #impl_generics #crate_::ParseMetaItem for #ident #type_generics #where_clause {
            #[inline]
            fn parse_meta_item(
                input: #priv_::ParseStream,
                _mode: #crate_::ParseMode,
            ) -> #crate_::Result<Self> {
                #parse
            }
            #inline
            #flag
            #missing
        }
        #extra
    }
}

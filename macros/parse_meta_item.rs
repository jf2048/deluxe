use deluxe_core::Errors;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use crate::field::*;

#[inline]
fn impl_for_struct(
    input: &syn::DeriveInput,
    struct_: &syn::DataStruct,
    errors: &Errors,
) -> (
    TokenStream,
    Option<TokenStream>,
    Option<TokenStream>,
    Option<TokenStream>,
    syn::Path,
    syn::Path,
) {
    let struct_attr =
        match <Struct as deluxe_core::ParseAttributes<syn::DeriveInput>>::parse_attributes(input) {
            Ok(s) => Some(s),
            Err(err) => {
                errors.push_syn(err);
                None
            }
        };

    let crate_path = struct_attr
        .as_ref()
        .and_then(|s| s.crate_.clone())
        .unwrap_or_else(|| super::crate_path(errors));
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;

    let fields = struct_attr
        .as_ref()
        .map(|s| s.fields.as_slice())
        .unwrap_or_else(|| &[]);
    let any_flat = fields.iter().any(|f| f.is_flat());
    let default_set = struct_attr.as_ref().and_then(|s| {
        s.default.as_ref().map(|d| {
            let expr = d.to_expr(&syn::parse_quote! { Self }, priv_);
            quote_spanned! { Span::mixed_site() =>
                let mut target = #expr;
            }
        })
    });
    let target = default_set.as_ref().map(|_| syn::parse_quote! { target });
    let target = target
        .as_ref()
        .map(ParseTarget::Var)
        .unwrap_or_else(|| ParseTarget::Init(None));
    let transparent = struct_attr
        .as_ref()
        .and_then(|s| s.transparent)
        .unwrap_or(false);
    let inline_expr = match &struct_.fields {
        syn::Fields::Named(_) => syn::parse_quote_spanned! { Span::mixed_site() =>
            <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                &[input],
                #priv_::Option::Some(<Self as #crate_::ParseMetaFlatNamed>::field_names()),
            )
        },
        syn::Fields::Unnamed(_) => syn::parse_quote_spanned! { Span::mixed_site() =>
            <Self as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(&[input], 0)
        },
        syn::Fields::Unit => syn::parse_quote_spanned! { Span::mixed_site() =>
            <Self as #crate_::ParseMetaItem>::parse_meta_item_inline(
                input, #crate_::ParseMode::Unnamed,
            )
        },
    };

    let ItemDef {
        parse,
        inline,
        flag,
    } = Field::to_parsing_tokens(
        fields,
        &struct_.fields,
        crate_,
        TokenMode::ParseMetaItem,
        target,
        &inline_expr,
        transparent,
    );

    match &struct_.fields {
        syn::Fields::Named(_) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
            let field_names = struct_attr
                .as_ref()
                .map(|s| s.to_field_names_tokens(crate_, priv_))
                .unwrap_or_else(|| quote_spanned! { Span::mixed_site() => &[] });
            (
                quote_spanned! { Span::mixed_site() =>
                    <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                        input, #crate_::ParseMode::Named
                    )
                },
                inline,
                flag,
                Some(quote_spanned! { Span::mixed_site() =>
                    impl #impl_generics #crate_::ParseMetaFlatNamed for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn field_names() -> &'static [&'static #priv_::str] {
                            #field_names
                        }
                        #[inline]
                        fn parse_meta_flat_named(
                            inputs: &[#priv_::ParseStream],
                            allowed: #priv_::Option<&[&#priv_::str]>,
                        ) -> #crate_::Result<Self> {
                            <Self as #crate_::ParseMetaFlatPrefixed>::parse_meta_flat_prefixed(
                                inputs,
                                "",
                                allowed,
                            )
                        }
                    }
                    impl #impl_generics #crate_::ParseMetaFlatPrefixed for #struct_ident #type_generics #where_clause {
                        fn parse_meta_flat_prefixed(
                            inputs: &[#priv_::ParseStream],
                            prefix: &#priv_::str,
                            allowed: #priv_::Option<&[&#priv_::str]>,
                        ) -> #crate_::Result<Self> {
                            #default_set
                            #parse
                        }
                    }
                }),
                crate_path,
                priv_path,
            )
        }
        syn::Fields::Unnamed(_) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
            let index_mut = any_flat.then(|| quote! { mut });
            let field_count = fields.iter().filter(|f| !f.is_flat()).count();
            let extra_counts = fields.iter().filter_map(|f| {
                if !f.is_flat() {
                    return None;
                }
                f.flatten.as_ref().map(|_| {
                    let ty = &f.field.ty;
                    quote_spanned! { ty.span() =>
                        <#ty as #crate_::ParseMetaFlatUnnamed>::field_count().unwrap_or(0)
                    }
                })
            });
            let (parse, parse_flat, inline, flag) = if transparent {
                (
                    quote_spanned! { Span::mixed_site() =>
                        #default_set
                        #parse
                    },
                    None,
                    Some(quote_spanned! { Span::mixed_site() =>
                        #default_set
                        #inline
                    }),
                    Some(quote_spanned! { Span::mixed_site() =>
                        #default_set
                        #flag
                    }),
                )
            } else {
                (
                    quote_spanned! { Span::mixed_site() =>
                        <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                            input, #crate_::ParseMode::Unnamed
                        )
                    },
                    Some(quote_spanned! { Span::mixed_site() =>
                        #default_set
                        #parse
                    }),
                    inline,
                    flag,
                )
            };
            (
                parse,
                inline,
                flag,
                parse_flat.map(|parse_flat| {
                    quote_spanned! { Span::mixed_site() =>
                        impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                            #[inline]
                            fn field_count() -> #priv_::Option<#priv_::usize> {
                                #priv_::Option::Some(#field_count #( +  #extra_counts)*)
                            }
                            #[inline]
                            fn parse_meta_flat_unnamed(inputs: &[#priv_::ParseStream], #index_mut index: #priv_::usize) -> #crate_::Result<Self> {
                                #parse_flat
                            }
                        }
                    }
                }),
                crate_path,
                priv_path,
            )
        }
        _ => (parse, inline, flag, None, crate_path, priv_path),
    }
}

#[inline]
fn impl_for_enum(
    input: &syn::DeriveInput,
    errors: &Errors,
) -> (
    TokenStream,
    Option<TokenStream>,
    Option<TokenStream>,
    Option<TokenStream>,
    syn::Path,
    syn::Path,
) {
    let enum_attr =
        match <Enum as deluxe_core::ParseAttributes<syn::DeriveInput>>::parse_attributes(input) {
            Ok(e) => Some(e),
            Err(err) => {
                errors.push_syn(err);
                None
            }
        };
    let crate_path = enum_attr
        .as_ref()
        .and_then(|s| s.crate_.clone())
        .unwrap_or_else(|| super::crate_path(errors));
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;
    let variants = enum_attr
        .as_ref()
        .map(|e| e.variants.as_slice())
        .unwrap_or_else(|| &[]);
    let enum_ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let field_names = enum_attr
        .as_ref()
        .map(|e| e.to_field_names_tokens(crate_, priv_))
        .unwrap_or_else(|| quote_spanned! { Span::mixed_site() => &[] });
    let parse = Variant::to_parsing_tokens(variants, crate_, TokenMode::ParseMetaItem);
    (
        quote_spanned! { Span::mixed_site() =>
            <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                input, #crate_::ParseMode::Named
            )
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
        Some(quote_spanned! { Span::mixed_site() =>
            impl #impl_generics #crate_::ParseMetaFlatNamed for #enum_ident #type_generics #where_clause {
                #[inline]
                fn field_names() -> &'static [&'static #priv_::str] {
                    #field_names
                }
                #[inline]
                fn parse_meta_flat_named(
                    inputs: &[#priv_::ParseStream],
                    allowed: #priv_::Option<&[&#priv_::str]>,
                ) -> #crate_::Result<Self> {
                    <Self as #crate_::ParseMetaFlatPrefixed>::parse_meta_flat_prefixed(
                        inputs,
                        "",
                        allowed,
                    )
                }
            }
            impl #impl_generics #crate_::ParseMetaFlatPrefixed for #enum_ident #type_generics #where_clause {
                fn parse_meta_flat_prefixed(
                    inputs: &[#priv_::ParseStream],
                    prefix: &#priv_::str,
                    allowed: #priv_::Option<&[&#priv_::str]>,
                ) -> #crate_::Result<Self> {
                    #parse
                }
            }
        }),
        crate_path,
        priv_path,
    )
}

pub fn impl_parse_meta_item(input: syn::DeriveInput, errors: &Errors) -> TokenStream {
    let (parse, inline, flag, extra, crate_, priv_) = match &input.data {
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

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let inline = inline.map(|inline| {
        quote_spanned! { Span::mixed_site() =>
            #[inline]
            fn parse_meta_item_inline(
                input: #priv_::ParseStream,
                _mode: #crate_::ParseMode,
            ) -> #crate_::Result<Self> {
                #inline
            }
        }
    });
    let flag = flag.map(|flag| {
        quote_spanned! { Span::mixed_site() =>
            #[inline]
            fn parse_meta_item_flag(span: #priv_::Span) -> #crate_::Result<Self> {
                #flag
            }
        }
    });
    quote_spanned! { Span::mixed_site() =>
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
        }
        #extra
    }
}

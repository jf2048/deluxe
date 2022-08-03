use deluxe_core::{parse_helpers, Errors};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use crate::field::*;

#[inline]
fn impl_for_struct(
    input: &syn::DeriveInput,
    struct_: &syn::DataStruct,
    errors: &Errors,
    crate_: &syn::Path,
    priv_: &syn::Path,
) -> (TokenStream, TokenStream, TokenStream) {
    let field_count = struct_.fields.len();
    let fields = struct_
        .fields
        .iter()
        .filter_map(
            |f| match <Field as deluxe_core::ParseAttributes<syn::Field>>::parse_attributes(f) {
                Ok(f) => Some(f),
                Err(err) => {
                    errors.push_syn(err);
                    None
                }
            },
        )
        .collect::<Vec<_>>();
    let is_unnamed = matches!(&struct_.fields, syn::Fields::Unnamed(_));
    let any_flat = fields.iter().any(|f| f.flatten.is_some());
    let names = fields
        .iter()
        .enumerate()
        .map(|(i, _)| quote::format_ident!("field{}", i, span = Span::mixed_site()))
        .collect::<Vec<_>>();
    let field_errors = {
        let mut cur_index = 0usize;
        let mut extra_counts = quote! {};
        fields.iter().enumerate().filter_map(|(i, f)| {
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
                    let error = format!("missing required field `{}`", f.field.ident.as_ref().unwrap());
                    quote_spanned! { Span::mixed_site() =>
                        if #name.is_none() {
                            errors.push_call_site(#error);
                        }
                    }
                }
            });
            if is_unnamed {
                let ty = &f.field.ty;
                if f.flatten.is_some() {
                    extra_counts.extend(quote_spanned! { ty.span() =>
                        + <#ty as #crate_::ParseMetaFlatUnnamed>::field_count().unwrap_or(0)
                    });
                } else {
                    cur_index += 1;
                }
            }
            push
        }).collect::<Vec<_>>()
    };
    let field_unwraps = fields.iter().enumerate().map(|(i, f)| {
        let name = &names[i];
        let ty = &f.field.ty;
        match &f.default {
            Some(FieldDefault::Default) => quote_spanned! { Span::mixed_site() =>
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

    match &struct_.fields {
        syn::Fields::Named(_) if field_count == 0 => (
            quote_spanned! { Span::mixed_site() =>
                <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                    input, #crate_::ParseMode::Named
                )
            },
            quote! {
                #[inline]
                fn parse_meta_item_inline(
                    input: #priv_::ParseStream,
                    _mode: #crate_::ParseMode,
                ) -> #crate_::Result<Self> {
                    <() as #crate_::ParseMetaItem>::parse_meta_item_inline(input, _mode)?;
                    #crate_::Result::Ok(Self {})
                }
                #[inline]
                fn parse_meta_item_flag(_: #priv_::Span) -> #crate_::Result<Self> {
                    #crate_::Result::Ok(Self {})
                }
            },
            quote! {},
        ),
        syn::Fields::Named(_) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
            let field_names = if fields.iter().any(|f| f.flatten.is_some()) {
                let names = fields.iter().map(|f| match &f.flatten {
                    Some(FieldFlatten {
                        prefix: Some(prefix),
                    }) => {
                        let prefix = parse_helpers::path_to_string(prefix);
                        quote_spanned! { Span::mixed_site() =>
                            vec.extend_from_slice(
                                #priv_::parse_helpers::join_paths(
                                    #prefix,
                                    <T as #crate_::ParseMetaFlatNamed>::field_names()
                                ).as_slice()
                            );
                        }
                    }
                    Some(FieldFlatten { prefix: None }) => quote_spanned! { Span::mixed_site() =>
                        vec.extend_from_slice(<T as #crate_::ParseMetaFlatNamed>::field_names());
                    },
                    None => {
                        let names = f.idents.iter().map(|i| i.to_string());
                        quote_spanned! { Span::mixed_site() =>
                            #(vec.push(#names);)*
                        }
                    }
                });
                quote_spanned! { Span::mixed_site() =>
                    static CELL: #priv_::SyncOnceCell<#priv_::Vec<&'static #priv_::str>> = #priv_::SyncOnceCell::new();
                    CELL.get_or_init(|| {
                        let mut vec = #priv_::Vec::new();
                        #(#names)*
                        vec
                    }).as_slice()
                }
            } else {
                let names = fields.iter().map(|f| {
                    let names = f.idents.iter().map(|i| i.to_string());
                    quote_spanned! { Span::mixed_site() => #(#names),* }
                });
                quote_spanned! { Span::mixed_site() =>
                    &[#(#names),*]
                }
            };
            let field_matches = fields.iter().enumerate().flat_map(|(i, f)| {
                let name = names[i].clone();
                let ident = f.field.ident.as_ref().unwrap();
                let error = format!("duplicate attribute for `{}`", ident);
                f.idents.iter().filter_map(move |ident| {
                    if f.flatten.is_some() {
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
                    quote_spanned! { Span::mixed_site() =>
                        match #call {
                            #crate_::Result::Ok(val) => #name = #priv_::Option::Some(val),
                            #crate_::Result::Err(err) => errors.push_syn(err),
                        }
                    }
                })
            });
            let field_defs = fields.iter().enumerate().map(|(i, f)| {
                let ident = f.field.ident.as_ref().unwrap();
                let name = &names[i];
                quote_spanned! { Span::mixed_site() => #ident: #name }
            });
            (
                quote_spanned! { Span::mixed_site() =>
                    <#priv_::parse_helpers::Brace as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                        input, #crate_::ParseMode::Named
                    )
                },
                quote_spanned! { Span::mixed_site() =>
                    #[inline]
                    fn parse_meta_item_inline(
                        input: #priv_::ParseStream,
                        _mode: #crate_::ParseMode,
                    ) -> #crate_::Result<Self> {
                        <Self as #crate_::ParseMetaFlatNamed>::parse_meta_flat_named(
                            &[input],
                            #priv_::Option::Some(<Self as #crate_::ParseMetaFlatNamed>::field_names()),
                        )
                    }
                    #[inline]
                    fn parse_meta_item_flag(span: #priv_::Span) -> #crate_::Result<Self> {
                        #priv_::parse_helpers::parse_empty_meta_item(span, #crate_::ParseMode::Named)
                    }
                },
                quote_spanned! { Span::mixed_site() =>
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
                            #(#field_errors)*
                            errors.check()?;
                            #(#field_unwraps)*
                            #crate_::Result::Ok(Self {
                                #(#field_defs),*
                            })
                        }
                    }
                },
            )
        }
        syn::Fields::Unnamed(_) if field_count == 0  => (
            quote_spanned! { Span::mixed_site() =>
                <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                    input, #crate_::ParseMode::Unnamed
                )
            },
            quote_spanned! { Span::mixed_site() =>
                #[inline]
                fn parse_meta_item_inline(
                    input: #priv_::ParseStream,
                    _mode: #crate_::ParseMode,
                ) -> #crate_::Result<Self> {
                    <() as #crate_::ParseMetaItem>::parse_meta_item_inline(input, _mode)?;
                    #crate_::Result::Ok(Self())
                }
                #[inline]
                fn parse_meta_item_flag(_: #priv_::Span) -> #crate_::Result<Self> {
                    #crate_::Result::Ok(Self())
                }
            },
            quote! {},
        ),
        syn::Fields::Unnamed(_) if field_count == 1 => {
            let field = fields.first().unwrap();
            let ty = &field.field.ty;
            let path = field
                .with
                .as_ref()
                .map(|p| quote_spanned! { p.span() => #p })
                .unwrap_or_else(|| quote_spanned! { ty.span() => <#ty as #crate_::ParseMetaItem> });
            (
                quote_spanned! { Span::mixed_site() =>
                    #crate_::Result::Ok(Self(
                        #path::parse_meta_item(input, _mode)?
                    ))
                },
                quote_spanned! { Span::mixed_site() =>
                    #[inline]
                    fn parse_meta_item_inline(
                        input: #priv_::ParseStream,
                        _mode: #crate_::ParseMode,
                    ) -> #crate_::Result<Self> {
                        #crate_::Result::Ok(Self(
                            #path::parse_meta_item_inline(input, _mode)?
                        ))
                    }
                    #[inline]
                    fn parse_meta_item_flag(_span: #priv_::Span) -> #crate_::Result<Self> {
                        #crate_::Result::Ok(Self(
                            #path::parse_meta_item_flag(_span)?
                        ))
                    }
                },
                quote! {},
            )
        }
        syn::Fields::Unnamed(_) => {
            let struct_ident = &input.ident;
            let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
            let index_mut = any_flat.then(|| quote! { mut });
            let field_count = fields.iter().filter(|f| f.flatten.is_none()).count();
            let extra_counts = fields.iter().filter_map(|f| f.flatten.as_ref().map(|_| {
                let ty = &f.field.ty;
                quote_spanned! { ty.span() =>
                    <#ty as #crate_::ParseMetaFlatUnnamed>::field_count().unwrap_or(0)
                }
            }));
            let field_matches = fields.iter().enumerate().map(|(index, f)| {
                let name = &names[index];
                let ty = &f.field.ty;
                let call = match (f.flatten.as_ref(), f.with.as_ref()) {
                    (Some(_), Some(m)) => quote_spanned! { Span::mixed_site() =>
                        #m::parse_meta_flat_unnamed(inputs, index)
                    },
                    (None, Some(m)) => quote_spanned! { Span::mixed_site() =>
                        #m::parse_meta_item(input, #crate_::ParseMode::Unnamed)
                    },
                    (Some(_), None) => quote_spanned! { Span::mixed_site() =>
                        <#ty as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(inputs, index)
                    },
                    (None, None) => quote_spanned! { Span::mixed_site() =>
                        <#ty as #crate_::ParseMetaItem>::parse_meta_item(input, #crate_::ParseMode::Unnamed)
                    },
                };
                let increment = any_flat.then(|| {
                    match f.flatten.as_ref() {
                        Some(_) => quote_spanned! { Span::mixed_site() =>
                            if let #priv_::Option::Some(count) = <#ty as #crate_::ParseMetaFlatUnnamed>::field_count() {
                                index += count;
                            }
                        },
                        None => quote_spanned! { Span::mixed_site() =>
                            index += 1;
                        }
                    }
                });
                quote_spanned! { Span::mixed_site() =>
                    #index => {
                        #name = #priv_::Option::Some(#call?);
                        #increment
                    }
                }
            });
            (
                quote_spanned! { Span::mixed_site() =>
                    <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                        input, #crate_::ParseMode::Unnamed
                    )
                },
                quote_spanned! { Span::mixed_site() =>
                    fn parse_meta_item_inline(
                        input: #priv_::ParseStream,
                        _mode: #crate_::ParseMode,
                    ) -> #crate_::Result<Self> {
                        <Self as #crate_::ParseMetaFlatUnnamed>::parse_meta_flat_unnamed(&[input], 0)
                    }
                },
                quote_spanned! { Span::mixed_site() =>
                    impl #impl_generics #crate_::ParseMetaFlatUnnamed for #struct_ident #type_generics #where_clause {
                        #[inline]
                        fn field_count() -> #priv_::Option<#priv_::usize> {
                            #priv_::Option::Some(#field_count #( +  #extra_counts)*)
                        }
                        #[inline]
                        fn parse_meta_flat_unnamed(inputs: &[#priv_::ParseStream], #index_mut index: #priv_::usize) -> #crate_::Result<Self> {
                            #(let mut #names = #priv_::Option::None;)*
                            #priv_::parse_helpers::parse_tuple_struct(inputs, #field_count, |input, inputs, i| {
                                match i {
                                    #(#field_matches)*
                                    _ => #priv_::unreachable!(),
                                }
                                #crate_::Result::Ok(())
                            })?;
                            let errors = #crate_::Errors::new();
                            #(#field_errors)*
                            errors.check()?;
                            #(#field_unwraps)*
                            #crate_::Result::Ok(Self(#(#names),*))
                        }
                    }
                },
            )
        }
        syn::Fields::Unit => (
            quote_spanned! { Span::mixed_site() =>
                <#priv_::parse_helpers::Paren as #priv_::parse_helpers::ParseDelimited>::parse_delimited_meta_item(
                    input, #crate_::ParseMode::Unnamed
                )
            },
            quote_spanned! { Span::mixed_site() =>
                #[inline]
                fn parse_meta_item_inline(
                    input: #priv_::ParseStream,
                    _mode: #crate_::ParseMode,
                ) -> #crate_::Result<Self> {
                    <() as #crate_::ParseMetaItem>::parse_meta_item_inline(input, _mode)?;
                    #crate_::Result::Ok(Self)
                }
                #[inline]
                fn parse_meta_item_flag(_: #priv_::Span) -> #crate_::Result<Self> {
                    #crate_::Result::Ok(Self)
                }
            },
            quote! {},
        ),
    }
}

#[inline]
fn impl_for_enum(
    input: &syn::DeriveInput,
    enum_: &syn::DataEnum,
    errors: &Errors,
    crate_: &syn::Path,
    priv_: &syn::Path,
) -> (TokenStream, TokenStream, TokenStream) {
    (quote! {}, quote! {}, quote! {})
}

pub fn impl_parse_meta_item(input: syn::DeriveInput, errors: &Errors) -> TokenStream {
    let crate_ = super::crate_path(errors);
    let priv_: syn::Path = syn::parse_quote! { #crate_::____private };

    let (parse, extra, extra_trait) = match &input.data {
        syn::Data::Struct(struct_) => impl_for_struct(&input, struct_, errors, &crate_, &priv_),
        syn::Data::Enum(enum_) => impl_for_enum(&input, enum_, errors, &crate_, &priv_),
        syn::Data::Union(union) => {
            errors.push_spanned(
                union.union_token,
                "Union not supported with derive(ParseMetaItem)",
            );
            return Default::default();
        }
    };

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    quote_spanned! { Span::mixed_site() =>
        impl #impl_generics #crate_::ParseMetaItem for #ident #type_generics #where_clause {
            #[inline]
            fn parse_meta_item(
                input: #priv_::ParseStream,
                _mode: #crate_::ParseMode,
            ) -> #crate_::Result<Self> {
                #parse
            }
            #extra
        }
        #extra_trait
    }
}

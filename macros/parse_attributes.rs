use std::borrow::Cow;

use deluxe_core::{parse_helpers, Errors, ParseAttributes};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use crate::field::*;

#[derive(PartialEq)]
pub enum Mode {
    Parse,
    Extract,
}

#[inline]
fn impl_for_struct(
    input: &syn::DeriveInput,
    struct_: &syn::DataStruct,
    errors: &Errors,
) -> (
    TokenStream,
    syn::Path,
    syn::Path,
    Vec<syn::Path>,
    Option<syn::Lifetime>,
    Option<syn::Type>,
) {
    todo!()
}

#[inline]
fn impl_for_enum(
    input: &syn::DeriveInput,
    errors: &Errors,
) -> (
    TokenStream,
    syn::Path,
    syn::Path,
    Vec<syn::Path>,
    Option<syn::Lifetime>,
    Option<syn::Type>,
) {
    todo!()
}

pub fn impl_parse_attributes(input: syn::DeriveInput, errors: &Errors, mode: Mode) -> TokenStream {
    let (parse, crate_, priv_, attributes, container_lifetime, container_ty) = match &input.data {
        syn::Data::Struct(struct_) => impl_for_struct(&input, struct_, errors),
        syn::Data::Enum(_) => impl_for_enum(&input, errors),
        syn::Data::Union(union_) => {
            errors.push_spanned(
                union_.union_token,
                "Union not supported with derive(FromAttributes)",
            );
            return Default::default();
        }
    };

    let container_field: Option<&syn::Field> = None; // TODO

    let ident = &input.ident;
    let mut generics = input.generics.clone();

    let mut container_is_ref = false;
    let mut container_is_generic = false;
    let mut lifetime = None;
    let mut has_attr_ty = None;
    if let Some(container_field) = container_field {
        let mut ty = &container_field.ty;
        if let syn::Type::Reference(ref_) = ty {
            container_is_ref = true;
            lifetime = ref_.lifetime.clone();
            ty = &*ref_.elem;
        }
        has_attr_ty = Some(Cow::Borrowed(ty));
        if let syn::Type::Path(path) = ty {
            if path.qself.is_none() {
                if let Some(ident) = path.path.get_ident() {
                    if generics.type_params().any(|p| p.ident == *ident) {
                        container_is_generic = true;
                    }
                }
            }
        }
    }
    let has_attr_ty = has_attr_ty.unwrap_or_else(|| {
        container_is_generic = true;
        let mut has_attr_ty = String::from("T");
        while generics.type_params().any(|p| p.ident == has_attr_ty) {
            has_attr_ty.push('_');
        }
        let has_attr_ty = syn::Ident::new(&has_attr_ty, Span::mixed_site());
        Cow::Owned(syn::parse_quote_spanned! { Span::mixed_site() => #has_attr_ty })
    });

    if container_is_generic {
        generics
            .params
            .insert(0, syn::parse_quote! { #has_attr_ty });
    }

    if mode == Mode::Parse && lifetime.is_none() {
        let mut lt = String::from("t");
        while generics.lifetimes().any(|l| l.lifetime.ident == lt) {
            lt.push('_');
        }
        lifetime = Some(syn::Lifetime::new(&lt, Span::mixed_site()));
        generics.params.insert(0, syn::parse_quote! { #lifetime });
        let where_clause = generics.make_where_clause();
        where_clause.predicates.push(syn::parse_quote! {
            #has_attr_ty: #crate_::HasAttributes
        });
    }

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let matches = if attributes.is_empty() {
        quote_spanned! { Span::mixed_site() => true }
    } else {
        let matches = attributes.iter().map(|p| {
            let segs = p.segments.iter().map(|s| s.ident.to_string());
            quote_spanned! { Span::mixed_site() => &[#(#segs),*] }
        });
        quote_spanned! { Span::mixed_site() =>
            #(#priv_::parse_helpers::path_matches(path, #matches))||*
        }
    };

    let trait_ = match mode {
        Mode::Parse => quote_spanned! { Span::mixed_site() =>
            fn parse_attributes(obj: &#lifetime #has_attr_ty) -> crate::Result<Self>
        },
        Mode::Extract => quote_spanned! { Span::mixed_site() =>
            fn extract_attributes(obj: &mut #has_attr_ty) -> crate::Result<Self>
        },
    };
    let sig = match mode {
        Mode::Parse => quote_spanned! { Span::mixed_site() =>
            crate::ParseAttributes<#lifetime, #has_attr_ty>
        },
        Mode::Extract => quote_spanned! { Span::mixed_site() =>
            crate::ExtractAttributes<#has_attr_ty>
        },
    };
    let get_tokens = match mode {
        Mode::Parse => quote_spanned! { Span::mixed_site() => ref_tokens },
        Mode::Extract => quote_spanned! { Span::mixed_site() => take_tokens },
    };

    quote_spanned! { Span::mixed_site() =>
        impl #impl_generics #trait_ for #ident #type_generics #where_clause {
            #[inline]
            fn path_matches(path: &private::Path) -> private::bool {
                #matches
            }
            #sig {
                parse_helpers::parse_struct_attr_tokens(
                    parse_helpers::#get_tokens::<Self, _>(obj),
                    |inputs| {
                        #parse
                    }
                )
            }
        }
    }
}

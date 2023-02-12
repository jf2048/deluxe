use std::borrow::Cow;

use deluxe_core::Errors;
use proc_macro2::{Span, TokenStream};

use crate::types::*;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    Parse,
    Extract,
}

impl Mode {
    fn into_token_mode(self) -> TokenMode {
        match self {
            Self::Parse => TokenMode::ParseAttributes,
            Self::Extract => TokenMode::ExtractAttributes,
        }
    }
}

struct AttrImpl<'i> {
    pub parse: TokenStream,
    pub crate_path: syn::Path,
    pub priv_path: syn::Path,
    pub attributes: Vec<syn::Path>,
    pub container_field: Option<&'i syn::Field>,
    pub container_lifetime: Option<syn::Lifetime>,
    pub container_ty: Option<syn::Type>,
}

#[inline]
fn impl_for_struct<'i>(
    input: &'i syn::DeriveInput,
    struct_: &syn::DataStruct,
    mode: Mode,
    errors: &Errors,
) -> Option<AttrImpl<'i>> {
    let struct_attr = errors.push_result(<Struct as deluxe_core::ParseAttributes<
        syn::DeriveInput,
    >>::parse_attributes(input));

    let crate_path = super::get_crate_path(struct_attr.as_ref().map(|s| s.crate_.clone()), errors)?;
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;

    let parse = struct_attr
        .as_ref()
        .map(|s| {
            let ItemDef { inline, .. } = s.to_parsing_tokens(
                input,
                crate_,
                mode.into_token_mode(),
                &parse_quote_mixed! { inline(input) },
                &parse_quote_mixed! { allowed },
            );
            let pre = match &struct_.fields {
                syn::Fields::Named(_) => {
                    let field_names = struct_attr
                        .as_ref()
                        .map(|s| s.to_field_names_tokens(crate_, priv_))
                        .unwrap_or_else(|| quote_mixed! { &[] });
                    let accepts_all = struct_attr
                        .as_ref()
                        .and_then(|s| s.to_accepts_all_tokens(crate_))
                        .unwrap_or_else(|| quote_mixed! { false });
                    quote_mixed! {
                        let allowed = #field_names;
                        let validate = !(#accepts_all);
                        let prefix = "";
                    }
                }
                syn::Fields::Unnamed(_) => {
                    quote_mixed! {
                        let mut index = 0usize;
                    }
                }
                _ => quote::quote! {},
            };
            quote_mixed! {
                #pre
                #inline
            }
        })
        .unwrap_or_else(|| {
            quote_mixed! {
                #priv_::unreachable!()
            }
        });
    let (container_field, container_lifetime, container_ty) = struct_attr
        .as_ref()
        .map(|s| s.fields.as_slice())
        .unwrap_or_default()
        .iter()
        .find_map(|f| {
            f.container
                .as_ref()
                .map(|c| (Some(f.field), c.lifetime.clone(), c.ty.clone()))
        })
        .unwrap_or_default();
    Some(AttrImpl {
        parse,
        crate_path,
        priv_path,
        attributes: struct_attr.map(|s| s.attributes).unwrap_or_default(),
        container_field,
        container_lifetime,
        container_ty,
    })
}

#[inline]
fn impl_for_enum<'i>(
    input: &'i syn::DeriveInput,
    mode: Mode,
    errors: &Errors,
) -> Option<AttrImpl<'i>> {
    let enum_attr = errors.push_result(
        <Enum as deluxe_core::ParseAttributes<syn::DeriveInput>>::parse_attributes(input),
    );

    let crate_path = super::get_crate_path(enum_attr.as_ref().map(|e| e.crate_.clone()), errors)?;
    let crate_ = &crate_path;
    let priv_path: syn::Path = syn::parse_quote! { #crate_::____private };
    let priv_ = &priv_path;

    let parse = enum_attr
        .as_ref()
        .map(|e| {
            let parse = e.to_inline_parsing_tokens(crate_, mode.into_token_mode());
            let field_names = e.to_field_names_tokens(crate_, priv_);
            let accepts_all = e
                .to_accepts_all_tokens(crate_)
                .unwrap_or_else(|| quote_mixed! { false });
            quote_mixed! {
                let allowed = #field_names;
                let validate = !(#accepts_all);
                let prefix = "";
                #parse
            }
        })
        .unwrap_or_else(|| {
            quote_mixed! {
                #priv_::unreachable!()
            }
        });

    let (container_field, container_lifetime, container_ty) = enum_attr
        .as_ref()
        .map(|e| e.variants.as_slice())
        .unwrap_or_default()
        .iter()
        .find_map(|v| {
            v.fields.iter().find_map(|f| {
                f.container
                    .as_ref()
                    .map(|c| (Some(f.field), c.lifetime.clone(), c.ty.clone()))
            })
        })
        .unwrap_or_default();
    Some(AttrImpl {
        parse,
        crate_path,
        priv_path,
        attributes: enum_attr.map(|s| s.attributes).unwrap_or_default(),
        container_field,
        container_lifetime,
        container_ty,
    })
}

pub fn impl_parse_attributes(input: syn::DeriveInput, errors: &Errors, mode: Mode) -> TokenStream {
    let attr = match &input.data {
        syn::Data::Struct(struct_) => impl_for_struct(&input, struct_, mode, errors),
        syn::Data::Enum(_) => impl_for_enum(&input, mode, errors),
        syn::Data::Union(union_) => {
            errors.push_spanned(
                union_.union_token,
                match mode {
                    Mode::Parse => "union not supported with derive(ParseAttributes)",
                    Mode::Extract => "union not supported with derive(ExtractAttributes)",
                },
            );
            return Default::default();
        }
    };
    let AttrImpl {
        parse,
        crate_path: crate_,
        priv_path: priv_,
        attributes,
        container_field,
        mut container_lifetime,
        container_ty,
    } = match attr {
        Some(a) => a,
        None => return Default::default(),
    };

    let ident = &input.ident;
    let mut generics = input.generics.clone();

    let mut container_is_generic = false;
    let mut container_is_ref = false;
    let mut container_ty = container_ty.map(Cow::Owned);
    if let Some(container_field) = container_field {
        let mut ty = &container_field.ty;
        // try to guess some things about the container type.
        // first infer if this is an option, and if so, use its inner type
        if_chain::if_chain! {
            if let syn::Type::Path(path) = ty;
            if path.qself.is_none();
            if let Some(last) = path.path.segments.last();
            if last.ident == "Option";
            if let syn::PathArguments::AngleBracketed(args) = &last.arguments;
            if args.args.len() == 1;
            if let syn::GenericArgument::Type(t) = &args.args[0];
            then {
                ty = t;
            }
        }
        // use inner type and lifetime from reference
        if let syn::Type::Reference(ref_) = ty {
            if container_lifetime.is_none() {
                container_lifetime = ref_.lifetime.clone();
            }
            container_is_ref = true;
            ty = &*ref_.elem;
        }
        // if we still need a lifetime, and inner type has a lifetime, use that
        if_chain::if_chain! {
            if container_lifetime.is_none();
            if let syn::Type::Path(path) = ty;
            if let Some(last) = path.path.segments.last();
            if let syn::PathArguments::AngleBracketed(args) = &last.arguments;
            if let Some(syn::GenericArgument::Lifetime(lt)) = args.args.first();
            then {
                container_lifetime = Some(lt.clone());
            }
        }
        // see if the type matches a generic param
        if container_ty.is_none() {
            container_ty = Some(Cow::Borrowed(ty));
            if_chain::if_chain! {
                if let syn::Type::Path(path) = ty;
                if path.qself.is_none();
                if let Some(ident) = path.path.get_ident();
                if generics.type_params().any(|p| p.ident == *ident);
                then {
                    container_is_generic = true;
                }
            }
        }
    }
    // if there was no container field, make it generic and add our own type param
    let container_ty = container_ty.unwrap_or_else(|| {
        container_is_generic = true;
        let mut ty = String::from("T");
        // ensure the `T` is a unique ident
        while generics.type_params().any(|p| p.ident == ty) {
            ty.push('_');
        }
        let ty = syn::Ident::new(&ty, Span::mixed_site());
        generics.params.insert(0, syn::parse_quote! { #ty });
        Cow::Owned(parse_quote_mixed! { #ty })
    });

    // value types must be copied into the structure
    if container_field.is_some() && !container_is_ref {
        let where_clause = generics.make_where_clause();
        where_clause.predicates.push(syn::parse_quote! {
            #container_ty: #priv_::Clone
        });
    }

    // must be able to access attributes on the generic type
    if container_is_generic {
        let where_clause = generics.make_where_clause();
        where_clause.predicates.push(syn::parse_quote! {
            #container_ty: #crate_::HasAttributes
        });
    }

    // ParseAttributes needs a lifetime param since the attributes are always referenced
    if mode == Mode::Parse && container_lifetime.is_none() {
        let mut lt = String::from("t");
        while generics.lifetimes().any(|l| l.lifetime.ident == lt) {
            lt.push('_');
        }
        lt.insert(0, '\'');
        container_lifetime = Some(syn::Lifetime::new(&lt, Span::mixed_site()));
        generics
            .params
            .insert(0, syn::parse_quote! { #container_lifetime });
    }

    let (_, type_generics, _) = input.generics.split_for_impl();
    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let matches = if attributes.is_empty() {
        quote_mixed! { true }
    } else {
        let matches = attributes.iter().map(|p| {
            let segs = p.segments.iter().map(|s| s.ident.to_string());
            quote_mixed! { &[#(#segs),*] }
        });
        quote_mixed! {
            #(#priv_::parse_helpers::path_matches(path, #matches))||*
        }
    };

    let sig = match mode {
        Mode::Parse => quote_mixed! {
            fn parse_attributes(obj: &#container_lifetime #container_ty) -> #crate_::Result<Self>
        },
        Mode::Extract => quote_mixed! {
            fn extract_attributes(obj: &mut #container_ty) -> #crate_::Result<Self>
        },
    };
    let trait_ = match mode {
        Mode::Parse => quote_mixed! {
            #crate_::ParseAttributes<#container_lifetime, #container_ty>
        },
        Mode::Extract => quote_mixed! {
            #crate_::ExtractAttributes<#container_ty>
        },
    };
    let get_tokens = match mode {
        Mode::Parse => quote_mixed! { ref_tokens },
        Mode::Extract => quote_mixed! { take_tokens },
    };
    let tokens_try = match mode {
        Mode::Parse => None,
        Mode::Extract => Some(quote_mixed! { ? }),
    };
    let path_name_unwrap = attributes.first().map(|path| {
        let path = deluxe_core::parse_helpers::key_to_string(path);
        quote_mixed! { .or(#priv_::Option::Some(#path)) }
    });

    quote_mixed! {
        impl #impl_generics #trait_ for #ident #type_generics #where_clause {
            #[inline]
            fn path_matches(path: &#priv_::Path) -> #priv_::bool {
                #matches
            }
            #sig {
                #priv_::parse_helpers::parse_struct_attr_tokens(
                    #priv_::parse_helpers::#get_tokens::<Self, _>(obj) #tokens_try,
                    |inputs, spans| {
                        let span = #priv_::parse_helpers::first_span(spans);
                        let path_name = #priv_::parse_helpers::first_path_name(spans) #path_name_unwrap;
                        let _mode = #crate_::ParseMode::Named(span);
                        #parse
                    }
                )
            }
        }
    }
}

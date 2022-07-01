use deluxe_core::Errors;
use proc_macro2::TokenStream;
use syn::{ext::IdentExt, parse::ParseStream, Token};

bootstrap_parse! {
    struct A {
    }
}

struct Field {
    field: syn::Field,
    idents: Vec<syn::Ident>,
    default: Option<syn::Expr>,
    flatten: bool,
}

impl Field {
    fn parse(field: &syn::Field, attrs: &[syn::Attribute]) -> Result<Option<Self>, syn::Error> {
        let mut errors = Errors::new();
        let mut field = Field {
            field: field.clone(),
            idents: Vec::new(),
            default: None,
            flatten: false,
        };
        let mut skip = false;
        let mut rename_ = None;
        for attr in attrs {
            if attr.path.is_ident("deluxe") {
                if let Err(e) = syn::parse::Parser::parse2(
                    |stream: ParseStream<'_>| -> Result<(), syn::Error> {
                        let content;
                        syn::parenthesized!(content in stream);
                        syn::custom_keyword!(rename);
                        syn::custom_keyword!(alias);
                        syn::custom_keyword!(default);
                        syn::custom_keyword!(flatten);
                        syn::custom_keyword!(skip);
                        loop {
                            if content.is_empty() {
                                break;
                            }
                            let lookahead = content.lookahead1();
                            if lookahead.peek(rename) {
                                let token = content.parse::<rename>()?;
                                let lookahead = content.lookahead1();
                                let name = if lookahead.peek(Token![=]) {
                                    content.parse::<Token![=]>()?;
                                    syn::Ident::parse_any(&content)?
                                } else if lookahead.peek(syn::token::Paren) {
                                    let inside;
                                    syn::parenthesized!(inside in content);
                                    let name = syn::Ident::parse_any(&inside)?;
                                    inside.parse::<syn::parse::Nothing>()?;
                                    name
                                } else {
                                    return Err(lookahead.error());
                                };
                                if rename_.is_some() {
                                    errors.push(token.span, "Duplicate attribute `rename`");
                                } else {
                                    rename_ = Some(name);
                                }
                            } else if lookahead.peek(alias) {
                                let token = content.parse::<alias>()?;
                                content.parse::<Token![=]>()?;
                                if lookahead.peek(Token![=]) {
                                    content.parse::<Token![=]>()?;
                                    let name = syn::ext::IdentExt::parse_any(&content)?;
                                    field.idents.push(name);
                                } else if lookahead.peek(syn::token::Paren) {
                                    let inside;
                                    syn::parenthesized!(inside in content);
                                    loop {
                                        let name = syn::ext::IdentExt::parse_any(&inside)?;
                                        if field.idents.contains(&name) {
                                            errors.push_spanned(
                                                &name,
                                                format!("Duplicate alias for `{}`", name),
                                            );
                                        } else if field.field.ident.as_ref() == Some(&name) {
                                            errors.push_spanned(
                                                &name,
                                                format!("Unnecessary alias for `{}`", name),
                                            );
                                        } else {
                                            field.idents.push(name);
                                        }
                                        if inside.peek(Token![,]) {
                                            inside.parse::<Token![,]>()?;
                                        }
                                        if inside.is_empty() {
                                            break;
                                        }
                                    }
                                } else {
                                    return Err(lookahead.error());
                                };
                            } else {
                                return Err(lookahead.error());
                            }
                        }
                        Ok(())
                    },
                    attr.tokens.clone(),
                ) {
                    errors.push_syn(e);
                    errors.check()?;
                    unreachable!();
                }
            }
        }
        Ok(Some(field))
    }
}

pub fn impl_parse_meta_item(input: syn::DeriveInput, errors: &Errors) -> TokenStream {
    let crate_path = super::crate_path(errors);
    let crate_priv: syn::Path = syn::parse_quote! { #crate_path::____private };

    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    match &input.data {
        syn::Data::Struct(struct_) => {
            quote::quote! {
                impl #impl_generics #crate_path::FromTokens for #ident #type_generics #where_clause {
                    fn from_tokens(
                        input: #crate_priv::ParseStream<'_>,
                    ) -> #crate_priv::Result<Self, #crate_priv::Errors> {

                    }
                }
            }
        }
        syn::Data::Enum(enum_) => {
            quote::quote! {}
        }
        syn::Data::Union(union_) => {
            errors.push_spanned(
                union_.union_token,
                "Union not supported with derive(FromTokens)",
            );
            Default::default()
        }
    }
}

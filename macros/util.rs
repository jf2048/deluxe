use syn::parse::{Parse, Parser};

use deluxe_core::Errors;

#[inline]
pub fn parse<T: Parse>(input: proc_macro::TokenStream, errors: &Errors) -> Option<T> {
    errors.push_result(<T as Parse>::parse.parse(input))
}

fn crate_path(errors: Option<&Errors>) -> Option<syn::Path> {
    use proc_macro_crate::FoundCrate;
    const CRATE_NAME: &str = "deluxe";

    let crate_name = match proc_macro_crate::crate_name(CRATE_NAME) {
        Ok(FoundCrate::Name(name)) => name,
        Ok(FoundCrate::Itself) => CRATE_NAME.into(),
        Err(e) => {
            if let Some(errors) = errors {
                errors.push(proc_macro2::Span::call_site(), e.to_string());
            }
            return None;
        }
    };

    let ident = syn::Ident::new(&crate_name, proc_macro2::Span::call_site());
    Some(syn::parse_quote! { ::#ident })
}

#[inline]
pub fn get_crate_path(path: Option<Option<syn::Path>>, errors: &Errors) -> Option<syn::Path> {
    match path {
        Some(Some(path)) => Some(path),
        Some(None) => crate_path(Some(errors)),
        None => crate_path(None),
    }
}

macro_rules! quote_mixed {
    ($($tt:tt)*) => {
        quote::quote_spanned! { Span::mixed_site() => $($tt)* }
    };
}

macro_rules! parse_quote_mixed {
    ($($tt:tt)*) => {
        syn::parse_quote_spanned! { Span::mixed_site() => $($tt)* }
    };
}

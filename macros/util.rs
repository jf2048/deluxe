use proc_macro2::TokenStream;
use syn::parse::{Parse, Parser};

use deluxe_core::Errors;

pub fn parse<T: Parse>(input: TokenStream, errors: &Errors) -> Option<T> {
    match <T as Parse>::parse.parse2(input) {
        Ok(t) => Some(t),
        Err(e) => {
            errors.push_syn(e);
            None
        }
    }
}

#[inline]
pub fn crate_path(errors: &Errors) -> syn::Path {
    use proc_macro_crate::FoundCrate;
    const CRATE_NAME: &str = "deluxe";

    let crate_name = match proc_macro_crate::crate_name(CRATE_NAME) {
        Ok(FoundCrate::Name(name)) => name,
        Ok(FoundCrate::Itself) => CRATE_NAME.into(),
        Err(e) => {
            errors.push(proc_macro2::Span::call_site(), e.to_string());
            CRATE_NAME.into()
        }
    };

    let ident = syn::Ident::new(&crate_name, proc_macro2::Span::call_site());
    syn::parse_quote! { ::#ident }
}

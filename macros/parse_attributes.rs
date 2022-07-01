use proc_macro2::TokenStream;

use deluxe_core::Errors;

pub fn impl_parse_attributes(
    input: syn::DeriveInput,
    errors: &Errors,
    extract: bool,
) -> TokenStream {
    match &input.data {
        syn::Data::Struct(struct_) => {
            quote::quote! {}
        }
        syn::Data::Enum(enum_) => {
            quote::quote! {}
        }
        syn::Data::Union(union_) => {
            errors.push_spanned(
                union_.union_token,
                "Union not supported with derive(FromAttributes)",
            );
            Default::default()
        }
    }
}

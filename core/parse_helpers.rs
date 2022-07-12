use crate::{Errors, ParseMetaItem, ParseMode, Result};
use proc_macro2::{TokenStream, TokenTree};
pub use syn::token::{Brace, Bracket, Paren};
use syn::{
    ext::IdentExt,
    parse::{Nothing, ParseBuffer, ParseStream},
    Token,
};

mod sealed {
    pub trait Sealed {}
    impl Sealed for syn::token::Paren {}
    impl Sealed for syn::token::Brace {}
    impl Sealed for syn::token::Bracket {}
}

/// Helpers for parsing tokens inside a delimiter.
///
/// This trait is only implemeted for the `syn` delimiter token types.
pub trait ParseDelimited: sealed::Sealed {
    /// Parses a pair of delimiter tokens, and returns a [`syn::ParseBuffer`] for the tokens inside
    /// the delimiter.
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer>;
    /// Parse a [`ParseMetaItem`] surrounded by a delimiter. The inner group is allowed to contain
    /// a trailing comma.
    #[inline]
    fn parse_delimited_meta_item<T: ParseMetaItem>(
        input: ParseStream,
        mode: ParseMode,
    ) -> Result<T> {
        let content = Self::parse_delimited(input)?;
        let result = T::parse_meta_item_inline(&content, mode)?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(result)
    }
}

impl ParseDelimited for Paren {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::parenthesized!(content in input);
        Ok(content)
    }
}

impl ParseDelimited for Brace {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::braced!(content in input);
        Ok(content)
    }
}

impl ParseDelimited for Bracket {
    #[inline]
    fn parse_delimited(input: ParseStream) -> Result<ParseBuffer> {
        let content;
        syn::bracketed!(content in input);
        Ok(content)
    }
}

#[inline]
pub fn peek_eof_or_trailing_comma(input: ParseStream) -> bool {
    parse_eof_or_trailing_comma(&input.fork()).is_ok()
}

#[inline]
pub fn parse_eof_or_trailing_comma(input: ParseStream) -> Result<()> {
    if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
    }
    input.parse::<Nothing>()?;
    Ok(())
}

#[inline]
pub fn parse_named_meta_item<T: ParseMetaItem>(input: ParseStream) -> Result<T> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![=]) {
        input.parse::<Token![=]>()?;
        T::parse_meta_item(input, ParseMode::Named)
    } else if lookahead.peek(Paren) {
        Paren::parse_delimited_meta_item(input, ParseMode::Named)
    } else {
        Err(lookahead.error())
    }
}

#[inline]
pub fn parse_tuple_struct<F: FnMut(ParseStream, usize) -> Result<()>>(
    input: ParseStream,
    len: usize,
    mut func: F,
) -> Result<usize> {
    let mut counter = 0;
    loop {
        if input.is_empty() || counter >= len {
            return Ok(counter);
        }
        func(input, counter)?;
        counter += 1;
        if !input.is_empty() && counter < len {
            input.parse::<Token![,]>()?;
        }
    }
}

#[inline]
pub fn parse_struct<F: FnMut(ParseStream, syn::Ident) -> Result<()>>(
    input: ParseStream,
    mut func: F,
) -> Result<()> {
    loop {
        if input.is_empty() {
            break;
        }
        let ident = input.call(syn::Ident::parse_any)?;
        func(input, ident)?;
        if !input.is_empty() {
            input.parse::<Token![,]>()?;
        }
    }
    Ok(())
}

#[inline]
pub fn skip_named_meta_item(input: ParseStream) {
    input
        .step(|cursor| {
            let mut cur = *cursor;
            while let Some((tt, next)) = cur.token_tree() {
                cur = next;
                if let TokenTree::Punct(punct) = tt {
                    if punct.as_char() == ',' {
                        break;
                    }
                }
            }
            Ok(((), cur))
        })
        .unwrap();
}

#[inline]
pub fn parse_tokens<T, F: FnOnce(ParseStream) -> Result<T>>(
    input: TokenStream,
    func: F,
) -> Result<T> {
    let errors = Errors::new();
    let mut ret = None;
    syn::parse::Parser::parse2(
        |stream: ParseStream<'_>| {
            ret = match func(stream) {
                Ok(ret) => Some(ret),
                Err(err) => {
                    errors.push_syn(err);
                    None
                }
            };
            if let Err(err) = parse_eof_or_trailing_comma(stream) {
                errors.push_syn(err);
            }
            Ok(())
        },
        input,
    )?;
    errors.check()?;
    Ok(ret.unwrap())
}

#[inline]
pub fn parse_struct_attr_tokens<T, F: FnOnce(ParseStream) -> Result<T>>(
    input: TokenStream,
    func: F,
) -> Result<T> {
    parse_tokens(input, |stream| {
        let content = Paren::parse_delimited(stream)?;
        let res = func(&content)?;
        parse_eof_or_trailing_comma(&content)?;
        Ok(res)
    })
}

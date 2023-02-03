//! Additional helper functions for validating after parsing.

use crate::Errors;

/// Appends an error if more than one given attribute is present.
///
/// `attrs` should provide an iterator of tuples containing the field name, and an [`Option`]
/// possibly containing the field value. If two or more are [`Some`], an error will be appended to
/// `errors`, with `prefix` prepended onto the names.
pub fn only_one<'t, I>(attrs: I, prefix: &str, errors: &Errors)
where
    I: IntoIterator<Item = &'t (&'static str, Option<&'t dyn syn::spanned::Spanned>)>,
    I::IntoIter: Clone,
{
    let iter = attrs.into_iter();
    let present_spans = iter.clone().filter_map(|f| f.1);
    if present_spans.clone().take(2).count() == 2 {
        let mut names = String::new();
        for (n, _) in iter {
            use std::fmt::Write;
            let n = crate::parse_helpers::join_path(prefix, n);
            if names.is_empty() {
                write!(names, "`{n}`").unwrap();
            } else {
                write!(names, ", `{n}`").unwrap();
            }
        }
        for span in present_spans {
            errors.push(span.span(), format!("only one of {names} is allowed"));
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! _only_one_arg {
    (($name:expr, $value:expr)) => {
        &(
            $name,
            $value.map(|s| s as &dyn $crate::syn::spanned::Spanned),
        )
    };
    ($field:ident) => {
        &(
            std::stringify!($field),
            $field
                .as_ref()
                .map(|s| s as &dyn $crate::syn::spanned::Spanned),
        )
    };
}

/// Convenience macro for [`only_one`](fn@only_one).
///
/// Automatically casts each field to `&dyn Spanned`. Can also take fields as a single ident if the
/// variable name is the same as the field name.
///
/// # Example
///
/// ```
/// let errors = deluxe_core::Errors::new();
/// let my_bool: Option<syn::LitBool> = None;
/// let path: Option<syn::Path> = None;
/// let field3: Option<syn::Type> = None;
/// // ... parsing omitted ...
/// // field names are `my_bool`, `path`, `ty`
/// deluxe_core::only_one!("", &errors, my_bool, path, ("ty", field3.as_ref()));
/// ```
#[macro_export]
macro_rules! only_one {
    ($prefix:expr, $errors:expr $(, $fields:tt)* $(,)?) => {
        $crate::validations::only_one([$($crate::_only_one_arg!($fields)),*], $prefix, $errors)
    };
}

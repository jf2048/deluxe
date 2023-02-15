//! Additional helper functions for validating after parsing.

use crate::Errors;

/// Appends an error if more than one given attribute is present.
///
/// `attrs` should provide an iterator of tuples containing the field name, and an [`Option`]
/// possibly containing the field value. If two or more are [`Some`], an error will be appended to
/// `errors`, with `prefix` prepended onto the names.
pub fn only_one<'t, I, O>(attrs: I, prefix: &str, errors: &Errors)
where
    I: IntoIterator<Item = &'t (&'static str, O)>,
    I::IntoIter: Clone,
    O: Into<Option<&'t dyn syn::spanned::Spanned>> + Clone + ?Sized + 't,
{
    let iter = attrs.into_iter();
    let present_spans = iter.clone().filter_map(|f| f.1.clone().into());
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

/// Appends an error if some attributes are present, but not all of them.
///
/// `attrs` should provide an iterator of tuples containing the field name, and an [`Option`]
/// possibly containing the field value. If at least one is [`Some`] and at least one is [`None`],
/// an error will be appended to `errors`, with `prefix` prepended onto the names.
pub fn all_or_none<'t, I, O>(attrs: I, prefix: &str, errors: &Errors)
where
    I: IntoIterator<Item = &'t (&'static str, O)>,
    I::IntoIter: Clone,
    O: Into<Option<&'t dyn syn::spanned::Spanned>> + Clone + ?Sized + 't,
{
    let iter = attrs.into_iter();
    let mut search = iter.clone().cloned().map(|f| f.1.into());
    let first_is_some = search.next().map(|f| f.is_some()).unwrap_or(false);
    if search.any(|f| f.is_some() != first_is_some) {
        let mut names = String::new();
        for (n, o) in iter.clone().cloned() {
            if o.into().is_none() {
                use std::fmt::Write;
                let n = crate::parse_helpers::join_path(prefix, n);
                if names.is_empty() {
                    write!(names, "`{n}`").unwrap();
                } else {
                    write!(names, ", `{n}`").unwrap();
                }
            }
        }
        for (n, o) in iter.cloned() {
            if let Some(o) = o.into() {
                errors.push(
                    o.span(),
                    format!("{names} must also be set in order to use `{n}`"),
                );
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! _spanned_arg {
    (($name:expr, $value:expr)) => {
        &(
            $name,
            $value.map(|s| s as &dyn $crate::syn::spanned::Spanned),
        )
    };
    ($field:ident) => {
        &(
            $crate::stringify!($field),
            $field
                .as_ref()
                .map(|s| s as &dyn $crate::syn::spanned::Spanned),
        )
    };
}

/// Appends an error if more than one given attribute is present, automatically casting and
/// stringifying field names.
///
/// Convenience macro for [`only_one`](fn@only_one). The first argument is a <code>&[str]</code>
/// prefix to prepend onto the names in the event of an error. The second argument is an
/// <code>&[Errors]</code> to add any errors into.
///
/// The rest of the arguments are either identifiers or 2-element tuples. Identifiers must be an
/// `Option<T>` and tuples must be `(&str, Option<&T>)`, where `&T` is castable to <code>&dyn
/// [syn::spanned::Spanned]</code>. Identifiers will automatically [`stringify`] the identifier
/// name to use in the error message, and so should used when the field name is the same as the
/// variable name. The tuple form can be used to specify a different field name from the name of
/// the variable. The identifier form is intended to make it easier to do validations by
/// destructuring a data type after it has been parsed.
///
/// # Example
///
/// ```
/// #[derive(Default)]
/// struct MyStruct {
///     my_bool: Option<syn::LitBool>,
///     path: Option<syn::Path>,
///     // #[deluxe(rename = "ty")]
///     field3: Option<syn::Type>,
/// }
/// let errors = deluxe_core::Errors::new();
/// let s = MyStruct::default();
///
/// // ... parsing omitted ...
///
/// let MyStruct { my_bool, path, field3 } = &s;
/// // only one of `my_bool`, `path`, `ty` is allowed
/// deluxe_core::only_one!("", &errors, my_bool, path, ("ty", field3.as_ref()));
/// # assert!(errors.is_empty());
/// ```
#[macro_export]
macro_rules! only_one {
    ($prefix:expr, $errors:expr $(, $fields:tt)* $(,)?) => {
        $crate::validations::only_one([$($crate::_spanned_arg!($fields)),*], $prefix, $errors)
    };
}

/// Appends an error if some attributes are present, but not all of them, automatically casting and
/// stringifying field names.
///
/// Convenience macro for [`all_or_none`](fn@all_or_none). The first argument is a
/// <code>&[str]</code> prefix to prepend onto the names in the event of an error. The second
/// argument is an <code>&[Errors]</code> to add any errors into.
///
/// The rest of the arguments are either identifiers or 2-element tuples. Identifiers must be an
/// `Option<T>` and tuples must be `(&str, Option<&T>)`, where `&T` is castable to <code>&dyn
/// [syn::spanned::Spanned]</code>. Identifiers will automatically [`stringify`] the identifier
/// name to use in the error message, and so should used when the field name is the same as the
/// variable name. The tuple form can be used to specify a different field name from the name of
/// the variable. The identifier form is intended to make it easier to do validations by
/// destructuring a data type after it has been parsed.
///
/// # Example
///
/// ```
/// #[derive(Default)]
/// struct MyStruct {
///     my_bool: Option<syn::LitBool>,
///     path: Option<syn::Path>,
///     // #[deluxe(rename = "ty")]
///     field3: Option<syn::Type>,
/// }
/// let errors = deluxe_core::Errors::new();
/// let s = MyStruct::default();
///
/// // ... parsing omitted ...
///
/// let MyStruct { my_bool, path, field3 } = &s;
/// // if any of `my_bool`, `path`, `ty` is present, then all must be present
/// deluxe_core::all_or_none!("", &errors, my_bool, path, ("ty", field3.as_ref()));
/// # assert!(errors.is_empty());
/// ```
#[macro_export]
macro_rules! all_or_none {
    ($prefix:expr, $errors:expr $(, $fields:tt)* $(,)?) => {
        $crate::validations::all_or_none([$($crate::_spanned_arg!($fields)),*], $prefix, $errors)
    };
}

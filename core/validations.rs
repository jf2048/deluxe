use syn::spanned::Spanned;

use crate::Errors;

pub fn only_one<'t, I>(attrs: I, prefix: &str, errors: &Errors)
where
    I: IntoIterator<Item = &'t (&'static str, Option<&'t dyn Spanned>)>,
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
                write!(names, "`{}`", n).unwrap();
            } else {
                write!(names, ", `{}`", n).unwrap();
            }
        }
        for span in present_spans {
            errors.push(span.span(), format!("only one of {} is allowed", names));
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! _only_one_arg {
    (($name:expr, $value:expr)) => {
        &($name, $value.map(|s| s as &dyn Spanned))
    };
    ($field:ident) => {
        &(
            std::stringify!($field),
            $field.as_ref().map(|s| s as &dyn Spanned),
        )
    };
}

#[macro_export]
macro_rules! only_one {
    ($prefix:expr, $errors:expr $(, $fields:tt)* $(,)?) => {
        $crate::validations::only_one([$($crate::_only_one_arg!($fields)),*], $prefix, $errors)
    };
}

use ::std::prelude::v1::*;

pub trait SynErrorExt {
    fn to_multi_string(&self) -> String;
}

impl SynErrorExt for ::syn::Error {
    fn to_multi_string(&self) -> String {
        self.into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }
}

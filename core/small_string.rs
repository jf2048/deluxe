/// A string type that can be stored on the stack or on the heap, and can also represent borrowed
/// strings.
pub struct SmallString<'a>(SmallStringInner<'a>);

const MAX_INLINE_LEN: usize = 56;

enum SmallStringInner<'a> {
    Heap(String),
    Inline(arrayvec::ArrayString<MAX_INLINE_LEN>),
    Borrowed(&'a str),
}

impl<'a> SmallString<'a> {
    /// Creates a new empty `SmallString`.
    ///
    /// Does not allocate.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self(SmallStringInner::Borrowed(""))
    }
    /// Converts this `SmallString` to `SmallString<'static>`.
    #[inline]
    #[must_use]
    pub fn into_owned(self) -> SmallString<'static> {
        use SmallStringInner::*;
        SmallString(match self.0 {
            Heap(s) => Heap(s),
            Inline(s) => Inline(s),
            Borrowed(s) => s.try_into().map(Inline).unwrap_or_else(|_| Heap(s.into())),
        })
    }
    /// Extracts a string slice containing the entire `SmallString`.
    #[inline]
    #[must_use]
    pub fn as_str(&self) -> &str {
        use SmallStringInner::*;
        match &self.0 {
            Heap(s) => s.as_str(),
            Inline(s) => s.as_str(),
            Borrowed(s) => s,
        }
    }
    /// Appends the given [`char`] to the end of this `SmallString`.
    #[inline]
    pub fn push(&mut self, c: char) {
        self.push_str(c.encode_utf8(&mut [0; 4]));
    }
    /// Appends a given string slice onto the end of this `SmallString`.
    pub fn push_str(&mut self, s: &str) {
        use SmallStringInner::*;
        match &mut self.0 {
            Heap(h) => h.push_str(s),
            Inline(i) => {
                if i.try_push_str(s).is_err() {
                    let mut v = String::from(i.as_str());
                    v.push_str(s);
                    *self = Self(Heap(v));
                }
            }
            Borrowed(b) => {
                if b.len().checked_add(s.len()).expect("integer overflow") > MAX_INLINE_LEN {
                    let mut v = String::from(*b);
                    v.push_str(s);
                    *self = Self(Heap(v));
                } else {
                    let mut v = arrayvec::ArrayString::from(b).unwrap();
                    v.push_str(s);
                    *self = Self(Inline(v));
                }
            }
        }
    }
}

impl<'a> Eq for SmallString<'a> {}

impl<'a> PartialEq for SmallString<'a> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl<'a> PartialEq<str> for SmallString<'a> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq(other)
    }
}

impl<'a> PartialEq<SmallString<'a>> for str {
    #[inline]
    fn eq(&self, other: &SmallString<'a>) -> bool {
        self.eq(other.as_str())
    }
}

impl<'a> Ord for SmallString<'a> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<'a> PartialOrd for SmallString<'a> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl<'a> PartialOrd<str> for SmallString<'a> {
    #[inline]
    fn partial_cmp(&self, other: &str) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other)
    }
}

impl<'a> PartialOrd<SmallString<'a>> for str {
    #[inline]
    fn partial_cmp(&self, other: &SmallString<'a>) -> Option<std::cmp::Ordering> {
        self.partial_cmp(other.as_str())
    }
}

impl<'a> std::hash::Hash for SmallString<'a> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl<'a> Default for SmallString<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> std::fmt::Debug for SmallString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SmallStringInner::*;
        match &self.0 {
            Heap(s) => s.fmt(f),
            Inline(s) => s.fmt(f),
            Borrowed(s) => s.fmt(f),
        }
    }
}

impl<'a> std::fmt::Display for SmallString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SmallStringInner::*;
        match &self.0 {
            Heap(s) => s.fmt(f),
            Inline(s) => s.fmt(f),
            Borrowed(s) => s.fmt(f),
        }
    }
}

impl<'a> From<&'a str> for SmallString<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self(SmallStringInner::Borrowed(value))
    }
}

impl<'a> From<String> for SmallString<'a> {
    #[inline]
    fn from(value: String) -> Self {
        if value.len() > MAX_INLINE_LEN {
            Self(SmallStringInner::Heap(value))
        } else {
            Self(SmallStringInner::Inline(
                arrayvec::ArrayString::from(value.as_str()).unwrap(),
            ))
        }
    }
}

impl<'a> From<std::borrow::Cow<'a, str>> for SmallString<'a> {
    #[inline]
    fn from(value: std::borrow::Cow<'a, str>) -> Self {
        match value {
            std::borrow::Cow::Borrowed(b) => b.into(),
            std::borrow::Cow::Owned(o) => o.into(),
        }
    }
}

impl<'a> std::ops::Deref for SmallString<'a> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<'a> AsRef<str> for SmallString<'a> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> AsRef<[u8]> for SmallString<'a> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl<'a> std::borrow::Borrow<str> for SmallString<'a> {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl<'a> std::fmt::Write for SmallString<'a> {
    #[inline]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }
    #[inline]
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.push(c);
        Ok(())
    }
}

impl<'a> quote::ToTokens for SmallString<'a> {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.as_str().to_tokens(tokens);
    }
}

impl<'a> crate::ParseMetaItem for SmallString<'a> {
    #[inline]
    fn parse_meta_item(
        input: syn::parse::ParseStream,
        _mode: crate::ParseMode,
    ) -> crate::Result<Self> {
        Ok(crate::parse_helpers::key_to_string(
            &input.parse::<syn::LitStr>()?.token(),
        ))
    }
}

impl<'a> crate::ToKeyString for SmallString<'a> {
    #[inline]
    fn fmt_key_string(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
    #[inline]
    fn with_key_string<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        f(self)
    }
}

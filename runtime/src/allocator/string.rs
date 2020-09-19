use super::constants::DATA_OFFSET;
use super::fixed_array::FixedArrayPtr;
use std::ops::Deref;

/// A managed thin pointer to a string. Interned strings are also tagged
/// despite not being managed so this can always be used
///
/// Tag | Size (LE 32) | str (utf-8)
///
/// new safety: must be utf-8!
type StringPtr = FixedArrayPtr<u8>;

/// gain all the methods of string slices
impl Deref for StringPtr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        // SAFETY: [StringPtr::new] being unsafe guarantees that we must be
        // well-formed
        unsafe {
            let ptr = self.ptr.add(DATA_OFFSET + 1) as *const u8;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, self.len()))
        }
    }
}
/// mostly just for convenience in a few circumstances where deeper thought
/// would allow us to skip this
impl From<&str> for StringPtr {
    fn from(s: &str) -> Self {
        crate::heap().alloc_str_or_gc(s)
    }
}
impl std::fmt::Debug for StringPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &**self)
    }
}
impl std::fmt::Display for StringPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &**self)
    }
}
impl PartialEq for StringPtr {
    fn eq(&self, other: &StringPtr) -> bool {
        if self.ptr == other.ptr {
            true
        } else {
            **self == **other
        }
    }
}
impl Eq for StringPtr {}
impl std::hash::Hash for StringPtr {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        (**self).hash(h)
    }
}

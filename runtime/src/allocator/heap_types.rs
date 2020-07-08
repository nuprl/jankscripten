pub use super::object_ptr::{ObjectDataPtr, ObjectPtr};
use super::TypePtr;
use super::{Tag, TypeTag};
use crate::{AnyValue, Key};
use std::collections::HashMap;

pub trait HasTag {
    const TYPE_TAG: TypeTag;
    fn get_tag() -> Tag {
        Tag::with_type(Self::TYPE_TAG)
    }
}

pub type I32Ptr<'a> = TypePtr<'a, i32>;
impl HasTag for i32 {
    const TYPE_TAG: TypeTag = TypeTag::I32;
}
pub type F64Ptr<'a> = TypePtr<'a, f64>;
impl HasTag for f64 {
    const TYPE_TAG: TypeTag = TypeTag::F64;
}
pub type StringPtr<'a> = TypePtr<'a, String>;
impl HasTag for String {
    const TYPE_TAG: TypeTag = TypeTag::String;
}
pub type AnyJSPtr<'a> = TypePtr<'a, AnyValue<'a>>;
impl<'a> HasTag for AnyValue<'a> {
    const TYPE_TAG: TypeTag = TypeTag::Any;
}

pub type HTPtr<'a, V> = TypePtr<'a, HashMap<Key, V>>;
impl<'a> HasTag for HashMap<Key, AnyValue<'a>> {
    const TYPE_TAG: TypeTag = TypeTag::HTAny;
}
impl HasTag for HashMap<Key, i32> {
    const TYPE_TAG: TypeTag = TypeTag::HTI32;
}
impl HasTag for HashMap<Key, f64> {
    const TYPE_TAG: TypeTag = TypeTag::HTF64;
}

pub type ArrayPtr<'a, V> = TypePtr<'a, Vec<V>>;
impl<'a> HasTag for Vec<AnyValue<'a>> {
    const TYPE_TAG: TypeTag = TypeTag::ArrayAny;
}
impl HasTag for Vec<i32> {
    const TYPE_TAG: TypeTag = TypeTag::ArrayI32;
}

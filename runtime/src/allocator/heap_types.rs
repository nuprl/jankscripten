pub use super::env::EnvPtr;
pub use super::object_ptr::{ObjectDataPtr, ObjectPtr};
pub use super::string::StringPtr;
use super::{AnyPtr, HeapPtr, Tag, TypePtr, TypeTag};
use crate::{AnyEnum, AnyValue, Heap, Key};
use std::collections::HashMap;

pub trait HasTag {
    const TYPE_TAG: TypeTag;
    fn get_tag() -> Tag {
        Tag::with_type(Self::TYPE_TAG)
    }
    fn get_data_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        vec![]
    }
}

impl HasTag for ObjectDataPtr {
    /// to be clear, this means the type tag OF THE POINTER
    const TYPE_TAG: TypeTag = TypeTag::ObjectPtrPtr;
}

pub type HTPtr = TypePtr<HashMap<Key, AnyValue>>;
impl HasTag for HashMap<Key, AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::HT;
    fn get_data_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        let mut branches = vec![];
        for any in self.values() {
            branches.extend(any.get_data_ptrs(heap));
        }
        branches
    }
}

pub type ArrayPtr = TypePtr<Vec<AnyValue>>;
impl HasTag for Vec<AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::Array;
    fn get_data_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        let mut branches = vec![];
        for any in self {
            branches.extend(any.get_data_ptrs(heap));
        }
        branches
    }
}

// REF TYPES
// =========

pub type NonPtr32Ptr = TypePtr<i32>;
impl HasTag for i32 {
    const TYPE_TAG: TypeTag = TypeTag::NonPtr32;
}
pub type AnyJSPtr = TypePtr<AnyValue>;
impl HasTag for AnyValue {
    const TYPE_TAG: TypeTag = TypeTag::Any;
    fn get_data_ptrs(&self, _: &Heap) -> Vec<*mut Tag> {
        if let Some(p) = (**self).get_ptr() {
            vec![p]
        } else {
            vec![]
        }
    }
}
pub type MutF64Ptr = TypePtr<f64>;
impl HasTag for f64 {
    const TYPE_TAG: TypeTag = TypeTag::Ptr;
}
pub type PtrPtr = TypePtr<AnyPtr>;
impl HasTag for AnyPtr {
    const TYPE_TAG: TypeTag = TypeTag::Ptr;
    fn get_data_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        vec![self.get_ptr()]
    }
}

impl AnyEnum {
    pub fn get_ptr(&self) -> Option<*mut Tag> {
        match *self {
            Self::Ptr(ptr) => Some(ptr.get_ptr()),
            Self::F64(ptr) => Some(ptr as *mut Tag),
            _ => None,
        }
    }
}

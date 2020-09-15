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
    fn get_data_f64s(&mut self, _heap: &Heap) -> Vec<*mut *const f64> {
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
    fn get_data_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        let mut branches = vec![];
        for any in self.values_mut() {
            branches.extend(any.get_data_f64s(heap));
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
    fn get_data_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        let mut branches = vec![];
        for any in self {
            branches.extend(any.get_data_f64s(heap));
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
    fn get_data_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        (**self).get_data_ptrs(heap)
    }
    fn get_data_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        (**self).get_data_f64s(heap)
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
    pub fn get_data_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        match self {
            Self::Ptr(ptr) => vec![ptr.get_ptr()],
            _ => vec![],
        }
    }
    pub fn get_data_f64s(&mut self, _heap: &Heap) -> Vec<*mut *const f64> {
        match self {
            Self::F64(ptr) => vec![&mut *ptr],
            _ => vec![],
        }
    }
}

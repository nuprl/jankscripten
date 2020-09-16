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
    fn get_gc_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        vec![]
    }
    fn get_gc_f64s(&mut self, _heap: &Heap) -> Vec<*mut *const f64> {
        vec![]
    }
}

pub type I32Ptr = TypePtr<i32>;
impl HasTag for i32 {
    const TYPE_TAG: TypeTag = TypeTag::I32;
}
pub type AnyJSPtr = TypePtr<AnyValue>;
impl HasTag for AnyValue {
    const TYPE_TAG: TypeTag = TypeTag::Any;
    fn get_gc_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        (**self).get_gc_ptrs(heap)
    }
    fn get_gc_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        (**self).get_gc_f64s(heap)
    }
}
//pub type PtrPtr = TypePtr<AnyPtr>;
//impl HasTag for AnyPtr {
//    const TYPE_TAG: TypeTag = TypeTag::Ptr;
//}

impl HasTag for ObjectDataPtr {
    /// to be clear, this means the type tag OF THE POINTER
    const TYPE_TAG: TypeTag = TypeTag::ObjectPtrPtr;
}

pub type HTPtr = TypePtr<HashMap<Key, AnyValue>>;
impl HasTag for HashMap<Key, AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::HT;
    fn get_gc_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        let mut branches = vec![];
        for any in self.values() {
            branches.extend(any.get_gc_ptrs(heap));
        }
        branches
    }
    fn get_gc_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        let mut branches = vec![];
        for any in self.values_mut() {
            branches.extend(any.get_gc_f64s(heap));
        }
        branches
    }
}

pub type ArrayPtr = TypePtr<Vec<AnyValue>>;
impl HasTag for Vec<AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::Array;
    fn get_gc_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        let mut branches = vec![];
        for any in self {
            branches.extend(any.get_gc_ptrs(heap));
        }
        branches
    }
    fn get_gc_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        let mut branches = vec![];
        for any in self {
            branches.extend(any.get_gc_f64s(heap));
        }
        branches
    }
}

impl AnyEnum {
    pub fn get_gc_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        match self {
            Self::Ptr(ptr) => vec![ptr.get_ptr()],
            _ => vec![],
        }
    }
    pub fn get_gc_f64s(&mut self, _heap: &Heap) -> Vec<*mut *const f64> {
        match self {
            Self::F64(ptr) => vec![&mut *ptr],
            _ => vec![],
        }
    }
}

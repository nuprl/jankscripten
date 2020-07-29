pub use super::object_ptr::{ObjectDataPtr, ObjectPtr};
use super::HeapPtr;
use super::TypePtr;
use super::{Tag, TypeTag};
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
    fn get_gc_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        (**self).get_gc_ptrs(heap)
    }
    fn get_gc_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        (**self).get_gc_f64s(heap)
    }
}

impl<'a> HasTag for ObjectDataPtr<'a> {
    /// to be clear, this means the type tag OF THE POINTER
    const TYPE_TAG: TypeTag = TypeTag::ObjectPtrPtr;
}

pub type HTPtr<'a> = TypePtr<'a, HashMap<Key, AnyValue<'a>>>;
impl<'a> HasTag for HashMap<Key, AnyValue<'a>> {
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

pub type ArrayPtr<'a> = TypePtr<'a, Vec<AnyValue<'a>>>;
impl<'a> HasTag for Vec<AnyValue<'a>> {
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

impl AnyEnum<'_> {
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

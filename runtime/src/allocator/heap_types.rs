pub use super::env::EnvPtr;
pub use super::object_ptr::{ObjectDataPtr, ObjectPtr};
pub use super::string::StringPtr;
use super::{AnyPtr, HeapPtr, Tag, TypePtr, TypeTag};
use crate::closure::Closure;
use crate::{AnyEnum, AnyValue, Heap, Key};
use std::collections::HashMap;

pub trait HasTag {
    const TYPE_TAG: TypeTag;
    // never needed to supply
    fn get_tag() -> Tag {
        Tag::with_type(Self::TYPE_TAG)
    }
    // must supply if might have pointers!!
    fn get_data_ptrs(&self, _heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        (vec![], vec![])
    }
}

impl HasTag for ObjectDataPtr {
    /// to be clear, this means the type tag OF THE POINTER
    const TYPE_TAG: TypeTag = TypeTag::ObjectPtrPtr;
}

pub type HTPtr = TypePtr<HashMap<Key, AnyValue>>;
impl HasTag for HashMap<Key, AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::HT;
    fn get_data_ptrs(&self, _: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        AnyEnum::iter_to_ptrs(self.values().map(|x| &**x))
    }
}

pub type ArrayPtr = TypePtr<Vec<AnyValue>>;
impl HasTag for Vec<AnyValue> {
    const TYPE_TAG: TypeTag = TypeTag::Array;
    fn get_data_ptrs(&self, _: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        AnyEnum::iter_to_ptrs(self.iter().map(|x| &**x))
    }
}

// REF TYPES
// =========
// TODO(luna): Is this necessary? Aren't the immediate values, rather than the
// ref, placed in the shadow stack? Why would a ref ever appear in the
// heap? Answer: If it was in an env. But, shouldn't the env be responsible for
// dereferencing the ref before giving the pointers to the mark phase?

pub type NonPtr32Ptr = TypePtr<i32>;
impl HasTag for i32 {
    const TYPE_TAG: TypeTag = TypeTag::NonPtr32;
}
pub type AnyJSPtr = TypePtr<AnyValue>;
impl HasTag for AnyValue {
    const TYPE_TAG: TypeTag = TypeTag::Any;
    fn get_data_ptrs(&self, _: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        AnyEnum::iter_to_ptrs(std::iter::once(&**self))
    }
}
pub type MutF64Ptr = TypePtr<f64>;
impl HasTag for f64 {
    // TODO(luna): is this right?? What happened to MutF64?
    const TYPE_TAG: TypeTag = TypeTag::Ptr;
}
pub type PtrPtr = TypePtr<AnyPtr>;
impl HasTag for AnyPtr {
    const TYPE_TAG: TypeTag = TypeTag::Ptr;
    fn get_data_ptrs(&self, _heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        (vec![self.get_ptr()], vec![])
    }
}
pub type ClosurePtr = TypePtr<Closure>;
impl HasTag for Closure {
    const TYPE_TAG: TypeTag = TypeTag::Closure;
    fn get_data_ptrs(&self, _heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        let env = self.0;
        (vec![env.get_ptr()], vec![])
    }
}

impl AnyEnum {
    pub fn insert_ptr(&self, tags: &mut Vec<*mut Tag>, f64s: &mut Vec<*mut *const f64>) {
        match self {
            Self::Ptr(ptr) => tags.push(ptr.get_ptr()),
            Self::Closure(clos) => {
                let env = clos.0;
                tags.push(env.get_ptr());
            }
            Self::F64(ptr) => f64s.push(ptr as *const *const f64 as *mut _),
            _ => (),
        }
    }
    pub fn iter_to_ptrs<'a, T: Iterator<Item = &'a Self>>(
        i: T,
    ) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        let size = i.size_hint().1.unwrap_or(0);
        let mut a = Vec::with_capacity(size);
        let mut b = Vec::with_capacity(size);
        for any in i {
            any.insert_ptr(&mut a, &mut b);
        }
        (a, b)
    }
}

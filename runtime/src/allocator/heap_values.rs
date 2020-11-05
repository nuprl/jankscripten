//! Values on the managed heap.
//!

use super::constants::*;
use super::heap_types::*;
use super::layout;
use super::*;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// The first machine-word of every heap value is a `Tag`, and every pointer
/// into the heap must point to a `Tag`. In other words, we do *not* support
/// interior pointers.
#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(C)]
pub struct Tag {
    pub marked: bool,
    pub type_tag: TypeTag,
    /// The `class_tag` is only meaningful if the `type_tag == TypeTag::DynObject`.
    pub class_tag: u16,
}

impl Tag {
    pub fn with_type(type_tag: TypeTag) -> Self {
        Tag {
            marked: false,
            type_tag,
            class_tag: 0,
        }
    }

    pub fn object(class_tag: u16) -> Self {
        Tag {
            marked: false,
            type_tag: TypeTag::DynObject,
            class_tag,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum TypeTag {
    /// this is any 32-bit non-pointer (i32, bool, fn)
    ///
    /// this is a fairly healthy choice for 0, which apparently needs to
    /// exist or we get messy errors i don't even want to think about
    NonPtr32,
    /// We specify a value so we can make fake tags from jankscripten
    String = 1,
    HT,
    Array,
    DynObject,
    /// The value after the tag is the address where the array of pointers
    /// begins, for this object
    ObjectPtrPtr,
    Env,
    /// following are immediate values only ever on the heap for Ref
    Any,
    /// these should only be used for Ref, most f64s go on the f64 heap. this
    /// avoids another layer of indirection we just put a f64 immediately
    /// following the tag. that f64 might be modified
    MutF64,
    /// this may or may not be duplicated by ObjectPtrPtr
    Ptr,
}

/// Every pointer into the heap points to a tag, thus we could build an API
/// where every function consumes and produces `*mut Tag`-typed values. However,
/// it would be really easy to make mistakes. Instead, our API has a Rust type
/// for each variant of `Tag`. Moreover, each Rust type implements the
/// `HeapPtr` trait, to get low-level access to the heap, which is needed to
/// actually manipulate heap values.
pub trait HeapPtr {
    fn get_ptr(&self) -> *mut Tag;
    /// The size of the data that follows the tag. i.e., this must exclude the
    /// size of the tag.
    fn get_data_size(&self, heap: &Heap) -> usize;
    /// this is not Drop::drop, don't call it unless you're the GC
    ///
    /// drops any data on the unmanaged heap. default implementation does
    /// nothing, because most structures do not allocate on the
    /// rust heap
    fn final_drop(&self) {}
    /// this should return all the pointers to garbage collected Tags in
    /// the managed heap in the data structure, including f64s
    fn get_gc_ptrs(&self, _heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        (vec![], vec![])
    }
    /// easily cast any heap pointer to an any pointer (provided)
    fn as_any_ptr(&self) -> AnyPtr {
        unsafe { AnyPtr::new(self.get_ptr()) }
    }
}

/// Returns a pointer to the data that follows the tag.
pub unsafe fn data_ptr<T>(tag_ptr: *mut Tag) -> *mut T {
    return std::mem::transmute(tag_ptr.add(DATA_OFFSET));
}

/// A pointer to an arbitrary `Tag`. We use `repr(transparent)` as an
/// optimization: the `AnyPtr` is represented as an ordinary pointer.
/// In principle, this allows us to `transmute` between `*mut Tag` and `AnyPtr`.
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct AnyPtr {
    ptr: *mut Tag,
}

/// We can safely turn an `AnyPtr` into a more specific type of pointer using
/// the `view` method, which produces a `HeapRefView`.
#[derive(Clone, Copy)]
pub enum HeapRefView {
    String(StringPtr),
    HT(HTPtr),
    Array(ArrayPtr),
    Any(AnyJSPtr),
    Class(ObjectDataPtr),
    ObjectPtrPtr(ObjectPtr),
    Env(EnvPtr),
    NonPtr32(NonPtr32Ptr),
    MutF64(MutF64Ptr),
    Ptr(PtrPtr),
}
impl HeapRefView {
    /// Return a less specific `HeapPtr` that points to the same heap value,
    /// for implementing HeapPtr
    fn heap_ptr<'a>(&'a self) -> &'a dyn HeapPtr {
        match self {
            Self::String(val) => val,
            Self::HT(val) => val,
            Self::Array(val) => val,
            Self::Any(val) => val,
            Self::Class(val) => val,
            Self::ObjectPtrPtr(val) => val,
            Self::Env(val) => val,
            Self::NonPtr32(val) => val,
            Self::MutF64(val) => val,
            Self::Ptr(val) => val,
        }
    }
}
impl HeapPtr for HeapRefView {
    fn get_ptr(&self) -> *mut Tag {
        self.heap_ptr().get_ptr()
    }
    fn get_data_size(&self, heap: &Heap) -> usize {
        self.heap_ptr().get_data_size(heap)
    }
    fn final_drop(&self) {
        self.heap_ptr().final_drop()
    }
    fn get_gc_ptrs(&self, heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        self.heap_ptr().get_gc_ptrs(heap)
    }
}

impl AnyPtr {
    /// # Safety
    ///
    /// since HeapPtr methods are safe, all AnyPtrs that are constructed must be
    /// valid. therefore AnyPtr::new() must follow the data model specified by the Tag
    /// given which can't easily be enforced by type
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        AnyPtr { ptr }
    }

    /// Discriminate on the tag, and return a more specific `HeapRefView` that
    /// points to the same heap value.
    pub fn view(&self) -> HeapRefView {
        // SAFETY: AnyPtrs are unsafe to construct (right?) so we have
        // guaranteed our ptr is to a tag. since we check the tag, we know
        // it's the right tag
        unsafe {
            let heap_ref: Tag = *self.ptr;
            match heap_ref.type_tag {
                TypeTag::String => HeapRefView::String(StringPtr::new(self.ptr)),
                TypeTag::HT => HeapRefView::HT(HTPtr::new_tag_unchecked(self.ptr)),
                TypeTag::Array => HeapRefView::Array(ArrayPtr::new_tag_unchecked(self.ptr)),
                TypeTag::Any => HeapRefView::Any(AnyJSPtr::new_tag_unchecked(self.ptr)),
                TypeTag::DynObject => HeapRefView::Class(ObjectDataPtr::new(self.ptr)),
                TypeTag::ObjectPtrPtr => HeapRefView::ObjectPtrPtr(ObjectPtr::new(self.ptr)),
                TypeTag::Env => HeapRefView::Env(EnvPtr::new(self.ptr)),
                TypeTag::NonPtr32 => {
                    HeapRefView::NonPtr32(NonPtr32Ptr::new_tag_unchecked(self.ptr))
                }
                TypeTag::MutF64 => HeapRefView::MutF64(MutF64Ptr::new_tag_unchecked(self.ptr)),
                TypeTag::Ptr => HeapRefView::Ptr(PtrPtr::new_tag_unchecked(self.ptr)),
            }
        }
    }
}
// Deref as well
impl HeapPtr for AnyPtr {
    fn get_ptr(&self) -> *mut Tag {
        self.view().get_ptr()
    }
    fn get_data_size(&self, heap: &Heap) -> usize {
        self.view().get_data_size(heap)
    }
    fn final_drop(&self) {
        self.view().final_drop()
    }
    fn get_gc_ptrs(&self, heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        self.view().get_gc_ptrs(heap)
    }
}

/// If p : TypePtr<i32> then
/// p.ptr : *const Tag and
/// p.ptr.type_tag == HasTag::<i32>::TYPE_TAG
///                == TypeTag::I32
///
/// Hypothetical:
/// pub struct TypePtr<T: HasTag> {
///     ptr: *mut Tag && ptr.type_tag == HasTag::<T>::TYPE_TAG,
/// }
///
/// Of course, we can't write the code above, which is what PhantomData
/// solves.
#[derive(Debug)]
#[repr(transparent)]
pub struct TypePtr<T> {
    ptr: *mut Tag,
    _phantom: PhantomData<T>,
}
// these are necessary because if T is not clone/copy, derive will try not
// to let it clone/copy, but this is a pointer and should copy
impl<T> Clone for TypePtr<T> {
    fn clone(&self) -> TypePtr<T> {
        TypePtr {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }
}
impl<T> Copy for TypePtr<T> {}

impl<T: HasTag> HeapPtr for TypePtr<T> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }

    fn get_data_size(&self, _heap: &Heap) -> usize {
        return layout::layout_aligned::<T>(ALIGNMENT).size();
    }

    fn final_drop(&self) {
        unsafe { std::ptr::drop_in_place::<T>(data_ptr(self.ptr)) }
    }

    fn get_gc_ptrs(&self, heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        self.get().get_data_ptrs(heap)
    }
}

impl<T> TypePtr<T> {
    pub fn size() -> isize {
        return layout::layout_aligned::<T>(ALIGNMENT).size() as isize;
    }

    // safety: Tag must match T, value must be immediately initialized
    // with write
    pub const unsafe fn new_tag_unchecked(ptr: *mut Tag) -> Self {
        TypePtr {
            ptr,
            _phantom: PhantomData,
        }
    }
    // safety: data_ptr must be initialized with write
    unsafe fn new_tag(ptr: *mut Tag, type_tag: TypeTag) -> Self {
        assert_eq!(ptr.read().type_tag, type_tag);
        Self::new_tag_unchecked(ptr)
    }
    pub fn new(ptr: *mut Tag, type_tag: TypeTag, value: T) -> Self {
        let mut this = unsafe { Self::new_tag(ptr, type_tag) };
        this.write(value);
        this
    }

    /// safe alternatives to read / write where read() doesn't cause drop
    pub fn get(&self) -> &T {
        unsafe { &*data_ptr(self.ptr) }
    }
    /// write data, dropping old data. if you're not the allocator, this is
    /// what you want
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *data_ptr(self.ptr) }
    }
    /// write data without dropping old rust-owned data. this should only
    /// really be used by the allocater for writing to uninitialized tag
    fn write(&mut self, val: T) {
        unsafe { std::ptr::write(data_ptr(self.ptr), val) }
    }
}

impl<T> Deref for TypePtr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.get()
    }
}
impl<T> DerefMut for TypePtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<T: std::hash::Hash> std::hash::Hash for TypePtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state)
    }
}
impl<T: PartialEq> PartialEq<TypePtr<T>> for TypePtr<T> {
    fn eq(&self, other: &TypePtr<T>) -> bool {
        self.get() == other.get()
    }
}
impl<T: PartialEq> Eq for TypePtr<T> {}

impl<T: HasTag> From<TypePtr<T>> for AnyPtr {
    fn from(ptr: TypePtr<T>) -> Self {
        // safety: TypePtr is valid, so an AnyPtr will be valid
        unsafe { AnyPtr::new(ptr.get_ptr()) }
    }
}

impl Tag {
    /**
     * Returns a reference to a slice of values that immediately follow this
     * tag. This method assumes that the tag is truly followed by `len`
     * `U`-typed values.
     */
    pub unsafe fn slice_ref<U>(&mut self, len: usize) -> &mut [U] {
        let self_ptr = self as *mut Tag;
        let data_ptr: *mut U = std::mem::transmute(self_ptr.add(DATA_OFFSET));
        return std::slice::from_raw_parts_mut(data_ptr, len);
    }
}

//! managed allocation. most allocations should be made through [Heap]

use crate::{AnyEnum, AnyValue};
use std::alloc;
use std::alloc::Layout;
use std::cell::{Cell, RefCell};
use std::mem;
mod class_list;
mod constants;
mod env;
mod heap_values;
mod layout;
mod object_ptr;
mod string;
use crate::util::*;

pub mod heap_types;
pub use heap_values::AnyPtr;
pub use heap_values::HeapPtr;
pub use heap_values::HeapRefView;
mod f64_allocator;

use class_list::ClassList;
use constants::*;
use f64_allocator::F64Allocator;
use heap_types::*;
pub use heap_values::Tag;
pub use heap_values::TypeTag;
use heap_values::*;

#[cfg(test)]
mod tests;

/// A managed heap backed by rust allocation (it simply tracks where objects
/// are allocated)
///
/// When running a program, we have a single global Heap ([crate::heap()]).
/// However, during testing, we create several heaps.
pub struct Heap {
    pub f64_allocator: RefCell<F64Allocator>,
    size: isize,
    // an estimate of the memory used by the rust allocator to support this
    // heap, based on allocation sizes, but not layout
    used_mem: Cell<isize>,
    alloc_list: RefCell<AllocList>,
    tag_size: isize,
    pub classes: RefCell<ClassList>,
    /// We initialize this to the empty stack. Before calling [Heap::gc()], the
    /// shadow stack must contain all GC roots.
    shadow_stack: RefCell<Vec<Vec<Option<*mut Tag>>>>,
}

/// https://rust-unofficial.github.io/too-many-lists/second-option.html
#[derive(Debug)]
struct AllocList {
    list: Link,
}
type Link = Option<Box<AllocItem>>;
#[derive(Debug)]
struct AllocItem {
    tag: *mut Tag,
    next: Link,
}

impl AllocList {
    pub fn new() -> Self {
        return AllocList { list: None };
    }

    pub fn push(&mut self, tag: *mut Tag) {
        self.list = Some(Box::new(AllocItem {
            tag,
            next: self.list.take(),
        }));
    }

    /// Returns map from sizes to counts of everything in the free list
    fn histogram(&self, heap: &Heap) -> std::collections::BTreeMap<usize, usize> {
        let mut map = std::collections::BTreeMap::new();
        let mut to = &self.list;
        loop {
            match to {
                Some(more) => {
                    let tag = unsafe { AnyPtr::new(more.tag) };
                    *map.entry(tag.get_data_size(heap)).or_insert(0) += 1;
                    to = &to.as_ref().unwrap().next;
                }
                None => break,
            }
        }
        map
    }
}

/// allocate the number of bytes using the rust allocator, with an alignment of
/// 4, and return the address
fn alloc_raw(bytes: isize) -> *mut Tag {
    debug_assert!(bytes > 0);
    // SAFETY: assertion above ensures size is not zero, and the alignment is
    // valid in wasm (power of two)
    unsafe { alloc::alloc(Layout::from_size_align_unchecked(bytes as usize, 4)) as *mut Tag }
}

impl Heap {
    /// Create a new heap with the given approximate max size
    ///
    /// The size given is approximately how much rust memory the heap should
    /// occupy before garbage collecting. It should be well below the amount of
    /// rust memory actually available, for two reasons:
    ///
    /// 1. Garbage collection may reserve some incidental memory, and OOM
    ///    should be avoided
    /// 2. The occupied memory of the heap is merely an estimate, and may be
    ///    more or less depending on how rust deals with things
    pub fn new(size: isize) -> Self {
        let f64_allocator = RefCell::new(F64Allocator::new());
        let used_mem = Cell::new(0);
        let alloc_list = RefCell::new(AllocList::new());
        let tag_size = layout::layout_aligned::<Tag>(ALIGNMENT).size() as isize;
        let classes = RefCell::new(ClassList::new());
        let shadow_stack = RefCell::new(vec![]);
        return Heap {
            f64_allocator,
            size,
            used_mem,
            alloc_list,
            tag_size,
            classes,
            shadow_stack,
        };
    }

    /// if there is enough space to [`alloc_raw`], **increase used_mem by that
    /// amount, add entry to alloc_list** and return the address. otherwise,
    /// return None
    fn alloc_raw(&self, bytes: isize) -> Option<*mut Tag> {
        if (self.size - self.used_mem.get()) < bytes {
            None
        } else {
            self.used_mem.set(self.used_mem.get() + bytes);
            let tag = alloc_raw(bytes);
            self.alloc_list.borrow_mut().push(tag);
            Some(tag)
        }
    }

    pub fn f64_to_any(&self, x: f64) -> AnyValue {
        AnyEnum::F64(self.alloc_f64_or_gc(x)).into()
    }

    pub fn alloc_f64_or_gc(&self, x: f64) -> *const f64 {
        let mut opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        if let None = opt_ptr {
            self.gc();
            // TODO(arjun): This is a design flaw. We have to borrow_mut again,
            // because self.gc also borrows the f64_allocator. Frankly, we
            // should use a raw pointer.
            opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        }
        unwrap_log(opt_ptr, "out of f64 memory")
    }

    /**
     * Allocates a primitive value on the heap, and returns a reference to the
     * tag that precedes the primitive.
     *
     * Primitive values are defined as those with [HasTag] implemented
     */
    pub fn alloc<T: HasTag>(&self, value: T) -> Result<TypePtr<T>, T> {
        self.alloc_tag(T::get_tag(), value)
    }
    pub fn alloc_or_gc<T: HasTag + std::fmt::Debug>(&self, value: T) -> TypePtr<T> {
        match self.alloc(value) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.gc();
                // TODO(luna): grow?
                self.alloc(value).expect("out of memory even after gc")
            }
        }
    }
    fn alloc_tag<T>(&self, tag: Tag, value: T) -> Result<TypePtr<T>, T> {
        let opt_ptr = self.alloc_raw(self.tag_size + TypePtr::<T>::size());
        match opt_ptr {
            None => Err(value),
            Some(ptr) => {
                let tag_ptr: *mut Tag = ptr as *mut Tag;
                unsafe { tag_ptr.write(tag) };
                let val_ref = TypePtr::<T>::new(tag_ptr, tag.type_tag, value);
                Ok(val_ref)
            }
        }
    }

    pub fn alloc_object(&self, type_tag: u16) -> Option<ObjectPtr> {
        let object_data = self.alloc_object_data(type_tag)?;
        // we explicitly make sure we're calling TypePtr's get_ptr
        // implementation so we don't get the pointer of the object data. this
        // should work properly now that ObjectDataPtr implements HasTag,
        // but if it gets messed up it leads to insidious memory bugs
        Some(unsafe { ObjectPtr::new(HeapPtr::get_ptr(&self.alloc(object_data).ok()?)) })
    }
    pub fn alloc_object_or_gc(&self, type_tag: u16) -> ObjectPtr {
        // TODO(luna): alloc_object isn't really as atomic as it needs to be
        match self.alloc_object(type_tag) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_object(type_tag)
                    .expect("out of memory even after gc")
            }
        }
    }
    pub fn alloc_object_data_or_gc(&self, type_tag: u16) -> ObjectDataPtr {
        match self.alloc_object_data(type_tag) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_object_data(type_tag)
                    .expect("out of memory even after gc")
            }
        }
    }
    fn alloc_object_data(&self, type_tag: u16) -> Option<ObjectDataPtr> {
        let num_elements = self.get_class_size(type_tag);
        let elements_size = self.object_data_size(num_elements) as isize;
        let tag_ptr = unsafe { self.alloc_slice(Tag::object(type_tag), elements_size) }?;
        let values_slice = unsafe { (*tag_ptr).slice_ref::<Option<AnyEnum>>(num_elements) };
        for opt_any in values_slice.iter_mut() {
            *opt_any = None;
        }
        Some(unsafe { ObjectDataPtr::new(tag_ptr) })
    }
    pub fn alloc_str(&self, s: &str) -> Option<StringPtr> {
        let from_str = s.as_ptr();
        // + 4 for the length (not the tag, which isn't included)
        let size = s.len() + 4;
        unsafe {
            let tag_ptr = self.alloc_slice(Tag::with_type(TypeTag::String), size as isize)?;
            let len_ptr = tag_ptr.add(DATA_OFFSET) as *mut u32;
            len_ptr.write(u32::to_le(s.len() as u32));
            let into_str = len_ptr.add(1) as *mut u8;
            std::ptr::copy_nonoverlapping(from_str, into_str, s.len());
            Some(StringPtr::new(tag_ptr))
        }
    }
    pub fn alloc_str_or_gc(&self, s: &str) -> StringPtr {
        match self.alloc_str(s) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_str(s).expect("out of memory even after gc")
            }
        }
    }
    /// # Safety
    ///
    /// [alloc_env_or_gc]
    unsafe fn alloc_env(&self, length: u32, fn_obj: ObjectPtr) -> Option<EnvPtr> {
        // + 4 for the length (not the tag, which isn't included)
        let size = mem::size_of::<AnyEnum>() * length as usize // EnvItems
            + 4  // Len
            + mem::size_of::<ObjectPtr>(); // Fn obj pointer
        let tag_ptr = self.alloc_slice(Tag::with_type(TypeTag::Env), size as isize)?;
        Some(EnvPtr::init(tag_ptr, length, fn_obj))
    }
    /// SAFETY:
    ///
    /// this is unsafe for the same reason as EnvPtr::new(); it makes GC do
    /// UB unless/until you fill in the environment with values
    pub unsafe fn alloc_env_or_gc(&self, length: u32, fn_obj: ObjectPtr) -> EnvPtr {
        match self.alloc_env(length, fn_obj) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                self.alloc_env(length, fn_obj)
                    .expect("out of memory even after gc")
            }
        }
    }
    /// allocate a tag immediately followed by slice of memory of a fixed
    /// size in bytes
    ///
    /// # Safety
    ///
    /// size must be exactly AnyPtr::new(&tag).get_data_size(). size must
    /// be aligned
    ///
    /// TODO(luna): Because we use the rust allocator, we could back Object
    /// with a Vec, delete ObjectDataPtr, and get O(n) cached inserts
    unsafe fn alloc_slice(&self, tag: Tag, size: isize) -> Option<*mut Tag> {
        let ptr = self.alloc_raw(self.tag_size + size)?;
        let tag_ptr = ptr as *mut Tag;
        tag_ptr.write(tag);
        Some(tag_ptr)
    }

    pub fn object_data_size(&self, num_elements: usize) -> usize {
        Layout::array::<Option<AnyEnum>>(num_elements)
            .unwrap()
            .size()
    }

    pub fn get_class_size(&self, class_tag: u16) -> usize {
        self.classes.borrow().get_class_size(class_tag)
    }

    pub fn push_shadow_frame(&self, slots: usize) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.push(vec![None; slots]);
    }

    /// # Safety
    ///
    /// calling this without an appropriate push_shadow_frame, or before
    /// all locals in the topmost frame have gone out of scope, results in
    /// [Heap::gc] being unsafe (will free in-use data)
    pub unsafe fn pop_shadow_frame(&self) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.pop();
    }

    pub fn set_in_shadow_frame_slot(&self, frame: usize, slot: usize, ptr: Option<*mut Tag>) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack[frame][slot] = ptr;
    }
    pub fn set_in_current_shadow_frame_slot(&self, slot: usize, ptr: Option<*mut Tag>) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.last_mut().unwrap()[slot] = ptr;
    }

    /// # Safety
    ///
    /// if push_shadow_frame / pop_shadow_frame / set_in_current_shadow_frame_slot were
    /// used correctly (tagged unsafe), this is safe
    pub fn gc(&self) {
        let roots = self
            .shadow_stack
            .borrow()
            .iter()
            .flatten()
            .flatten()
            .map(|refptr| *refptr)
            .collect::<Vec<*mut Tag>>();
        self.mark_phase(roots, |_| ());
        self.sweep_phase();
    }

    /// # Safety
    ///
    /// if push_shadow_frame / pop_shadow_frame / set_in_current_shadow_frame_slot were
    /// used correctly (tagged unsafe), this is safe
    pub fn heap_dump(&self) {
        let roots = self
            .shadow_stack
            .borrow()
            .iter()
            .flatten()
            .flatten()
            .map(|refptr| *refptr)
            .collect::<Vec<*mut Tag>>();
        log!("===== BEGIN JANKYSCRIPT HEAP DUMP =====");
        self.mark_phase(roots, |any| {
            log!("{:x?}    {:?}", any.get_ptr(), any.view())
        });
        log!("=====    BEGIN HEAP DUMP SWEEP    =====");
        self.sweep_phase();
        log!("=====  END JANKYSCRIPT HEAP DUMP  =====");
    }

    fn mark_phase(&self, roots: Vec<*mut Tag>, dump: impl Fn(AnyPtr)) {
        error!("=====    BEGIN JANKYPSCRIPT GC    =====");
        let mut current_roots = roots;
        let mut new_roots = Vec::<*mut Tag>::new();

        let mut f64_allocator = self.f64_allocator.borrow_mut();
        f64_allocator.semispace_swap();

        // this is just for debugging, but i'm going to leave it in because i
        // don't foresee not needing it any time soon
        let mut count = 0;
        while current_roots.is_empty() == false {
            for root in current_roots.drain(0..) {
                let tag = unsafe { &mut *root };

                if tag.marked == true {
                    continue;
                }
                tag.marked = true;
                count += 1;

                let any_ptr = unsafe { AnyPtr::new(root) };
                dump(any_ptr);

                let (mut tags, f64s) = any_ptr.get_gc_ptrs(self);
                new_roots.append(&mut tags);
                for ptr in f64s {
                    unsafe { *ptr = f64_allocator.alloc(**ptr).unwrap() }
                }
            }
            mem::swap(&mut current_roots, &mut new_roots);
        }
        error!("===== MARKED {} OBJECTS =====", count);
    }

    fn sweep_phase(&self) {
        let mut count = 0;
        let mut to = &mut self.alloc_list.borrow_mut().list;
        while let Some(ref item) = to {
            let ptr = item.tag;
            unsafe {
                if (*ptr).marked {
                    (*ptr).marked = false;
                    to = &mut to.as_mut().unwrap().next;
                } else {
                    let any_ptr = AnyPtr::new(ptr);
                    let size = self.tag_size as usize + any_ptr.get_data_size(self);
                    // drop any rust memory that may exist
                    any_ptr.final_drop();
                    alloc::dealloc(
                        item.tag as *mut u8,
                        Layout::from_size_align_unchecked(size, 4),
                    );
                    self.used_mem.set(self.used_mem.get() - size as isize);
                    count += 1;
                    // remove from the free list
                    *to = to.take().unwrap().next;
                }
            }
        }
        error!(
            "===== FREED {} OBJECTS. {}/{} USED =====",
            count,
            self.used_mem.get(),
            self.size
        );
        error!("=====      END JANKYSCRIPT GC     =====");
    }

    /// for debugging. print info about free vs allocated memory
    pub fn mem_info(&self) {
        let hist = self.alloc_list.borrow().histogram(self);
        error!("FREE LIST HIST\n{:#?}\nEND", hist);
    }
}

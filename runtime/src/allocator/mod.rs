use std::alloc::Layout;
use std::alloc;
use std::cell::RefCell;
use std::collections::HashMap;
mod layout;
mod heap_values;
mod constants;

use heap_values::*;

#[cfg(test)]
mod tests;

#[cfg(target_pointer_width = "64")]
pub const ALIGNMENT : usize = 8;

#[cfg(target_pointer_width = "32")]
pub const ALIGNMENT : usize = 4;

pub struct Heap {
    buffer: *mut u8,
    size: isize,
    free_list: RefCell<FreeList>,
    prim_size: isize,
    tag_size: isize,
    container_sizes: HashMap<u16, usize>,
    next_container_type: u16
}

struct Block {
    start: *mut u8,
    size: isize
}

enum FreeList {
    Nil,
    Block(Block, Box<FreeList>)
}


impl FreeList {

    pub fn new(start: *mut u8, size: isize) -> Self {
        let block = Block { start, size };
        return FreeList::Block(block, Box::new(FreeList::Nil));
    }

    pub fn insert(mut self, start: *mut u8, size: isize) -> FreeList {
        match &mut self {
            FreeList::Nil => {
                return FreeList::new(start, size);
            }
            FreeList::Block(block, rest) => {
                let new_block_end = unsafe { start.add(size as usize) };
                if new_block_end < block.start {
                    let new_block = Block { start, size };
                    return FreeList::Block(new_block, Box::new(self));
                }
                else if new_block_end == block.start {
                    block.start = start;
                    block.size = block.size + size;
                    return self;
                }
                else if start == unsafe { start.add(size as usize) } {
                    block.size = block.size + size;
                    return self;
                }
                else {
                    let rest2 = std::mem::replace(rest.as_mut(), FreeList::Nil);
                    *rest.as_mut() = rest2.insert(start, size);
                    return self;
                }
            }
        }
    }

    /**
     * Returns the address of a free block of size `size`, and updates the
     * free size.
     */
    pub fn find_free_size(&mut self, size: isize) -> Option<*mut u8> {
        // TODO(arjun): Make iterative for performance.
        match self {
            FreeList::Block(block, next) => {
                if size == block.size {
                    let addr = block.start;
                    let next = std::mem::replace(next.as_mut(), FreeList::Nil);
                    std::mem::replace(self, next);
                    return Some(addr);
                }
                else if size < block.size {
                    block.size = block.size - size;
                    let addr = block.start;
                    block.start = unsafe { block.start.offset(size) };
                    return Some(addr);
                }
                else /* size > block.size */ {
                    return Self::find_free_size(next.as_mut(), size);
                }
            }
            FreeList::Nil => {
                return None;
            }
        }
    }
}

fn layout_aligned<T>() -> Layout {
    return Layout::from_size_align(Layout::new::<T>().size(), ALIGNMENT).unwrap().pad_to_align();
}

impl Heap {

    pub fn new(size: isize) -> Self { 
        let layout = Layout::from_size_align(size as usize, ALIGNMENT).unwrap();
        let buffer = unsafe { alloc::alloc_zeroed(layout) };
        let free_list = RefCell::new(FreeList::new(buffer, size));
        let prim_size = layout_aligned::<i32>().size() as isize;
        let tag_size = layout_aligned::<Tag>().size() as isize;
        let container_sizes = HashMap::new();
        let next_container_type = 0;
        return Heap { buffer, size, free_list, prim_size, tag_size, container_sizes, next_container_type };
    }

    pub fn new_container_type(&mut self, num_elements: usize) -> u16 {
        let type_tag = self.next_container_type;
        self.next_container_type += 1;
        self.container_sizes.insert(type_tag, num_elements);
        return type_tag;
    }

    /**
     * Allocates a primitive value on the heap, and returns a reference to the
     * tag that precedes the primitive.
     */
    pub fn alloc_i32<'a>(&'a self, value: i32) -> Option<I32Ptr<'a>> {
        let opt_ptr = self.free_list.borrow_mut().find_free_size(self.tag_size + self.prim_size);
        match opt_ptr {
            None => None,
            Some(ptr) => {
                let tag_ptr: *mut Tag = unsafe { std::mem::transmute(ptr) };
                let tag_ref: &mut Tag = unsafe { &mut *tag_ptr };
                *tag_ref = Tag::i32();
                let i32ref = I32Ptr::new(unsafe { std::mem::transmute(ptr) });
                i32ref.write(value);
                return Some(i32ref);
            }
        }
    }

    pub fn alloc_container<'a>(&'a self, type_tag: u16) -> Option<ObjectPtr<'a>> {
        let num_elements = self.container_sizes.get(&type_tag).unwrap();
        let elements_size = Layout::array::<Option<&Tag>>(*num_elements).unwrap().size() as isize;
        let opt_ptr = self.free_list.borrow_mut().find_free_size(self.tag_size + elements_size);
        match opt_ptr {
            None => None,
            Some(ptr) => {
                let tag_ptr: *mut Tag = unsafe { std::mem::transmute(ptr) };
                let tag_ref: &mut Tag = unsafe { &mut *tag_ptr };
                unsafe {
                    tag_ptr.write(Tag::object(type_tag));
                }
                let values_slice = unsafe { tag_ref.slice_ref::<Option<&mut Tag>>(*num_elements) };
                for ptr in values_slice.iter_mut() {
                    *ptr = None;
                }
                return Some(unsafe { ObjectPtr::new(tag_ptr) });
            }
        }
    }

    pub unsafe fn garbage_collect(&self, roots: &[*mut Tag]) {
        self.mark_phase(roots);
        self.sweep_phase();
    }

    fn mark_phase(&self, roots: &[*mut Tag]) {
        let mut current_roots = roots.to_vec();
        let mut new_roots = Vec::<*mut Tag>::new();

        while current_roots.is_empty() == false {
            for root in current_roots.drain(0..) {
                let tag = unsafe { &mut *root };
                
                if tag.marked == true {
                    continue;
                }
                tag.marked = true;

                if tag.type_tag != TypeTag::Object {
                    continue;
                }
                let class_tag = tag.class_tag; // needed since .class_tag is packed
                let num_ptrs = *self.container_sizes.get(&class_tag).expect("unknown class tag");
                let members_ptr : *mut *mut Tag = unsafe { data_ptr(root) };
                let members = unsafe { std::slice::from_raw_parts(members_ptr, num_ptrs) };
                new_roots.extend_from_slice(members);
            }
            std::mem::swap(&mut current_roots, &mut new_roots);
        }
    }

    fn sweep_phase(&self) {
        use std::borrow::BorrowMut;
        let mut free_list = self.free_list.borrow_mut();
        
        let borrowed_free_list : &mut FreeList = free_list.borrow_mut();
        let mut free_list_ptr = std::mem::replace(borrowed_free_list, FreeList::Nil);

        let mut ptr : *mut u8 = self.buffer;
        let heap_max = unsafe { self.buffer.add(self.size as usize) };
        while ptr < heap_max {
            let any_ptr = unsafe { AnyPtr::new(std::mem::transmute(ptr)) };
            let tag_ref = unsafe { &mut *any_ptr.get_ptr() };
            let size = self.tag_size as usize + any_ptr.get_data_size(self);
            if tag_ref.marked == true {
                tag_ref.marked = false;
                ptr = unsafe { ptr.add(size) };
                continue;
            }

            free_list_ptr = free_list_ptr.insert(ptr, size as isize);
                    
        }
        *borrowed_free_list = free_list_ptr;
    }


    #[cfg(test)]
    pub fn raw(&self) -> &[u8] {
        return unsafe { std::slice::from_raw_parts(self.buffer, self.size as usize) };
    }


}
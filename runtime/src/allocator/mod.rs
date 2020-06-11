use std::alloc::Layout;
use std::alloc;
use std::cell::RefCell;
use std::collections::HashMap;
mod layout;
mod heap_values;
mod constants;

use constants::*;
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
    pub fn alloc_i32<'a>(&'a self, value: i32) -> Option<I32Ref<'a>> {
        let opt_ptr = self.free_list.borrow_mut().find_free_size(self.tag_size + self.prim_size);
        match opt_ptr {
            None => None,
            Some(ptr) => {
                let i32ref = I32Ref::new(unsafe { std::mem::transmute(ptr) });
                i32ref.write(value);
                return Some(i32ref);
            }
        }
    }

    pub fn alloc_container<'a>(&'a self, type_tag: u16) -> Option<ObjectRef<'a>> {
        let num_elements = self.container_sizes.get(&type_tag).unwrap();
        let elements_size = Layout::array::<Option<&Tag>>(*num_elements).unwrap().size() as isize;
        let opt_ptr = self.free_list.borrow_mut().find_free_size(self.tag_size + elements_size);
        match opt_ptr {
            None => None,
            Some(ptr) => {
                let tag_ptr: *mut Tag = unsafe { std::mem::transmute(ptr) };
                let tag_ref: &mut Tag = unsafe { &mut *tag_ptr };
                *tag_ref = Tag::Object(type_tag);
                let values_slice = unsafe { tag_ref.slice_ref::<Option<&mut Tag>>(*num_elements) };
                for ptr in values_slice.iter_mut() {
                    *ptr = None;
                }
                return Some(unsafe { ObjectRef::new(tag_ptr) });
            }
        }
    }


    #[cfg(test)]
    pub fn raw(&self) -> &[u8] {
        return unsafe { std::slice::from_raw_parts(self.buffer, self.size as usize) };
    }


}
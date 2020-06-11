use super::*;

#[test]
fn alloc_i32s() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.alloc_i32(32).expect("first alloc");
    heap.alloc_i32(64).expect("second alloc");
    assert_eq!(heap.alloc_i32(12), None);
    let raw = heap.raw();
    // Both x86 and Wasm are little-endian.
    assert_eq!(raw[ALIGNMENT], 32);
    assert_eq!(raw[ALIGNMENT * 3], 64);
}

#[test]
fn update_prims() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.alloc_i32(32).expect("first alloc");
    let ptr = heap.alloc_i32(64).expect("second alloc");
    ptr.write(128);
    let raw = heap.raw();
    assert_eq!(raw[ALIGNMENT], 32);
    assert_eq!(raw[ALIGNMENT * 3], 128);
}

#[test]
fn alloc_container1() {
    let mut heap = Heap::new(64);
    let type_tag = heap.new_container_type(2);
    heap.alloc_i32(32).expect("first alloc");
    let container = heap.alloc_container(type_tag).expect("second alloc");
    assert_eq!(container.read_at(&heap, 0), None);
    assert_eq!(container.read_at(&heap, 1), None);

}

#[test]
fn alloc_container2() {
    let mut heap = Heap::new(40);
    let type_tag = heap.new_container_type(2);
    let container = heap.alloc_container(type_tag).expect("second alloc");
    let x = heap.alloc_i32(200).expect("second alloc");
    container.write_at(&heap, 0, x);
    x.write(100);
    let elt = container.read_at(&heap, 0).expect("read");
    match elt.view() {
        HeapRefView::I32(prim) => assert_eq!(100, prim.read()),
        _ => assert!(false)
    }
}


#[test]
fn alloc_container3() {
    let mut heap = Heap::new(40);
    let type_tag = heap.new_container_type(2);
    let container = heap.alloc_container(type_tag).expect("second alloc");
    let x = heap.alloc_i32(200).expect("second alloc");
    container.write_at(&heap, 1, x);
    x.write(100);
    let elt = container.read_at(&heap, 1).expect("read");
    match elt.view() {
        HeapRefView::I32(prim) => assert_eq!(100, prim.read()),
        _ => assert!(false)
    }
}
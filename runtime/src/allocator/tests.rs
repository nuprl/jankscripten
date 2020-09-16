use super::class_list::Class;
use super::*;
use wasm_bindgen_test::*;

#[test]
#[wasm_bindgen_test]
fn allocs() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let x = heap.alloc(32).expect("first alloc");
    assert_eq!(*x.get(), 32);
    let y = heap.alloc(64).expect("second alloc");
    assert_eq!(*y.get(), 64);
    assert_eq!(*x.get(), 32);
    assert!(heap.alloc(12).is_err());
}

#[test]
#[wasm_bindgen_test]
fn gc_enter_exit() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.push_shadow_frame(1);
    let x = heap.alloc(32).expect("first allocation failed");
    assert_eq!(
        *x.get(),
        32,
        "first value was not written to the heap correctly"
    );
    heap.set_in_current_shadow_frame_slot(0, x.get_ptr());
    // not added as a root
    let y = heap.alloc(64).expect("second allocation failed");
    assert_eq!(
        *y.get(),
        64,
        "second value was not written to the heap correctly"
    );
    assert_eq!(
        *x.get(),
        32,
        "second allocation corrupted the first value on the heap"
    );
    assert!(
        heap.alloc(12).is_err(),
        "third allocation should fail due to lack of space"
    );
    heap.gc();
    let z = heap.alloc(128).expect("GC failed to free enough memory");
    assert_eq!(*z.get(), 128, "allocation should succeed after GC");
    assert_eq!(
        *x.get(),
        32,
        "allocation after GC corrupted the first value on the heap"
    );
}

#[test]
#[wasm_bindgen_test]
fn alloc_or_gc_gcs() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.push_shadow_frame(1);
    let x = heap.alloc(32).expect("first allocation failed");
    assert_eq!(
        *x.get(),
        32,
        "first value was not written to the heap correctly"
    );
    heap.set_in_current_shadow_frame_slot(0, x.get_ptr());
    // not added as a root
    let y = heap.alloc(64).expect("second allocation failed");
    assert_eq!(
        *y.get(),
        64,
        "second value was not written to the heap correctly"
    );
    assert_eq!(
        *x.get(),
        32,
        "second allocation corrupted the first value on the heap"
    );
    assert!(
        heap.alloc(12).is_err(),
        "third allocation should fail due to lack of space"
    );
    let z = heap.alloc_or_gc(128);
    assert_eq!(
        *z.get(),
        128,
        "allocation should succeed after automatic GC"
    );
    assert_eq!(
        *x.get(),
        32,
        "allocation after GC corrupted the first value on the heap"
    );
}

#[test]
#[wasm_bindgen_test]
fn array_members_marked() {
    let heap = Heap::new((ALIGNMENT * 6) as isize);
    // the one root will be the array
    heap.push_shadow_frame(1);
    // i32: ALIGNMENT * 2 (32-bit), OR ALIGNMENT * 1 (64-bit)
    let x = heap.alloc(32).expect("first allocation failed");
    assert_eq!(
        *x.get(),
        32,
        "first value was not written to the heap correctly"
    );
    // x is not a root
    // Vec: ALIGNMENT * 4 (tag, ptr, len, cap)
    let mut arr: TypePtr<Vec<AnyValue>> = heap.alloc_or_gc(Vec::new());
    // arr is
    heap.set_in_current_shadow_frame_slot(0, arr.get_ptr());
    // but put x into the array
    arr.push(AnyEnum::Ptr(x.into()).into());
    assert!(
        heap.alloc(12).is_err(),
        "third allocation should fail due to lack of space"
    );
    heap.gc();
    assert!(heap.alloc(12).is_err(), "nothing should have been freed");
    // pop the x to free it on next gc
    arr.pop();
    heap.gc();
    heap.alloc(12).expect("now an int has been freed");
}

#[test]
#[wasm_bindgen_test]
fn object_members_marked() {
    let heap = Heap::new((ALIGNMENT * 8 + 5) as isize);
    // the roots will be: the string and the array
    heap.push_shadow_frame(2);
    // i32: ALIGNMENT * 2 (32-bit), OR ALIGNMENT * 1 (64-bit)
    let x = heap.alloc(32).expect("first allocation failed");
    assert_eq!(
        *x.get(),
        32,
        "first value was not written to the heap correctly"
    );
    // ALIGNMENT (tag) + 4 (size) + 1
    let x_str = heap.alloc_str("x").unwrap();
    heap.set_in_current_shadow_frame_slot(0, x_str.get_ptr());
    let one_type = heap.classes.borrow_mut().transition(0, x_str);
    // Object: ALIGNMENT * 5 = ALIGNMENT * 2 (tag, ptr) + ALIGNMENT * 3 (tag, any (size: ALIGNMENT * 2))
    let mut obj = heap.alloc_object(one_type).expect("couldn't alloc obj");
    // obj is root
    heap.set_in_current_shadow_frame_slot(1, obj.get_ptr());
    // but put x into the obj
    let mut cache = -1;
    obj.insert(&heap, x_str, AnyEnum::Ptr(x.into()).into(), &mut cache);
    assert!(
        heap.alloc(12).is_err(),
        "third allocation should fail due to lack of space"
    );
    heap.gc();
    assert!(heap.alloc(12).is_err(), "nothing should have been freed");
    // unset the pointer to free the x
    obj.insert(&heap, x_str, AnyEnum::Bool(false).into(), &mut cache);
    heap.gc();
    heap.alloc(12).expect("now an int has been freed");
}

#[test]
#[wasm_bindgen_test]
fn gc_f64s() {
    let heap = Heap::new((ALIGNMENT * 6) as isize);
    heap.push_shadow_frame(1);
    let x = heap.f64_to_any(5.);
    let mut arr: TypePtr<Vec<AnyValue>> = heap.alloc(Vec::new()).unwrap();
    heap.set_in_current_shadow_frame_slot(0, arr.get_ptr());
    arr.push(x);
    let x_copy = arr[0];
    heap.gc();
    assert_ne!(x_copy, arr[0]);
}

#[test]
#[wasm_bindgen_test]
fn update_prims() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.alloc(32).expect("first alloc");
    let mut ptr = heap.alloc(64).expect("second alloc");
    *ptr.get_mut() = 128;
    let raw = heap.raw();
    assert_eq!(raw[ALIGNMENT], 32);
    assert_eq!(raw[ALIGNMENT * 3], 128);
}

#[test]
#[wasm_bindgen_test]
fn alloc_container1() {
    let heap = Heap::new(128);
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(0, heap.alloc_str("x").unwrap());
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, heap.alloc_str("y").unwrap());
    heap.alloc(32).expect("first alloc");
    let container = heap.alloc_object(type_tag).expect("second alloc");
    assert_eq!(container.read_at(&heap, 0), None);
    assert_eq!(container.read_at(&heap, 1), None);
}

#[test]
#[wasm_bindgen_test]
fn insert_object() {
    let heap = Heap::new(128);
    let mut obj = heap.alloc_object(0).expect("second alloc");
    let mut cache = -1;
    assert_eq!(
        obj.insert(
            &heap,
            heap.alloc_str("x").unwrap(),
            AnyEnum::I32(32).into(),
            &mut cache
        ),
        AnyEnum::I32(32).into()
    );
    assert_eq!(
        obj.insert(
            &heap,
            heap.alloc_str("x").unwrap(),
            AnyEnum::I32(32).into(),
            &mut cache
        ),
        AnyEnum::I32(32).into()
    );
    assert!(matches!(
        obj.get(&heap, heap.alloc_str("x").unwrap(), &mut cache)
            .expect("no x"),
        AnyEnum::I32(32)
    ));
    assert!(matches!(
        obj.get(&heap, heap.alloc_str("x").unwrap(), &mut -1)
            .expect("no x"),
        AnyEnum::I32(32)
    ));
}

#[test]
fn alloc_container2() {
    let heap = Heap::new(128);
    let empty_type = heap.classes.borrow_mut().new_class_type(Class::new());
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(empty_type, heap.alloc_str("x").unwrap());
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, heap.alloc_str("y").unwrap());
    let container = heap.alloc_object(type_tag).expect("second alloc");
    let mut x: AnyJSPtr = heap.alloc(AnyEnum::I32(200).into()).expect("second alloc");
    container.write_at(&heap, 0, AnyEnum::Ptr(x.into()).into());
    *x.get_mut() = AnyEnum::I32(100).into();
    let elt = container.read_at(&heap, 0).expect("read");
    match elt {
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::Any(any) => assert_eq!(AnyEnum::I32(100), **any.get()),
            _ => panic!("not an any"),
        },
        _ => assert!(false),
    }
}

#[wasm_bindgen_test]
#[test]
fn string_read_alloc() {
    let heap = Heap::new((ALIGNMENT * 128) as isize);
    let x = heap.alloc_str("steven").expect("first alloc");
    let _ = &*x;
    let _ = &*x;
    assert_eq!(&*x, "steven");
    assert_eq!(&*x, "steven");
}

#[test]
fn string_ptr_drop_read() {
    // String = Vec<u8> = {addr, size, alloc}
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let x = heap.alloc_str("universe").expect("first alloc");
    let copied = x.clone();
    drop(copied);
    assert_eq!(&*x, "universe");
    drop(x);
}

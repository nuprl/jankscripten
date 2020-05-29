(module
  ;; (param $out $int)
  (import "runtime" "num_i32" (func $num_i32 (param i32) (param i32)))
  ;; (param $out $float)
  (import "runtime" "num_f64" (func $num_f64 (param i32) (param f64)))
  ;; i think $_ is because of i64 alignment
  ;; (param $out $kind $_ $val $kind $_ $val)
  (import "runtime" "add_num" (func $add_num (param i32 i32 i32 i64 i32 i32 i64)))
  ;; (param $out $num)
  (import "runtime" "num_as_i32" (func $num_as_i32 (param i32) (result i32)))
  (import "runtime" "memory" (memory 1))

  (func (export "run") (result i32)
    ;; sample program: Num(4) + Num(6)

    ;; Num(4) -> [0]
    i32.const 0
    i32.const 4
    call $num_i32
    ;; Num(6) -> [16]
    i32.const 16
    i32.const 6
    call $num_i32
    ;; for call to add:
    ;; out address = 32
    i32.const 32
    ;; load num components of [0]
    i32.const 0
    i32.load
    i32.const 0
    i32.load offset=4
    i32.const 0
    i64.load offset=8
    ;; load num components of [16]
    i32.const 16
    i32.load
    i32.const 16
    i32.load offset=4
    i32.const 16
    i64.load offset=8
    call $add_num

    ;; now convert back into i32 for return
    i32.const 32
    call $num_as_i32
  )

)



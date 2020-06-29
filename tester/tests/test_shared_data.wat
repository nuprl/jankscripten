(module
  (import "runtime" "JNKS_STRINGS" (global $JNKS_STRINGS i32))
  (import "runtime" "memory" (memory 1))

  ;; sample program: (Num(4) + Num(6) => Num(10)).unwrap() => 10
  (func (export "main") (result i32)
    global.get $JNKS_STRINGS
    i32.load
    return
  )

  (data (global.get $JNKS_STRINGS) "\04\00\00\00ello world")

)

// Testing various data type coercions with the + operator

// monotype
log_any(1 + 1); // 2
log_any("aaa" + "bbb"); // aaabbb
log_any(1.2 + 1.2) // 2.4

// polytype with string coercions
log_any("number " + 1); // number 1
log_any(1 + " number"); // 1 number
log_any("float " + 1.2); // float 1.2
log_any(1.2 + " float"); // 1.2 float
log_any("boolean " + true); // boolean true
log_any(true + " boolean"); // true boolean
log_any("value " + undefined); // value undefined
log_any(undefined + " value"); // undefined value
log_any("value " + null); // value null
log_any(null + " value"); // null value

log_any(1 + {}); // 1[object Object]
log_any({} + 1); // [object Object]1
log_any({} + {}); // [object Object][object Object]

var obj = {
    toString: function() {
        return "hello world!";
    }
}
log_any("Message: " + obj); // Message: hello world!

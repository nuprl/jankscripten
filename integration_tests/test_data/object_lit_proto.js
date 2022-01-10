//! -t
// Make sure that `{}` inherits from `Object.prototype`.

Object.prototype.sum = function(x, y) {
    return x + y;
}

let obj = {};

let sumResult = obj.__proto__.sum(5, 3);

// 5 + 3 should equal 8
log_any(sumResult);

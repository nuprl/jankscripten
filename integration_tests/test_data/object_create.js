//! -t
var x = {};
x.hello = "there";
var y = Object.create(x);
console.log(y.hello);

function f(o) {
    o.a = 10;
    return o.b;
}
console.log(f({b: 10}));
console.log(f({c: 12, b: 10}));

function any(x) {
    return x;
}
any(5); // Force any to be polymorphic
let a = [8, 6, 2, 8];
console.log(a.length);
console.log(any(a).length);
let b = { length: 'beep' };
console.log(b.length);
console.log(any(b).length);

function returns_twelve(value) {
    return 12;
}
// Force any (actually just an id function but it's used by multiple types)
function any(x) {
    return x;
}
var a = any([]);
a.push(undefined);
console.log(a);
console.log(any("hello").slice(2, 4));
// We call it push so that there is some ambiguity
let o = any({ push: returns_twelve });
console.log(o.push(undefined));

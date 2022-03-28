function inner() {
    return 5;
}
function outer() {
    return inner();
}
console.log(outer());

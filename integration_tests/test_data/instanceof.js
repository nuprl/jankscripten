// Blocking --typeinf: ! support (elimination form overrides)

function MyObj() {
    this.everything = "everything";
    return this;
}
function OtherObj() {
    return this;
}

function assert(test, what) {
    if (!test) {
        console.log("assertion failed: " + what);
    }
}

var x = new MyObj();
assert(x instanceof MyObj, "simple instance");
var y = Object.create(x);
assert(y instanceof MyObj, "two levels");
var z = Object.create(MyObj.prototype);
assert(z instanceof MyObj, "non-new");
assert(!(x instanceof OtherObj), "not an instance");

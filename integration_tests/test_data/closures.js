function makeAdder(x) {
    return function(y) {
        return x + y;
    }
}

log_any(makeAdder(4)(7));

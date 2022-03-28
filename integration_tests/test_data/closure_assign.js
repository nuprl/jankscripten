function sillyAdder(x) {
    return function(y) {
        x += 1;
        return x + y;
    }
}

var adder = sillyAdder(3);
log_any(adder(4));
log_any(adder(4));

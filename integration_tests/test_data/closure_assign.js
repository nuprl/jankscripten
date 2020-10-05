function sillyAdder(x) {
    // TODO(luna): fix param boxing so we don't need this
    var realX = x;
    return function(y) {
        realX += 1;
        return realX + y;
    }
}

var adder = sillyAdder(3);
log_any(adder(4));
log_any(adder(4));

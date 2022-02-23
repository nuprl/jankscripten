//! -t

// slow primes using nested loop
var max = 30;
log_any(2);
outer: for (var candidate = 3; candidate < max; candidate += 2) {
    // TODO(luna): sqrt
    for (var divisor = 2; divisor < candidate / 2; ++divisor) {
        if (candidate % divisor == 0) {
            continue outer;
        }
    }
    log_any(candidate);
}

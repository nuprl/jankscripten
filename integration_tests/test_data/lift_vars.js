//! -t
// x will be lifted, so undefined here
var early = x;
// later, declare x, and define it to make sure the assignment isn't lifted
var x = 5;
log_any(early);

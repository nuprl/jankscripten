var i = 0;
// redundant definition because undefined is broken
for (i=0; i<100; ++i) {
    if (9 < i) {
        break;
    }
}
log_any(i);

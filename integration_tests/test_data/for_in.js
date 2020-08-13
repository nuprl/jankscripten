let x = { a: 1, b: 2, c: 3 };

for (var prop in x) {
  log_any(x[prop]);
}

/*
var x00 = {
  a: 1,
  b: 2,
  c: 3
};
let it_obj0 = x00;

loop_break0: while (it_obj0 !== null) {
  loop_continue0: {
    const keys0 = Object.keys(it_obj0);
    let idx0 = 0;

    loop_break1: while (idx0 < keys0.length) {
      loop_continue1: {
        var prop = keys0[idx0];
        let app0 = log_and(x00[prop]);
      }

      idx0++;
    }

    let app1 = Object.getPrototypeOf(it_obj0);
    it_obj0 = app1;
  }
}
*/
// Blocking --typeinf: no explicit return
function ObjOfAges() {
    this.x = 5;
}

var myObj = new ObjOfAges();
log_any(myObj.x);

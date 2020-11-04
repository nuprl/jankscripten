function ObjOfAges() {
    this.x = 5;
}

ObjOfAges.prototype = {};

var myObj = new ObjOfAges();
log_any(myObj.x);

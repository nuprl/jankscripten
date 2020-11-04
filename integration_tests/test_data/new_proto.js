// Testing the `new` operator with its
// prototype/inheritance semantics.

// This test makes sure that objects created with the
// `new` operator inherit the prototype of their constructor
// function.

function Person(name, age) {
    this.name = name;
    this.age = age;
}

Person.prototype = {};
Person.prototype.introduceSelf = function() {
    log_any("Hello, I'm")
    log_any(this.name);
    log_any("My age is")
    log_any(this.age);
}

let jane = new Person("Jane", 32);

// Since objects inherit from their constructors' prototypes,
// Jane should have the `introduceSelf` method.
jane.introduceSelf();
// This test ensures that field accesses on an object automatically searches
// prototype chain if the object is missing a field. This is the basis for
// inheritance in JavaScript.

let rectanglePrototype = {
    getShapeName: function() {
        return "Rectangle";
    },
    area: function(width, height) {
        return width * height;
    },
};

let coloredRectanglePrototype = {
    // Override getShapeName
    getShapeName: function() {
        return "Colored Rectangle";
    },
    // New method getColor
    getColor: function() {
        return "Red";
    },
};

// Create the colored rectangle object and set up its prototype chain.
let coloredRectangle = {};
coloredRectangle.__proto__ = coloredRectanglePrototype;
coloredRectangle.__proto__.__proto__ = rectanglePrototype;

// Try calling the three methods we defined.
log_any(coloredRectangle.getShapeName()); // Colored Rectangle
log_any(coloredRectangle.area(5, 4))      // 20
log_any(coloredRectangle.getColor())      // Red

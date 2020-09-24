import * as process from 'process';

/**
 * Keeps track of all bad behavior observed in the program so far.
 * It maps each location to the set of bad behavior observed there.
 */
type BadBehavior = Map<string, Set<string>>;

// Different types of bad behavior have their own maps.

const badArityBehavior: BadBehavior = new Map();

const badOperands: BadBehavior = new Map();

const expectedNumber: BadBehavior = new Map();

const platypusTypes: BadBehavior = new Map();

const exceptions: BadBehavior = new Map();

const prototypeChanges: BadBehavior = new Map();

// Needed to keep track of all prototype objects.
// Map: prototype objects ~~> the location they were bound as a prototype
const prototypeObjects: WeakMap<any, string> = new WeakMap();

// Runtime functions

/**
 * Record an instance of bad behavior.
 * @param theMap the BadBehavior map to record to
 * @param location where this happened
 * @param message a description of the bad behavior
 */
function record(theMap: BadBehavior, location: string, message: string): void {
    let existingMessages = theMap.get(location);
    if (existingMessages === undefined) {
        theMap.set(location, new Set([message]));
    }
    else {
        existingMessages.add(message);
    }
}

// Public functions that record specific types of bad behavior.

/**
 * Expect the given value to be a number. Record bad behavior if it isn't.
 * @param loc the location where value appears
 * @param value the value that should be a number
 * @returns the given value
 */
export function expectNumber(loc: string, value: any): any {
    if (typeof value !== 'number') {
        record(expectedNumber, loc, typeof value);
    }
    return value;
}

/**
 * Expect the given value to be a primitive, used in a binary operator.
 * Record bad behavior if it isn't.
 * @param loc the location where the value appears
 * @param value the value that should be a primitive
 * @returns the given value
 */
export function checkOperand(loc: string, value: any): any {
    if (typeof value === 'object' || typeof value === 'function') {
        record(badOperands, loc, 'received an object or function');
    }
    return value;
}

/**
 * Ensure the number of formal arguments matches the number of given arguments.
 * Record bad behavior if they don't match.
 * @param loc the location of the function
 * @param numFormals the number of formal arguments to the function
 * @param numActuals the number of actual arguments to the function
 */
export function checkArgs(loc: string, numFormals: number, numActuals: number): void {
    if (numFormals !== numActuals) {
        record(badArityBehavior, loc, `received ${numActuals} actual arguments (${numFormals} formal arguments)`);
    }
}

/**
 * Ensure the object field access matches the expected behavior for the given
 * object.
 * Two cases are checked:
 * 1. an array being used as an object
 * 2. an object being used as an array
 * @param loc the location of the function
 * @param numFormals the number of formal arguments to the function
 * @param numActuals the number of actual arguments to the function
 * @returns the value of the field access
 */
export function checkPlatypus(loc: string, obj: any, property: any, isCalled: boolean): any {
    const arrayPrototype = [
        // length isn't actually part of the array prototype, it's extremely special,
        // but we still don't want to include it because we'll be specially handling it
        // Array.prototype
        "length", "concat", "copyWithin", "entries", "every", "fill",
        "filter", "find", "findIndex", "flat", "flatMap", "forEach",
        "includes", "indexOf", "join", "keys", "lastIndexOf", "map",
        "pop", "push", "reduce", "reduceRight", "reverse", "shift",
        "slice", "some", "sort", "splice", "toLocaleString", "toSource",
        "toString", "unshift", "values",
        // so begins String.prototype
        "anchor", "big", "blink", "bold", "charAt", "charCodeAt",
        "codePointAt", "concat", "endsWith", "fixed", "fontcolor",
        "fontsize", "includes", "indexOf", "italics", "lastIndexOf",
        "link", "localeCompare", "match", "matchAll", "normalize", "padEnd",
        "padStart", "repeat", "replace", "replaceAll", "search", "slice",
        "small", "split", "startsWith", "strike", "sub", "substr", "substring",
        "sup", "toLocaleLowerCase", "toLocaleUpperCase", "toLowerCase",
        "toSource", "toString", "toUpperCase", "trim", "trimEnd", "trimStart",
        "valueOf",
    ];
    // this indicates object-access of an object that also serves as an array
    if (typeof property !== "number") {
        if (obj instanceof Array && !(arrayPrototype.includes(property))) {
            let asString = ("" + property).substring(0, 50);
            record(platypusTypes, loc, `was array, accessed property: ${asString}`);
        }
    } else {
        // this indicates array-access of an object that isn't an array
        if (!(obj instanceof Array)) {
            record(platypusTypes, loc, `was object, but used as array`);
        }
    }

    // fix the return value, in the case that it's immediately called as a
    // function
    var rv = obj[property];
    if (typeof rv === "function" && isCalled) {
        // by returning obj[property] we un-bind the `this` so it'll be the
        // global object instead of obj because it doesn't appear in context of
        // property access. so we bind it so it'll last through the
        // return. this issue actually happens in practice. however, if the
        // result is *not* called, it *should* then be un-bound. so we only
        // bind if it's immediately called
        rv = rv.bind(obj);
    }
    return rv;
}

/**
 * Record an exception that occurred as bad behavior.
 * @param loc the location where the exception was caught
 */
export function checkException(loc: string): void {
    record(exceptions, loc, `exception`);
}

/**
 * Check an object property write to make sure the object isn't swapping out
 * its prototype for another.
 * 
 * Example:
 *     obj.__proto__ = {};    ~~>    checkForProtoSwap(loc, "__proto__")
 *             ^                                     ^
 *        (this code)   should trigger  (this runtime function call)
 * 
 * and the runtime function call will record this as bad behavior.
 * 
 * @param loc the location where this property write occurred
 * @param property the name of the property being written to
 * @returns the name of the property being written to
 */
export function checkForProtoSwap(loc: string, property: any): any {
    if (property === "__proto__") {
        record(prototypeChanges, loc, `prototype changed via property write`);
    }

    return property;
}

/**
 * Record that an object changed its prototype by calling
 * `Object.setPrototypeOf`.
 * 
 * The location will be inferred.
 */
export function recordPrototypeChange(): void {
    record(prototypeChanges, Error().stack as string, `prototype of object changed via Object.setPrototypeOf`);
}

/**
 * Instrument the construction of a new object. A regular object construction,
 * `new Array(1, 2)`, would result in a call to this function,
 * `checkNewObject(loc, Array, [1, 2])
 * @param loc the location where this object was created
 * @param constructor the constructor function
 * @param args the arguments to the constructor function
 * @returns the new object
 */
export function checkNewObject(loc: string, constructor: any, args: any[]): any {
    trackPrototype(loc, constructor.prototype);
    return new constructor(...args);
}

export function checkPropDelete(loc: string, object: any, property: string): any {
    if (isPrototype(object)) {
        record(prototypeChanges, loc, `property DELETED on prototype object`);
    }
    delete object[property];
}

export function checkPropWrite(loc: string, object: any, property: string, value: any): any {
    // see PrototypeVisitor.ts for explanation of cases

    // is the property being written to either of these special ones?
    let propIs__proto__ = property === "__proto__";

    // case (1.1)
    if (propIs__proto__) {
        record(prototypeChanges, loc, `prototype of object change via property write`);
        trackPrototype(loc, value);
    }

    // handles case (1.2)
    if (isPrototype(object)) {
        // check if this prototype is monotonically growing (1.2.2) or not (1.2.1)

        let childObjectLocation = prototypeObjects.get(object);
        if (object[property] !== undefined && object[property] !== value) {
            record(prototypeChanges, loc, `property MODIFIED on prototype object. object was assigned as prototype at ${childObjectLocation}`);
        } else {
            record(prototypeChanges, loc, `prototype object monotonically grew by adding a property. object was assigned as prototype at ${childObjectLocation}`);
        }

    }

    // actually perform the property write and return its value
    return object[property] = value;
}

/**
 * Tells the runtime that the given object is a prototype of another object.
 * @param loc the location where `o` was assigned as a prototype
 * @param o the prototype object
 */
export function trackPrototype(loc: string, o: any): void {
    try {
        // track this prototype at the given location if we haven't already
        if (!prototypeObjects.has(o)) {
            prototypeObjects.set(o, loc);
        }
    } catch(e) {
        // if this failed, the prototype was probably set to null or undefined.
        // this is perfectly valid, and it just means we can't track changes to
        // it.
    }
}

/**
 * Is the given object a prototype of another object?
 * @param o the object to check
 */
function isPrototype(o: any): boolean {
    return prototypeObjects.has(o);
}

// Display the logs of bad behavior to the console.
process.on('beforeExit', () => {
    console.error(badArityBehavior);
    console.error(badOperands);
    console.error(expectedNumber);
    console.error(platypusTypes);
    console.error(exceptions);
    console.error(prototypeChanges);
});

import * as process from 'process';

type BadBehavior = Map<string, Set<string>>;

const badArityBehavior: BadBehavior = new Map();

const badOperands: BadBehavior = new Map();

const expectedNumber: BadBehavior = new Map();

const platypusTypes: BadBehavior = new Map();

function record(theMap: BadBehavior, location: string, message: string) {
    let existingMessages = theMap.get(location);
    if (existingMessages === undefined) {
        theMap.set(location, new Set([message]));
    }
    else {
        existingMessages.add(message);
    }
}

export function expectNumber(loc: string, value: any): any {
    if (typeof value !== 'number') {
        record(expectedNumber, loc, typeof value);
    }
    return value;
}

export function checkOperand(loc: string, value: any): any {
    if (typeof value === 'object' || typeof value === 'function') {
        record(badOperands, loc, 'received an object or function');
    }
    return value;
}

export function checkArgs(loc: string, numFormals: number, numActuals: number) {
    if (numFormals !== numActuals) {
        record(badArityBehavior, loc, `received ${numActuals} actual arguments (${numFormals} formal arguments)`);
    }
}

export function checkPlatypus(loc: string, obj: any, property: any, isCalled: boolean) {
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
        if (!(obj instanceof Array)) {
            record(platypusTypes, loc, `was object, but used as array`);
        }
    }
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

process.on('beforeExit', () => {
    console.error(badArityBehavior);
    console.error(badOperands);
    console.error(expectedNumber);
    console.error(platypusTypes);
});

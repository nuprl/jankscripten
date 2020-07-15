import * as process from 'process';

type BadBehavior = Map<string, Set<string>>;

const badArityBehavior: BadBehavior = new Map();
const badOperands: BadBehavior = new Map();

function record(theMap: BadBehavior, location: string, message: string) {
    let existingMessages = theMap.get(location);
    if (existingMessages === undefined) {
        theMap.set(location, new Set([message]));
    }
    else {
        existingMessages.add(message);
    }
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

process.on('beforeExit', () => {
    console.log(badArityBehavior);
    console.log(badOperands);
});
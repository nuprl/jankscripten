// This script uses Node to run a program produced by jankscripten, and takes
// care of linking with the runtime system. Since Node uses V8, and V8 is
// deployed in several web browsers, this is the right platform to compare
// our work with JavaScript.
const fs = require('fs');
const process = require('process');
const assert = require('assert');
const path = require('path');

let build = "debug";
if (process.argv.length === 4 && process.argv[3] === "--release") {
    build = "release";
} else {
    assert(process.argv.length === 3);
}

// Allows us to call this script from any directory.
const runtimePath = path.normalize(path.join(path.dirname(process.argv[1]),
    `../target/wasm32-unknown-unknown/${build}/runtime.wasm`));

// keep a WebAssembly memory reference for `readString`
let memory;

const imports = {
    env: {
        jankscripten_log: function(startOffset) {
            let view = new Uint8Array(memory.buffer, startOffset);
            let len = 0;
            // This will infinite loop if the string is not null-terminated.
            while (view[len] !== 0) {
                len += 1;
            }
            view = new Uint8Array(memory.buffer, startOffset, len);
            console.log((new TextDecoder()).decode(view));
        }
    },
};

async function main(filename) {
    if (filename === '-') {
        filename = process.stdin.fd;
    }
    const programBuf = fs.readFileSync(filename);
    const runtimeBuf = fs.readFileSync(runtimePath);
    const runtimeModule = await WebAssembly.compile(runtimeBuf);
    const programModule = await WebAssembly.compile(programBuf);
    const runtimeInstance = await WebAssembly.instantiate(runtimeModule, imports);
    const programInstance = await WebAssembly.instantiate(programModule, {
        runtime: runtimeInstance.exports
    });
    const exports = runtimeInstance.exports;
    memory = exports.memory;
    const startTime = Date.now();
    const result = programInstance.exports.main();
    const endTime = Date.now();
    console.error(`Running time: ${endTime - startTime}ms`);
    return result;
}


main(process.argv[2])
    .catch(err => {
        console.error(err);
        process.exit(1);
    })
    .then(result => {
        if (typeof result !== "undefined") {
            console.log(result);
        }
    });

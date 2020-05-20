const { readFileSync } = require("fs");

const run = async () => {
  const wasmFile = process.argv[2];
  const buffer = readFileSync(wasmFile);
  const module = await WebAssembly.compile(buffer);
  const instance = await WebAssembly.instantiate(module);
  console.log(instance.exports.helloWorld());
};

run();


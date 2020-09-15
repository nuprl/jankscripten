# jankyp

The purpose of *jankyp* is to help you understand if *jankscripten* is likely to
work on a compiler's JavaScript output. Jankyp instruments JavaScript programs
to print warnings when they exhibit behavior that jankscripten either does
not support, or supports with poor quality code.

At the moment, it prints the following warnings:

1. A function receives more or fewer arguments than it has declared in its
   formal argument list.
2. A binary operator receives a object or function, thus the correct behavior
   is to call `.toString()` or `.valueOf()`.
3. A numeric operator (multiplication, subtraction, etc.) receives a non-numeric
   operand.

## Building

```
npm install
npm run-script build
```

# Usage

```
npm run-script run PROGRAM.js PROGRAM.instrumented.js [analysis features...]
node PROGRAM.instrument.js
```

Example:
```
npm run-script run PROGRAM.js PROGRAM.instrumented.js platypus arguments
node PROGRAM.instrumented.js
```

The file `PROGRAM.instrumented.js` must be in this directory, because the instrumentation
assumes that the runtime system is located at `./dist/runtime.js`.
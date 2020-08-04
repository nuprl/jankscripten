This is a simple tool that we use to run end-to-end tests (integration tests)
for jankscripten. Each test uses "jankscripten compile" to compile JavaScript
programs to WebAssembly, "run-node" to run the program, and finally compares
the standard output of the program to captured output file. (Note that
we ignore the standard error.)

Building
--------

Prerequisites: ensure the jankscripten compiler and runtime system are already
built.

In this directory, run "npm install".

Usage
-----

To run the tests, run "npx jest".

To add a new test, create test_data/FILENAME.js and test_data/FILENAME.txt.
There is no need to re-build the tool after you add the test.

Note that when a test fails, we do not delete the generated WebAssembly file
(test_data/FILENAME.wasm) to aid debugging. Do not commit these to the
repository.
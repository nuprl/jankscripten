const fs = require('fs');
const cp = require('child_process');

const testDir = 'test_data';

function makeTest(filename) {
    test(filename, () => {
        // Note that cp.execSync throws on a non-zero exit code.
        let jsPath = `${testDir}/${filename}`;
        let wasmPath = jsPath.replace(/\.js$/, '.wasm');
        let expectedOutputPath = jsPath.replace(/.js$/, '.txt');
    
        // Use -j for jankyscript output
        // Use -n for notwasm output
        cp.execSync(`../bin/jankscripten compile -o ${wasmPath} ${jsPath}`,
            { stdio: 'inherit' });

        let output;
        try {
            output = String(cp.execSync(`../bin/run-node ${wasmPath}`)).trim();
        } catch (e) {
            // jest is very stubborn about printing in the right spot only
            // if it's a thrown error, and the captured stdout won't be seen
            // if we don't stick it in here
            throw new Error(
`stdout:
${e.stdout}
stderr:
${e.stderr}`);
        }
    
        let expectedOutput = String(fs.readFileSync(expectedOutputPath)).trim();
        expect(output).toBe(expectedOutput);
        fs.unlinkSync(wasmPath);
    });
}

fs.readdirSync(testDir)
    .filter(filename => filename.endsWith('.js'))
    .forEach(filename => makeTest(filename));

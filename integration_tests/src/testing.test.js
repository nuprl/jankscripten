const fs = require('fs');
const cp = require('child_process');
const path = require('path');

const testDir = 'test_data';

// Normalize string.
function normalizeStr(text) {
  return text.replace(/\r\n/g, '\n').trim();
}

function makeTest(filename) {
    test(filename, () => {
        // Note that cp.execSync throws on a non-zero exit code.
        let jsPath = `${testDir}/${filename}`;
        // Replace the extension .js or .notwasm with .wasm.
        let wasmPath = jsPath.replace(/\.js$|\.notwasm$/, '.wasm');
        let expectedOutputPath = jsPath.replace(/\.js$|\.notwasm$/, '.txt');
    
        // Use -j for jankyscript output
        // Use -n for notwasm output

        let root = path.dirname(path.dirname(__dirname));
        let jankscriptenPath = path.join(root, 'target', 'debug', 'jankscripten');

        cp.spawnSync(jankscriptenPath, ['compile', '-o', path.join(wasmPath), path.join(jsPath)], { stdio: 'inherit' });
        let output;
        try {
            output = String(cp.execSync(`node ../bin/run-node ${wasmPath}`, { stderr: 'inherit' })).trim();
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
    
        output = normalizeStr(output);
        let expectedOutput = normalizeStr(String(fs.readFileSync(expectedOutputPath)));
        expect(output).toBe(expectedOutput);
        fs.unlinkSync(wasmPath);
    });
}

fs.readdirSync(testDir)
    .filter(filename => filename.endsWith('.js') || filename.endsWith('.notwasm'))
    .forEach(filename => makeTest(filename));

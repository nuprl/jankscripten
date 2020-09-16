import * as parser from '@babel/parser';
import generator from '@babel/generator';
import * as t from '@babel/types';
import traverse, { TraverseOptions, NodePath, Visitor } from '@babel/traverse';
import * as process from 'process';
import * as fs from 'fs';
import { visitors } from './visitors';
import { JankyPVisitor, qJankyp } from './visitors/JankypVisitor';

function main() {
    // ensure correct usage
    if (process.argv.length !== 5) {
        usage();
        process.exit(1);
    }

    // select visitor based on user input
    let visitor = findVisitor(process.argv[4]);

    // read js file in
    let js_str = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });

    // parse it
    let ast = parser.parse(js_str);

    // insert selected instrumentation
    traverse(ast, visitor.visitor);
    let { code } = generator(ast);

    // bind jankyp runtime in the instrumented code
    traverse(ast, {
        Program: {
            exit(path) {
                path.node.body.unshift(
                    t.variableDeclaration('const',
                        [t.variableDeclarator(qJankyp,
                            t.callExpression(t.identifier('require'), [t.stringLiteral('./dist/runtime.js')]))]));
            }
        }
    })

    // write out the instrumented program
    fs.writeFileSync(process.argv[3], code);
}

function usage() {
    console.log(`USAGE:
    npm run-script run PROGRAM.js PROGRAM.instrumented.js analysis
    node PROGRAM.instrument.js

Supported analysis features (located in ./src/visitors/*):`);

    visitors.forEach(visitor => console.log(`    - ${visitor.name}`));
}

function findVisitor(name: string): JankyPVisitor {
    return visitors.filter(v => v.name == name)[0];
}

main();

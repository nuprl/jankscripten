import * as parser from '@babel/parser';
import * as t from '@babel/types';
import generator from '@babel/generator';
import traverse, { TraverseOptions, NodePath, Visitor } from '@babel/traverse';
import * as process from 'process';
import * as fs from 'fs';
import { memoryUsage } from 'process';
import { visitors } from './visitors';
import { ArgumentsVisitor } from './visitors/ArgumentsVisitor';

/////// SELECT YOUR INSTRUMENTATION TYPE HERE:
const visitor = ArgumentsVisitor;

// you can choose any visitor from './visitors/*', or merge multiple visitors:
//     `traverse.visitors.merge(...)`
//
// `visitors` contains a list of `JankypVisitor`'s, if you want to let the user
// choose from the console.

function main() {
    // read js file in
    let js_str = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });

    // parse it
    let ast = parser.parse(js_str);

    // insert selected instrumentation
    traverse(ast, visitor.visitor);
    let { code } = generator(ast);

    // write out the instrumented program
    fs.writeFileSync(process.argv[3], code);
}

main();

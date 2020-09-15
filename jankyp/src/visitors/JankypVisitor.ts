import * as t from '@babel/types';
import traverse, { TraverseOptions, NodePath, Visitor } from '@babel/traverse';

export interface JankyPVisitor {
    name: String;
    visitor: Visitor;
}

/**
 * An identfier in the generated code. It is bound to the jankyp runtime 
 * module, `runtime.ts`.
 */
export const qJankyp = t.identifier('$jankyp');

/**
 * Call a function in the jankyp runtime.
 * @param name name of the runtime function
 * @param args args to the runtime function
 */
export function qCall(name: string, args: t.Expression[]): t.CallExpression {
    return t.callExpression(t.memberExpression(qJankyp, t.identifier(name), false), args);
}

/**
 * Converts a babel source location into a generated JS string.
 * @param loc the location to convert
 */
export function qLoc(loc: t.SourceLocation): t.Expression {
    return t.stringLiteral(`Line ${loc.start.line}, Column ${loc.start.column}`);
}

/**
 * Sets up a function object to instrument itself. Currently the
 * generated instrumentation checks the functions' arguments for
 * arity mismatches.
 * @param path the babel function object
 */
export function instrumentFunction(path: NodePath<t.FunctionDeclaration> | NodePath<t.FunctionExpression>) {
    let numFormals = t.numericLiteral(path.node.params.length);
    let numActuals = t.memberExpression(t.identifier('arguments'), t.identifier('length'), false);
    if (path.node.loc === null) {
        throw new Error('no location');
    }
    path.node.body.body.unshift(t.expressionStatement(qCall('checkArgs', [qLoc(path.node.loc), numFormals, numActuals])));
}

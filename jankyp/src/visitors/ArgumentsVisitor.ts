import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const ArgumentsVisitor: JankyPVisitor = {
    name: "arguments",

    visitor: {

        FunctionExpression: {
            exit(path) {
                instrumentFunction(path);
            }
        },
        FunctionDeclaration: {
            exit(path) {
                instrumentFunction(path);
            }
        }
    },
};

/**
 * Sets up a function object to instrument itself. Currently the
 * generated instrumentation checks the functions' arguments for
 * arity mismatches.
 * @param path the babel function object
 */
function instrumentFunction(path: NodePath<t.FunctionDeclaration> | NodePath<t.FunctionExpression>) {
    let numFormals = t.numericLiteral(path.node.params.length);
    let numActuals = t.memberExpression(t.identifier('arguments'), t.identifier('length'), false);
    if (path.node.loc === null) {
        throw new Error('no location');
    }
    path.node.body.body.unshift(t.expressionStatement(qCall('checkArgs', [qLoc(path.node.loc), numFormals, numActuals])));
}
import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc, qJankyp} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";
import * as parser from '@babel/parser';

export const PrototypeVisitor: JankyPVisitor = {
    name: "prototype",

    visitor: {
        Program: {
            exit(path) {
                // Monkey patch Object.setPrototypeOf to record this bad behavior

                let monkeyPatch = `
                let $jankyp_old_Object_setPrototypeOf = Object.setPrototypeOf;
                Object.setPrototypeOf = function(obj, proto) {
                    ${qJankyp.name}.recordPrototypeChange();
                    $jankyp_old_Object_setPrototypeOf(obj, proto);
                }
                `;

                let monkeyPatchStmts = parser.parse(monkeyPatch).program.body;

                path.node.body.unshift(...monkeyPatchStmts);
            }
        },
        AssignmentExpression: {
            exit(path) {
                if (path.node.loc === null) {
                    return;
                }
                if (t.isMemberExpression(path.node.left)) {
                    // this assignment expression could be computed or non-computed.
                    // the result of this transformation is going to be computed,
                    // so if it's non-computed, we have to make that change.
                
                    let property: t.Expression;

                    // make sure it's properly quoted
                    if (path.node.left.computed) {
                        property = path.node.left.property as t.Expression;
                    } else if (t.isIdentifier(path.node.left.property)) {
                        property = t.stringLiteral(path.node.left.property.name)
                    } else {
                        throw new Error(`unsupported member expression type: ${path.node.left.type}`);
                    }

                    let instrumentedProperty = qCall('checkPropWriteForProtoChange', [qLoc(path.node.loc), property])

                    // path.replaceWith(t.memberExpression(path.node.left.object, instrumentedProperty, true));
                    path.node.left.property = instrumentedProperty;
                    path.node.left.computed = true;
                    path.skip();
                }
            }
        },
    }
}

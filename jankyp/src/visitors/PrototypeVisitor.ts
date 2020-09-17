import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc, qJankyp} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";
import * as parser from '@babel/parser';

/**
 * This visitor is designed to detect bad behavior around object prototypes.
 * Jankscripten's eventual goal is to implement prototypes using a method
 * similar to vtables. However, we want to avoid *deoptimizing* these vtables
 * at runtime, which we would have to do in order to support full prototypes
 * like V8.
 * 
 * Since we want to avoid deoptimization, this visitor detects 2 forms of bad
 * behavior that would cause deoptimization:
 * 
 * 1. *Changes in the shape of prototype chains.* That is, once an object is
 *    created with a particular prototype, no links between these objects 
 *    should be modified. An example of this type of behavior is:
 * 
 *        let o = new Object(); Object.setPrototypeOf(o, Array.prototype);
 * 
 * 2. *Modification of properties in prototype objects.* Once an object becomes
 *    a prototype, its properties must not change. No functions can be added,
 *    removed, or modified. It's important to note that only shallow changes
 *    would trigger deoptimization, as the vtables only store shallow pointers
 *    to their properties. Here's an example of this type of bad behavior:
 * 
 *        let r = new Rectangle(2, 2); Rectangle.prototype.extraMethod = ...;
 *
 */
export const PrototypeVisitor: JankyPVisitor = {
    name: "prototype",

    visitor: {
        Program: {
            exit(path) {
                // Monkey patch Object.setPrototypeOf to record this bad behavior

                let monkeyPatch = `
                let $jankyp_old_Object_setPrototypeOf = Object.setPrototypeOf;
                Object.setPrototypeOf = function(obj, proto) {
                    (${qJankyp.name}).recordPrototypeChange();
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

                    let instrumentedProperty = qCall('checkForProtoSwap', [qLoc(path.node.loc), property])

                    // path.replaceWith(t.memberExpression(path.node.left.object, instrumentedProperty, true));
                    path.node.left.property = instrumentedProperty;
                    path.node.left.computed = true;
                    path.skip();
                }
            }
        },
    }
}

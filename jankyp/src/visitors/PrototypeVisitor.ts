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
 * 
 * Here are the different syntax forms we instrument to observe the relevant
 * changes to objects and their prototypes:
 * 
 * 1. property write: `o.p = v;`
 *     1. if `p` is "__proto__", `o` has changed its prototype (bad behavior).
 *     2. if `o` is the prototype of another object, a prototype object has
 *        modified one of its properties (bad behavior).
 *     3. if `p` is "prototype" or "__proto__", `v` is a prototype object.
 * 
 * 2. property deletion: `delete o.p;`
 *     1. if `o` is a prototype object, a prototype object has modified one of
 *        its properties (bad behavior).
 * 
 * 3. object creation with constructor: `new F()`
 *     1. `F.prototype` is a prototype object.
 * 
 * 4. `Object.setPrototypeOf(object, prototype)`
 *     1. `prototype` is a prototype object.
 * 
 * 5. `Object.create(o)`
 *     1. `o` is a prototype object.
 *
 */
export const PrototypeVisitor: JankyPVisitor = {
    name: "prototype",

    visitor: {
        Program: {
            exit(path) {
                // Deals with cases (4) and (5).
                // Monkey patch Object.setPrototypeOf to record this bad behavior.
                // Also monkey patch Object.create to track prototype objects.

                let monkeyPatch = `
                let $jankyp_old_Object_setPrototypeOf = Object.setPrototypeOf;
                let $jankyp_old_Object_create = Object.create;
                Object.setPrototypeOf = function(obj, proto) {
                    (${qJankyp.name}).recordPrototypeChange();
                    return $jankyp_old_Object_setPrototypeOf(obj, proto);
                }
                Object.create = function(proto, propertiesObject) {
                    (${qJankyp.name}).trackPrototype("", proto);
                    return $jankyp_old_Object_create(proto, propertiesObject);
                }
                `;

                let monkeyPatchStmts = parser.parse(monkeyPatch).program.body;

                path.node.body.unshift(...monkeyPatchStmts);
            }
        },
        AssignmentExpression: {
            // obj.__proto__ = ???;
            // obj[field] = ???; => obj[check(field)] = ???;
            exit(path) {
                // to make typescript happy. i don't think it's possible for
                // loc to be null.
                if (path.node.loc === null) {
                    return;
                }

                // o.p = v;
                // 1. o[checkField(p)] = v;
                // 2. (checkForProtoModification(o))[checkField(p)] = v;
                // 3. (fieldWrite(o, p, v))
                
                if (t.isMemberExpression(path.node.left)) {
                    let [object, property] = decomposeMemberExpression(path.node.left);

                    let assignedValue = path.node.right;

                    let loc = qLoc(path.node.loc);

                    // the call to the runtime function that will replace this
                    // property write
                    let runtimeCall = qCall('checkPropWrite', [loc, object, property, assignedValue]);

                    path.replaceWith(runtimeCall);
                    // path.skip();
                }
            }
        },
        NewExpression: {
            exit(path) {
                // to make typescript happy. i don't think it's possible for
                // loc to be null.
                if (path.node.loc === null) {
                    return;
                }

                // also to make typescript happy..
                if (path.node.callee.type === "V8IntrinsicIdentifier") {
                    return;
                }

                let loc = qLoc(path.node.loc);
                let runtimeCall = qCall('checkNewObject', [loc, path.node.callee, t.arrayExpression(path.node.arguments as unknown as t.Expression[])])

                path.replaceWith(runtimeCall);
                // path.skip();
            }
        },
        UnaryExpression: {
            exit(path) {
                // we're only interested in unary expressions if we're deleting a property
                if (path.node.operator !== "delete") {
                    return;
                }

                // i think this is the only type of property deletion that
                // actually has property deletion semantics (as opposed to just
                // returning true)
                if (path.node.argument.type !== "MemberExpression") {
                    return;
                }

                // to make typescript happy. i don't think it's possible for
                // loc to be null.
                if (path.node.loc === null) {
                    return;
                }

                let [object, property] = decomposeMemberExpression(path.node.argument);

                let loc = qLoc(path.node.loc);
                let runtimeCall = qCall('checkPropDelete', [loc, object, property]);

                path.replaceWith(runtimeCall);
                // path.skip();

            }
        }
    }
}

/**
 * Decompose a member expression into its 2 parts: the object and the property.
 * Both will be valid JS expressions.
 * @param m the member expression to decompose
 */
function decomposeMemberExpression(m: t.MemberExpression): [t.Expression, t.Expression] {
    let property: t.Expression;

    // make sure it's properly quoted
    if (m.computed) {
        property = m.property as t.Expression;
    } else if (t.isIdentifier(m.property)) {
        property = t.stringLiteral(m.property.name)
    } else {
        throw new Error(`unsupported member expression type: ${m.type}`);
    }

    return [m.object, property];
}

import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const PlatypusVisitor: JankyPVisitor = {
    name: "platypus",

    visitor: {
        MemberExpression: {
            exit(path) {
                // undefineds are statements we inserted; which is fine (important even!)
                // in this case, but i'm not sure if that's a problem with other insertions
                // that might be added to jankyp
                // TODO(luna): technically as a result obj.x.y will never check
                // if `obj` is platypus, only `obj.x`
                // TODO(luna): detect when used as lvals
                // including the property access in the check method is a hack that works for rvals only
                // in order to properly support lvals i think we'd need to desugar logical operators
                // and then we could insert object and property as fresh names
                if (path.node.loc === null || typeof path.node.loc === "undefined" || isLVal(path)) {
                    return;
                }
                let property;
                if (path.node.computed == false) {
                    property = t.stringLiteral((path.node.property as t.Identifier).name);
                }
                else {
                    property = path.node.property as t.Expression;
                }
                // see the runtime to understand this garbage
                let isCalled = t.booleanLiteral(immediatelyCalled(path));
                path.replaceWith(qCall('checkPlatypus', [qLoc(path.node.loc), path.node.object, property, isCalled]));
            }
        },
    }
}

// t.isLVal actually returns true for anything that *can* be an LVal
function isLVal(path: NodePath<t.MemberExpression>): boolean {
    let parent = path.parentPath;
    let assign = parent.isAssignmentExpression() && parent.node.left == path.node;
    // TODO(luna): should be able to detect these
    let update = parent.isUpdateExpression();
    return assign || update;
}

function immediatelyCalled(path: NodePath<t.MemberExpression>): boolean {
    let parent = path.parentPath;
    return parent.isCallExpression();
}
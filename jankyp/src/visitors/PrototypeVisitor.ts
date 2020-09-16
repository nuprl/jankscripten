import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const PrototypeVisitor: JankyPVisitor = {
    name: "prototype",

    visitor: {
        AssignmentExpression: {
            exit(path) {
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

                    let instrumentedProperty = qCall('checkPropWriteForProtoChange', [property])

                    // path.replaceWith(t.memberExpression(path.node.left.object, instrumentedProperty, true));
                    path.node.left.property = instrumentedProperty;
                    path.node.left.computed = true;
                    path.skip();
                }
            }
        },
    }
}

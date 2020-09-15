import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const BinaryVisitor: JankyPVisitor = {
    name: "binary",

    visitor: {
        BinaryExpression: {
            exit(path) {
                if (typeof path.node.left.loc === "undefined" || typeof path.node.right.loc === "undefined") {
                    return;
                }
                let op = path.node.operator;
                // Let's assume all (in)equalities are safe.
                if (['==', '!=', '===', '!=='].includes(op)) {
                    return;
                }
                if (path.node.left.type == 'PrivateName') {
                    // No idea what this is.
                    return;
                }
                if (path.node.left.loc === null) {
                    throw new Error('no location');
                }
                if (path.node.right.loc === null) {
                    throw new Error('no location');
                }

                if (['*', '/', '-', '&', '|', '<<', '>>', '>>>'].includes(op)) {
                    path.node.left = qCall('expectNumber', [qLoc(path.node.left.loc), path.node.left]);
                    path.node.right = qCall('expectNumber', [qLoc(path.node.right.loc), path.node.right]);
                    return;
                }
                path.node.left = qCall('checkOperand', [qLoc(path.node.left.loc), path.node.left]);
                path.node.right = qCall('checkOperand', [qLoc(path.node.right.loc), path.node.right]);
            }
        }
    },
};
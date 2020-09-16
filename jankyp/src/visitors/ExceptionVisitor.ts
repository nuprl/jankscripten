import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const ExceptionVisitor: JankyPVisitor = {
    name: "exception",

    visitor: {

        TryStatement: {
            exit(path) {
                if (path.node.loc === null) {
                    return;
                }
                if (path.node.handler !== null) {
                    path.node.handler.body.body.unshift(t.expressionStatement(qCall('checkException', [qLoc(path.node.loc)])));
                }
            }
        }
    },
};
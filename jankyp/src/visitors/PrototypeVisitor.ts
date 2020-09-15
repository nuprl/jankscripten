import * as t from '@babel/types';
import {JankyPVisitor, qCall, qLoc} from "./JankypVisitor";
import { NodePath, Visitor } from "@babel/traverse";

export const PrototypeVisitor: JankyPVisitor = {
    name: "prototype",

    visitor: {
    }
}

import * as parser from '@babel/parser';
import * as t from '@babel/types';
import generator from '@babel/generator';
import traverse, { TraverseOptions, NodePath } from '@babel/traverse';
import * as process from 'process';
import * as fs from 'fs';

const qJankyp = t.identifier('$jankyp');

var nextFresh = 0;

function fresh(desc: string) {
    return `$jankyp_${desc}_${nextFresh++}`;
}

function qCall(name: string, args: t.Expression[]) {
    return t.callExpression(t.memberExpression(qJankyp, t.identifier(name), false), args);
}

function qLoc(loc: t.SourceLocation): t.Expression {
    return t.stringLiteral(`Line ${loc.start.line}, Column ${loc.start.column}`);
}

function instrumentFunction(path: NodePath<t.FunctionDeclaration> | NodePath<t.FunctionExpression>) {
    let numFormals = t.numericLiteral(path.node.params.length);
    let numActuals = t.memberExpression(t.identifier('arguments'), t.identifier('length'), false);
    if (path.node.loc === null) {
        throw new Error('no location');
    }
    path.node.body.body.unshift(t.expressionStatement(qCall('checkArgs', [qLoc(path.node.loc), numFormals, numActuals])));
}

const visitor: TraverseOptions = {
    Program: {
        exit(path) {
            path.node.body.unshift(
                t.variableDeclaration('const',
                    [t.variableDeclarator(qJankyp,
                        t.callExpression(t.identifier('require'), [t.stringLiteral('./dist/runtime.js')]))]));
        }
    },
    BinaryExpression: {
        exit(path) {
            if (typeof path.node.left.loc === "undefined") {
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
    },
    FunctionExpression: {
        exit(path) {
            instrumentFunction(path);
        }
    },
    FunctionDeclaration: {
        exit(path) {
            instrumentFunction(path);
        }
    },
    MemberExpression: {
        exit(path) {
            // undefineds are statements we inserted; which is fine (important even!)
            // in this case, but i'm not sure if that's a problem with other insertions
            // that might be added to jankyp
            // TODO(luna): technically as a result obj.x.y will never check
            // if `obj` is platypus, only `obj.x`
            if (path.node.loc === null || typeof path.node.loc === "undefined") {
                return;
            }
            let property;
            if (path.node.computed == false) {
                property = t.stringLiteral((path.node.property as t.Identifier).name);
            }
            else {
                property = path.node.property as t.Expression;
            }
            // to avoid duplicating side effects,
            // we have to assign the object and property to fresh names
            let nodes = [];
            let obj = path.node.object;
            let prop: t.Expression = property;
            if (!t.isIdentifier(obj)) {
                let objName = fresh("obj");
                nodes.push(decl(objName, obj));
                obj = t.identifier(objName);
            }
            if (!t.isIdentifier(prop) && !t.isLiteral(prop)) {
                let propName = fresh("prop");
                nodes.push(decl(propName, prop));
                prop = t.identifier(propName);
            }
            nodes.push(t.expressionStatement(qCall('checkPlatypus', [qLoc(path.node.loc), obj, prop])));
            path.node = t.memberExpression(obj, prop, true, path.node.optional);
            path.getStatementParent().insertBefore(nodes);
        }
    },
};

function decl(name: string, exp: t.Expression): t.Statement {
    return t.variableDeclaration("const", [t.variableDeclarator(t.identifier(name), exp)]);
}

function main() {
    let js_str = fs.readFileSync(process.argv[2], { encoding: 'utf-8' });
    let ast = parser.parse(js_str);
    traverse(ast, visitor);
    let { code } = generator(ast);
    fs.writeFileSync(process.argv[3], code);
}

main();

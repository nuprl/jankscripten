%start Program
%%

U32 -> u32 :
    'INT' { parse_uint($lexer.span_str($1.unwrap().span())) }
  ;

I32 -> (i32, Pos) :
    'INT' { ($lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() , pos($1)) }
  ;

F64 -> (f64, Pos) :
    'FLOAT' 
    { 
      // drop the trailing 'f' before parsing
      let s = $lexer.span_str($1.unwrap().span());
      (s[..s.len() - 1].parse().unwrap(), pos($1))
    }
  ;

IdString -> String : 
    'ID' { $lexer.span_str($1.unwrap().span()).to_string() }
  ;

Id -> Id : 
    'ID'                  { Id::Named($lexer.span_str($1.unwrap().span()).to_string()) }
  | 'bogus' '(' 'env' ')' { Id::Bogus("env") }
  ;

IdAtom -> Atom :
    'ID' { Atom::Id(Id::Named($lexer.span_str($1.unwrap().span()).to_string()), pos($1)) }
  ;

Lit -> (Lit, Pos) :
    'true'       { (Lit::Bool(true), pos($1)) }
  | 'false'      { (Lit::Bool(false), pos($1)) }
  | 'null'       { (Lit::Null, pos($1)) }
  | I32          { (Lit::I32($1.0), $1.1) }
  | F64          { (Lit::F64($1.0), $1.1) }
  | 'STRING_LIT' { (Lit::String(unescape_string($lexer.span_str($1.unwrap().span())).unwrap()), pos($1)) }
  ;

TypeSeq -> Vec<Type> :
                     { Vec::new() }
  | Type             { vec![$1] }
  | TypeSeq ',' Type { $1.push($3); $1 }
  ;

IdSeq -> Vec<Id> :
                 { Vec::new() }
  | Id           { vec! [ $1 ] }
  | IdSeq ',' Id { $1.push($3); $1 }
  ;

FnType -> FnType :
    '(' TypeSeq ')' '->' Type   { FnType { args: $2, result: Some(Box::new($5)) } }
  | '(' TypeSeq ')' '->' 'void' { FnType { args: $2, result: None } }  
  ;

Type -> Type :
    'any'                 { Type::Any }
  | 'i32'                 { Type::I32 }
  | 'f64'                 { Type::F64 }
  | 'bool'                { Type::Bool }
  | 'str'                 { Type::String }
  | 'Array'               { Type::Array }
  | 'DynObject'           { Type::DynObject }
  | FnType                { Type::Fn($1) }
  | 'clos' FnType         { Type::Closure($2) }


  | 'HT'                  { Type::HT }

  | 'Ref' '(' Type ')'    { Type::Ref(Box::new($3)) }
  | 'env'                 { Type::Env }
  ;

AtomSeq -> Vec<Atom> :
                     { Vec::new() }
  | Atom             { vec![$1] }
  | AtomSeq ',' Atom { $1.push($3); $1 }
  ;

Atom -> Atom :
    '$' Id '(' AtomSeq ')' { Atom::PrimApp($2, $4, pos($1)) }    
  | 'any' '(' Atom ')'     { Atom::ToAny(ToAny::new($3), pos($1)) }
  | 'env' '.' U32 ':' Type { Atom::EnvGet($3, $5, pos($4)) }
  | 'rt' '(' Id ')'        { Atom::GetPrimFunc($3, pos($1)) }
  | Lit                    { Atom::Lit($1.0, $1.1) }
  // TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
  | IdAtom '.' IdString    { Atom::ObjectGet(Box::new($1), Box::new(Atom::Lit(Lit::String($3), pos($2))), pos($2)) }
  // TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
  // TODO(arjun): Should this turn into an RTS call?
  | IdAtom '<<' 'length'   { Atom::ArrayLen(Box::new($1), pos($2)) }
  // TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
  | IdAtom '<<' IdString   { Atom::HTGet(Box::new($1), Box::new(Atom::Lit(Lit::String($3), pos($2))), pos($2)) }
  | IdAtom '[' Atom ']'    { Atom::Index(Box::new($1), Box::new($3), pos($2)) }
  | IdAtom                 { $1 }
  // TODO(arjun): The type annotation on deref should not be necessary in the
  // concrete syntax. The type-checker can figure it out.
  | '*' Atom ':' Type      { Atom::Deref(Box::new($2), $4, pos($1)) }
  | Atom 'as' Type         { Atom::FromAny(Box::new($1), $3, pos($2)) }
  ;

AtomMul -> Atom :
    Atom              { $1 }
  | Atom '*' AtomMul  { binary_(BinaryOp::I32Mul, $1, $3, pos($2)) }
  | Atom '*.' AtomMul { binary_(BinaryOp::F64Mul, $1, $3, pos($2)) }
  | Atom '/.' AtomMul { binary_(BinaryOp::F64Div, $1, $3, pos($2)) }
  ;

AtomAdd -> Atom :
    AtomMul { $1 }
  | AtomMul '+' AtomAdd   { binary_(BinaryOp::I32Add, $1, $3, pos($2)) }
  | AtomMul '>' AtomAdd   { binary_(BinaryOp::I32GT, $1, $3, pos($2)) }
  | AtomMul '<' AtomAdd   { binary_(BinaryOp::I32LT, $1, $3, pos($2)) }
  | AtomMul '>=' AtomAdd  { binary_(BinaryOp::I32Ge, $1, $3, pos($2)) }
  | AtomMul '<=' AtomAdd  { binary_(BinaryOp::I32Le, $1, $3, pos($2)) }
  | AtomMul '-' AtomAdd   { binary_(BinaryOp::I32Sub, $1, $3, pos($2)) }
  | AtomMul '===' AtomAdd { binary_(BinaryOp::PtrEq, $1, $3, pos($2)) }
  | AtomMul '==' AtomAdd  { binary_(BinaryOp::I32Eq, $1, $3, pos($2)) }
  | AtomMul '+.' AtomAdd  { binary_(BinaryOp::F64Add, $1, $3, pos($2)) }
  | AtomMul '-.' AtomAdd  { binary_(BinaryOp::F64Sub, $1, $3, pos($2)) }
  ;

// TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
IdAtomTypeSeq -> Vec<(Atom, Type)> :
                                      { Vec::new() }
  | IdAtom ':' Type                   { vec![($1, $3)] }
  | IdAtomTypeSeq ',' IdAtom ':' Type { $1.push(($3, $5)); $1 }
  ;

IdTypeSeq -> Vec<(Id, Type)> :
                              { Vec::new() }
  | Id ':' Type               { vec![($1, $3)] }
  | IdTypeSeq ',' Id ':' Type { $1.push(($3, $5)); $1 }
  ;

Expr -> Expr :
    '[' ']'                             { Expr::Array }
  | 'HT' '{' '}'                        { Expr::HT }
  | '{' '}'                             { Expr::ObjectEmpty }
  | 'clos' '(' Id ',' IdAtomTypeSeq ')' { Expr::Closure($3, $5, pos($1)) }
  | 'arrayPush' '(' Atom ',' Atom ')'   { Expr::Push($3, $5, pos($1)) }
  // TODO(arjun): We can infer the type annotation.
  | 'newRef' '(' Atom ',' Type ')'      { Expr::NewRef($3, $5, pos($1)) }
  | Id '!' '(' IdSeq ')'                { Expr::ClosureCall($1, $4, pos($2)) }
  | Id '(' IdSeq ')'                    { Expr::Call($1, $3, pos($2)) }
  | AtomAdd                             { let p = $1.pos().clone(); Expr::Atom($1, p) }
  ;

TypeOpt -> Option<Type>:
             { None }
  | ':' Type { Some($2) }
  ;

StmtSeq -> Vec<Stmt> :
                 { Vec::new() }
  | StmtSeq Stmt { $1.push($2); $1 }
  ;

Block -> Stmt :
    '{' StmtSeq '}' { Stmt::Block($2, pos($1)) }
  ;

Stmt -> Stmt :
    'var' Id TypeOpt '=' Expr ';'
    { Stmt::Var(VarStmt { id: $2, named: $5, ty: $3 }, pos($1)) }
  | Id '=' Expr ';'                         { Stmt::Assign($1, $3, pos($2)) }
  | IdString ':' Block                      { label_($1, $3, pos($2)) }
  // TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
  | IdAtom '.' IdString '=' AtomAdd ';'
    { Stmt::Var(VarStmt::new(id_("_"), Expr::ObjectSet($1, str_($3, pos($2)), $5, pos($2))), pos($2)) }
  // TODO(arjun): The concrete syntax is more restrictive than the abstract syntax.
  | IdAtom '<<' IdString '=' AtomAdd ';'   
    { Stmt::Var(VarStmt::new(id_("_"), Expr::HTSet($1, str_($3, pos($2)), $5, pos($2))), pos($2)) }
  | 'if' '(' AtomAdd ')' Block 'else' Block { Stmt::If($3, Box::new($5), Box::new($7), pos($1)) }
  | 'loop' Block                            { Stmt::Loop(Box::new($2), pos($1)) }
  | 'return' AtomAdd ';'                    { Stmt::Return($2, pos($1)) }
  | 'break' IdString ';'                    { Stmt::Break(Label::Named($2), pos($1)) }
  | 'while' '(' AtomAdd ')' Block           { while_($3, $5, pos($1)) }
  | '*' Id '=' Expr ';'                     { Stmt::Store($2, $4, pos($1)) }
  | Expr ';'                                { Stmt::Expression($1, pos($2)) }
  ; 

Global -> (Id, Global) :
    'const' Id ':' Type '=' AtomAdd ';' { ($2, Global { is_mut: false, ty: $4, atom: Some($6) }) }
  | 'var' Id ':' Type '=' AtomAdd ';'   { ($2, Global { is_mut: true, ty: $4, atom: Some($6) }) }
  | 'var' Id ':' Type ';'               { ($2, Global { is_mut: true, ty: $4, atom: None }) }
  ;

GlobalVec -> HashMap<Id, Global> :
                     { HashMap::new() }
  | GlobalVec Global { $1.insert($2.0, $2.1); $1 }
  ;

FunctionVec -> HashMap<Id, Function> :
                         { HashMap::new() }
  | FunctionVec Function { $1.insert($2.0, $2.1); $1 }
  ;

Function -> (Id, Function) :
    'function' Id '(' IdTypeSeq ')' Block 
    {
      let mut args = Vec::new();
      let mut params = Vec::new();
      for (p, a) in $4.into_iter() { args.push(a); params.push(p); }
      let fn_type = FnType { args, result: None };
      ($2, Function { body: $6, fn_type, params, span: pos($1) })
    }
  | 'function' Id '(' IdTypeSeq ')' ':' Type Block 
    {
      let mut args = Vec::new();
      let mut params = Vec::new();
      for (p, a) in $4.into_iter() { args.push(a); params.push(p); }
      let fn_type = FnType { args, result: Some(Box::new($7)) };
      ($2, Function { body: $8, fn_type, params, span: pos($1) })
    }
  ;

Import -> (String, Type) :
    'import' IdString ':' Type ';' { ($2, $4) }
  ;

Imports -> HashMap<String, Type> :
                   { HashMap::new() }
  | Imports Import { $1.insert($2.0, $2.1); $1 }
  ;

Program -> Program :
    Imports GlobalVec FunctionVec 
    { Program { rts_fn_imports: $1, globals: $2, functions: $3, data: Vec::new() } }
  ;

// An idiom that turns lexing errors into parsing errors. Any mismatched
// character turns into the token "UNMATCHED". From:
// https://softdevteam.github.io/grmtools/master/book/errorrecovery.html
Unmatched -> ():
  "UNMATCHED" { } 
  ;

%%

use std::collections::HashMap;
use super::syntax::*;
use super::constructors::*;
use super::parser::pos;
use super::super::pos::Pos;
use crate::string_escaping::unescape_string;

fn parse_uint(s: &str) -> u32 {
    match s.parse::<u32>() {
        Ok(val) => val,
        Err(_) => panic!("{} cannot be represented as a u64", s)
    }
}

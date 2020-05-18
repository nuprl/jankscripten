use im_rc::HashSet;
use std::env;
use std::fs;

use libjankscripten::javascript::*;

type Class = HashSet<Key>;
type ClassTable = HashSet<Class>;

struct Gather {
    class_table: ClassTable,
    new_table: HashSet<String>,
}

impl Gather {
    fn new() -> Self {
        return Self {
            class_table: HashSet::new(),
            new_table: HashSet::new(),
        };
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Lit(_) => {}
            Expr::Array(es) => es.iter().for_each(|e| self.expr(e)),
            Expr::Object(kvs) => {
                let keys: HashSet<Key> = kvs.iter().map(|(k, _)| k.to_owned()).collect();
                self.class_table.insert(keys);
                kvs.iter().for_each(|(_, v)| self.expr(v));
            }
            Expr::This => {}
            Expr::Id(_x) => {}
            Expr::Dot(e, _) => self.expr(e),
            Expr::Bracket(e1, e2) => {
                self.expr(e1);
                self.expr(e2);
            }
            Expr::New(f, es) => {
                self.expr(f);
                if let Expr::Id(x) = f.as_ref() {
                    self.new_table.insert(x.clone());
                }
                es.iter().for_each(|e| self.expr(e));
            }
            Expr::Unary(_, e) => self.expr(e),
            Expr::Binary(_, e1, e2) => {
                self.expr(e1);
                self.expr(e2);
            }
            Expr::UnaryAssign(_, lv) => {
                // TODO
            }
            Expr::If(e1, e2, e3) => {
                self.expr(e1);
                self.expr(e2);
                self.expr(e3);
            }
            Expr::Assign(_, lv, e) => {
                // TODO LV
                self.expr(e);
            }
            Expr::Call(f, es) => {
                self.expr(f);
                es.iter().for_each(|e| self.expr(e));
            }
            Expr::Func(_, _, s) => self.stmt(s),
            Expr::Seq(es) => es.iter().for_each(|e| self.expr(e)),
        }
    }

    fn var_decl(&mut self, decl: &VarDecl) {
        self.expr(&decl.named);
    }

    fn for_init(&mut self, init: &ForInit) {
        match init {
            ForInit::Expr(e) => self.expr(e),
            ForInit::Decl(decls) => decls.iter().for_each(|d| self.var_decl(d)),
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(stmts) => stmts.iter().for_each(|s| self.stmt(s)),
            Stmt::Empty => (),
            Stmt::Expr(e) => self.expr(e),
            Stmt::If(e, s1, s2) => {
                self.expr(e);
                self.stmt(s1);
                self.stmt(s2);
            }
            Stmt::Switch(e, cases, default) => {
                self.expr(e);
                cases.iter().for_each(|(_, s)| self.stmt(s));
                self.stmt(default);
            }
            Stmt::While(e, s) => {
                self.expr(e);
                self.stmt(s);
            }
            Stmt::DoWhile(s, e) => {
                self.expr(e);
                self.stmt(s);
            }
            Stmt::For(init, e1, e2, s) => {
                self.for_init(init);
                self.expr(e1);
                self.expr(e2);
                self.stmt(s);
            }
            Stmt::ForIn(_, _, e, s) => {
                self.expr(e);
                self.stmt(s);
            }
            Stmt::Label(_, s) => self.stmt(s),
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::Catch(s1, _x, s2) => {
                self.stmt(s1);
                self.stmt(s2);
            }
            Stmt::Finally(s1, s2) => {
                self.stmt(s1);
                self.stmt(s2);
            }
            Stmt::Throw(e) => self.expr(e),
            Stmt::VarDecl(decls) => decls.iter().for_each(|d| self.var_decl(d)),
            Stmt::Func(_f, _args, body) => self.stmt(body),
            Stmt::Return(e) => self.expr(e),
        }
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let filename = &args[1];
    let js_code = fs::read_to_string(filename).unwrap();
    let result = parse(&js_code);
    if let Err(_) = result {
        println!("Cannot parse {}: {:?}", filename, result);
    }
    let program = result.unwrap();
    let mut gather = Gather::new();
    gather.stmt(&program);
    println!(
        "{}: {},{}",
        filename,
        gather.class_table.len(),
        gather.new_table.len()
    );
}

//! Some convenient abstractions for working with Z3.
//!
//!
//! The code for the macros in this file are based on:
//!
//! https://github.com/arjunguha/TypeWhich/commits/main/src/z3_state.rs
//use paste::paste;

/// A macro for building a Z3 formula, with some special bits for representing
/// types.
#[macro_export]
macro_rules! z3f {
    ($me:ident, true) => (z3::ast::Bool::from_bool($me.cxt, true));
    ($me:ident, false) => (z3::ast::Bool::from_bool($me.cxt, false));
    ($me:ident, (tid $($t:tt)*)) => ($me.t(&$($t)*));
    ($me:ident, (typ $($t:tt)*)) => ($me.t(&typ!($($t)*)));
    ($me:ident, (and $(($($t1:tt)*))*)) =>
        ($me.zand(vec![ $(z3f!($me, ($($t1)*))),* ]));
    ($me:ident, (or $(($($t1:tt)*))*)) =>
        ($me.zor(&[ $(&z3f!($me, ($($t1)*))),* ]));
    ($me:ident, (id $($t:tt)*)) => ($($t)*);
    ($me:ident, (not ($($t1:tt)*))) =>
    (!z3f!($me, ($($t1)*)));

    ($me:ident, (= ($($t1:tt)*) ($($t2:tt)*))) =>
        (z3f!($me, ($($t1)*))._eq(&z3f!($me, ($($t2)*))));
}

#[macro_export]
macro_rules! z3_datatype_accessor (
    ($bool_sort:ident, (datatype $name:ident)) => (z3::DatatypeAccessor::Datatype(stringify!($name).into()));
    ($bool_sort:ident, bool) => (z3::DatatypeAccessor::Sort($bool_sort));
);

#[macro_export]
macro_rules! z3_datatype {
    ($sort_name:ident $( ($variant:ident $( ($field_name:ident $field_sort:tt) )* ) )* ) => {

        pub struct $sort_name<'a> {
            pub cxt: &'a z3::Context,
            dts: &'a z3::DatatypeSort<'a>,
            variant_index: std::collections::HashMap<&'static str, usize>,
            field_index: std::collections::HashMap<(&'static str, &'static str), usize>,
        }

        impl<'a> $sort_name<'a> {

            #[allow(unused)]
            pub fn new(cxt: &'a z3::Context, dts: &'a z3::DatatypeSort<'a>) -> Self {
                let mut i = 0;
                let mut j = 0;
                let mut variant_index = std::collections::HashMap::<&'static str, usize>::new();
                let mut field_index = std::collections::HashMap::new();
                $(
                variant_index.insert(stringify!($variant), i);
                j = 0;
                $(
                field_index.insert((stringify!($variant), stringify!($field_name)), j);
                j = j + 1;
                )*
                i = i + 1;
                )*

                return Self {
                    cxt,
                    dts,
                    variant_index,
                    field_index
                }
            }

            #[allow(unused)]
            pub fn fresh(&self, prefix: &str) -> z3::ast::Dynamic<'a> {
                let t = z3::ast::Datatype::fresh_const(self.cxt,
                    prefix,
                    &self.dts.sort);
                return z3::ast::Dynamic::from_ast(&t);
            }

            /// `b` must be BoolSort.
            #[allow(unused)]
            pub fn make_dts(cxt: &'a z3::Context, b: &'a z3::Sort) -> z3::DatatypeBuilder<'a, 'a> {
                z3::DatatypeBuilder::new(&cxt, stringify!($sort_name))
                    $(.variant(stringify!($variant),
                        vec![$( (stringify!($field_name), z3_datatype_accessor!(b, $field_sort)) ),*]))*
            }

            // From TypeWhich, written by Luna.
            fn is_variant(&self, i: usize, model: &z3::Model, e: &z3::ast::Dynamic) -> bool {
                model
                    .eval(&self.dts.variants[i].tester.apply(&[&e]).as_bool().unwrap())
                    .unwrap()
                    .as_bool()
                    .unwrap()
            }

            fn variant_index(&self, variant_name: &'static str) -> usize {
                return *self.variant_index.get(variant_name)
                    .expect(&format!("no variant named {}", variant_name));
            }

            // To concat identifiers, e.g., [<make_ $variant>]
            paste! {

                $(

                #[allow(unused)]
                pub fn [<make_ $variant>](&self,  $($field_name: &z3::ast::Dynamic<'a>),* ) -> z3::ast::Dynamic<'a> {
                    let variant_index =
                    return self
                        .dts
                        .variants[self.variant_index(stringify!($variant))]
                        .constructor
                        .apply(&[$( $field_name ),* ]);
                }

                #[allow(unused)]
                pub fn [<is_ $variant>](&self, model: &z3::Model, e: &z3::ast::Dynamic) -> bool {
                    return self.is_variant(self.variant_index(stringify!($variant)), model, e);
                }

                /// Produces a predicate to check if an expression is constructed with $variant.
                #[allow(unused)]
                pub fn [<test_ $variant>](&self, e: &z3::ast::Dynamic<'a>) -> z3::ast::Dynamic<'a> {
                    let variant_index = self.variant_index(stringify!($variant));
                    return self.dts.variants[variant_index].tester.apply(&[e]);
                }

                $(

                #[allow(unused)]
                pub fn [<$variant _ $field_name>](&self, e: &z3::ast::Dynamic<'a>) -> z3::ast::Dynamic<'a> {
                    let variant_index = self.variant_index(stringify!($variant));
                    let field_index = *self.field_index.get(&(stringify!($variant), stringify!($field_name)))
                        .expect("macro error: field/variant combo missing");
                    return self.dts.variants[variant_index].accessors[field_index].apply(&[e]);
                }

                )*

                )*

            }


        }

    };
}

pub struct Z3EZ<'a> {
    bool_vars: Vec<z3::ast::Bool<'a>>,
    ctx: &'a z3::Context,
    solver: &'a z3::Optimize<'a>,
}

/// The types of the high-level Z3 API are all parameterized by the lifetime of the instantiated
/// Z3 solver. For example, the type of Z3 booleans is `z3::ast::Bool<'a>`. That lifetime parameter
/// prevents us from storing Z3 booleans in other types, such as the type of JankyScript
/// expressions. This structure provides a workaround
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Bool {
    index: usize,
}

impl<'a> Z3EZ<'a> {
    pub fn new(ctx: &'a z3::Context, solver: &'a z3::Optimize<'a>) -> Self {
        let bool_vars = Vec::new();
        Z3EZ {
            bool_vars,
            ctx,
            solver,
        }
    }

    pub fn fresh_bool_const(&mut self) -> Bool {
        let index = self.bool_vars.len();
        self.bool_vars
            .push(z3::ast::Bool::fresh_const(self.ctx, "b"));
        Bool { index }
    }

    pub fn eval_bool_const(&self, model: &'a z3::Model<'a>, b: &Bool) -> bool {
        let z3b = self.bool_vars.get(b.index).expect("Z3 boolean not found");
        model
            .eval(z3b)
            .expect("no result from model")
            .as_bool()
            .expect("result in model is not a bool")
    }
}

impl Bool {
    pub fn z<'a, 'b>(&self, z3ez: &'b Z3EZ<'a>) -> z3::ast::Bool<'a> {
        z3ez.bool_vars
            .get(self.index)
            .expect("Z3 boolean not found")
            .clone()
    }
}

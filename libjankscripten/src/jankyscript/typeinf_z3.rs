//! A high-level API to Z3 for TypeWhich-style type inference.
//!
//! This is based on the following file:
//!
//! https://github.com/arjunguha/TypeWhich/commits/main/src/z3_state.rs
use paste::paste;

macro_rules! z3_datatype_accessor (
    ((datatype $name:ident)) => (z3::DatatypeAccessor::Datatype(stringify!($name).into()));
);

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

            #[allow(unused)]
            pub fn make_dts(cxt: &'a z3::Context) -> z3::DatatypeBuilder<'a, 'a> {
                z3::DatatypeBuilder::new(&cxt, stringify!($sort_name))
                    $(.variant(stringify!($variant),
                        vec![$( (stringify!($field_name), z3_datatype_accessor!($field_sort)) ),*]))*
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

z3_datatype! {
    Z3Typ
    (any)
    (int)
    (float)
    (bool)
    (str)
    (array)
    (dynobject)
    (fun (args (datatype Z3TypList)) (ret (datatype Z3Typ)))
}

z3_datatype! {
    Z3TypList
    (tnil)
    (tcons (thd (datatype Z3Typ)) (ttl (datatype Z3TypList)))
}

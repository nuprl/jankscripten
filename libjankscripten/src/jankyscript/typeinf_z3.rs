//! A high-level API to Z3 for TypeWhich-style type inference.
//!
//! This is based on the following file:
//!
//! https://github.com/arjunguha/TypeWhich/commits/main/src/z3_state.rs
use super::super::shared::Type;
use paste::paste;
use z3::*;

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
            pub fn make_dts(cxt: &'a z3::Context) -> z3::DatatypeSort<'a> {
                z3::DatatypeBuilder::new(&cxt, stringify!($sort_name))
                    $(.variant(stringify!($variant),
                        vec![$( (stringify!($field_name), z3_datatype_accessor!($field_sort)) ),*]))*
                    .finish()
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
                pub fn [<$variant _ $field_name>](self, e: &z3::ast::Dynamic<'a>) -> z3::ast::Dynamic<'a> {
                    let variant_index = self.variant_index(stringify!($variant));
                    let field_index = *self.field_index.get(&(stringify!(variant_name), stringify!(field_name))).unwrap();
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
    (bool)
    (fun (args (datatype Z3Typ)))
}

/*
pub struct Z3State<'a> {
    /// Z3 solver context
    cxt: &'a z3::Context,
    /// A new sort that represents types in Z3
    typ_sort: &'a Sort<'a>,
    /// Holds accessors, etc. for typ_sort.
    typ: &'a z3::DatatypeSort<'a>,
    /// Z3 constant that represents Type::Any.
    const_any: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::Float.
    const_float: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::Int.
    const_int: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::Bool.
    const_bool: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::String.
    const_string: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::Array.
    const_array: z3::Dynamic<'a>,
    /// Z3 constant that represents Type::DynObject.
    const_dynobject: z3::Dynamic<'a>,
    /// Z3 function to construct a `typ_sort` that represents a thunk.
    ctor_func0: &'a FuncDecl<'a>,
    /// Z3 function to construct a `typ_sort` that represents a unary function.
    ctor_func1: &'a FuncDecl<'a>,
    /// Z3 function to construct a `typ_sort` that represents a binary function.
    ctor_func1: &'a FuncDecl<'a>,
}
*/
/*
 * Populates the given Z3 context with new sorts for type inference. This essentially defines
 * the following datatype:
 *
 * ```
 * (declare-datatypes ()
 *   ((Typ Any
 *         Float
 *         Int
 *         Bool
 *         String
 *         Array
 *         DynObject
 *         (AnyFun (arity N))
 *         (Fun (args (List Typ)) (result Typ)))))
 *
 */

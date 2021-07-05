pub trait Pretty {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone;
}

pub const DEFAULT_WIDTH: usize = 72;

#[macro_export]
macro_rules! impl_Display_Pretty {
    ($T:ty) => {
        impl std::fmt::Display for $T {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let pp = pretty::BoxAllocator;
                let doc = self.pretty::<_, ()>(&pp);
                doc.1.render_fmt($crate::pretty::DEFAULT_WIDTH, f)
            }
        }
    };
}

/// Nice syntax for writing pretty printers.
///
/// prettyp!(pp, e) where
/// 
/// ```text
/// <e> ::= space                // Insert a space or line break
///       | line                 // Insert a line break
///       | <string_lit>         // Prints a string literal
///       | (comma_sep <x>)      // Prints a comma separated list of <x>
///       | (line_sep <x>)       // Prints a line separated list of <x>
///       | (id <x>)             // Unquotation: prints <x>
///       | (nest <e>)           // Prints <e> as a nested expression
///       | (parens <e> )        // Parentheses
///       | (brackets <e>)       // Square brackets
///       | (braces <e>)         // Curly braces
///       | (seq <e> ...)        // Sequence
/// ```
#[macro_export]
macro_rules! prettyp {
    ($pp:ident, space) =>
        ($pp.space());
    ($pp:ident, line) =>
        ($pp.line());
    ($pp:ident, $t:literal) => ($t.pretty($pp));
    ($pp:ident, ( comma_sep $x:ident ) ) =>
        ($pp.intersperse($x.iter().map(|item| item.pretty($pp)), $pp.text(",")));
    ($pp:ident, ( line_sep $x:ident ) ) =>
        ($pp.intersperse($x.iter().map(|item| item.pretty($pp)), $pp.line()));
    ($pp:ident, ( as_string $t:ident ) ) =>
        ($pp.as_string($t));
    ($pp:ident, ( id $( $t:tt )* ) ) =>
        (($($t)*).pretty($pp));        
    ($pp:ident, ( nest $( $ts:tt )* ) ) =>
        ((prettyp!($pp, $( $ts )*)).nest(2));
    ($pp:ident, ( parens $( $ts:tt )* ) ) =>
        ((prettyp!($pp, $( $ts )*)).parens());
    ($pp:ident, ( brackets $( $ts:tt )* ) ) =>
        ((prettyp!($pp, $( $ts )*)).brackets());
    ($pp:ident, ( braces $( $ts:tt )* ) ) =>
        ((prettyp!($pp, $( $ts )*)).braces());
    ($pp:ident, ( seq $( $ts:tt )* ) ) =>
        ($pp.concat(vec![ $( prettyp!($pp, $ts) ),* ]));
}

impl Pretty for &'static str {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.text(*self)
    }
}

impl Pretty for String {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.text(self)
    }
}

impl Pretty for u32 {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.text(format!("{}", *self))
    }
}

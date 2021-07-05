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

#[macro_export]
macro_rules! pretty {
    ($pp:ident, space) => ($pp.space());
    ($pp:ident, comma_sep ( $x:ident ) ) =>
        ($pp.intersperse($x.iter().map(|item| item.pretty($pp)), $pp.text(",")));
    ($pp:ident, ( $( $ts:tt )* ) ) =>
        ((pretty!($pp, $( $ts )*)).parens());
    ($pp:ident, $t:ident) => ($t.pretty($pp));
    ($pp:ident, $t:literal) => ($t.pretty($pp));
    ($pp:ident, $( $ts:tt ),* ) => ($pp.concat(vec![ $( pretty!($pp, $ts ) ),* ]));
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

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
                doc.1.render_fmt($crate::shared::pretty::DEFAULT_WIDTH, f)
            }
        }
    };
}

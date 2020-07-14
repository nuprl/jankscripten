#[derive(Debug)]
pub enum BinOp {
    Plus,
    PlusFloatFloat,
    // TODO: others
}

#[derive(Debug)]
pub enum UnaryOp {
    IncrementAny,
    IncrementNum
    // TODO: others
}
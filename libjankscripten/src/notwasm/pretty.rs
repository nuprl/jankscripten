use super::syntax::*;

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            UnaryOp::Sqrt => write!(f, "Math.sqrt"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            BinaryOp::PtrEq => write!(f, "==="),
            BinaryOp::I32Eq => write!(f, "==="),
            BinaryOp::I32Add => write!(f, "+"),
            BinaryOp::I32Sub => write!(f, "-"),
            BinaryOp::I32Mul => write!(f, "*"),
            BinaryOp::I32GT => write!(f, ">"),
            BinaryOp::I32LT => write!(f, "<"),
            BinaryOp::I32Ge => write!(f, ">="),
            BinaryOp::I32Le => write!(f, "<="),
            BinaryOp::I32And => write!(f, "&"),
            BinaryOp::I32Or => write!(f, "|"),
            BinaryOp::I32Div => write!(f, "/"),
            BinaryOp::I32Rem => write!(f, "%"),
            BinaryOp::F64Add => write!(f, "+"),
            BinaryOp::F64Sub => write!(f, "-"),
            BinaryOp::F64Mul => write!(f, "*"),
            BinaryOp::F64Div => write!(f, "/"),

        }
    }
}
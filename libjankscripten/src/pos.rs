//! Source locations for the jankscripten toolchain.
use std::rc::Rc;
use std::fmt;
use combine::stream::state::SourcePosition;
use swc_common::{SourceMap, Span};

/// A position in a source file. The type is opaque, because SWC uses a fancy representation of
/// positions that is more sophisticated than what we need. Moreover, there is no need for the
/// rest of the toolchain to actually examine positions.
#[derive(PartialEq, Clone)]
pub struct Pos {
    pos: P
}

#[derive(Clone)]
enum P {
    /// Line, column (indexed by one), and filename. The filename appears at every AST node. Thus we
    /// reference count to avoid copying the `String`, and to avoid writing `Pos<'a>` everywhere.
    Combine(usize, usize, Rc<String>),
    /// You are probably wondering why we don't store line and column information. See the
    /// implementation of `fmt::Debug for P` for the answer.
    SWC(Rc<SourceMap>, Span),
    Unknown,
}

/// We need to implement this manually, since SourceMap does not implement PartialEq.
impl PartialEq for P {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Ignores filenames, which should be fine since jankscripten only works with a single
            // JavaScript input file at a time.
            (P::SWC(_, span1), P::SWC(_, span2)) => span1 == span2,
            (P::Combine(line1, col1, file1), P::Combine(line2, col2, file2)) =>
                line1 == line2 && col1 == col2 && file1 == file2,
            (P::Unknown, P::Unknown) => true,
            _ => false
       }
    }
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // No point seeing the internal structure of a `Pos`.
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pos.fmt(f)
    }
}

impl std::fmt::Display for P {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            P::Unknown => write!(f, "unknown position"),
            P::Combine(line, col, file) => write!(f, "{}: line {}, column {}", file, line, col),
            P::SWC(source_map, span) => {
                // The implementation of this function starts from the beginning of the file and
                // counts newline characters and multi-byte Unicode characters. If we were to call
                // this function at every AST node, we would probably slow down the system
                // significantly. Therefore, we only call it here, and thus need to store the
                // `SourceMap` in the `Pos`.
                let loc = source_map.lookup_char_pos(span.lo);
                // Column is zero based. col_display accounts for multi-byte Unicode. 🤣
                write!(f, "{}: line {}, column {}", loc.file.name, loc.line, loc.col_display + 1)
            }
        }
    }
}

impl Pos {
    pub fn from_combine(filename: &Rc<String>, sp: SourcePosition) -> Pos {
        // Classic "garbage-in garbage-out" if `sp.line` or `sp.column` are negative.
        Pos { pos: P::Combine(sp.line as usize, sp.column as usize, filename.clone()) }
    }

    pub fn from_swc(source_map: &Rc<SourceMap>, span: Span) -> Pos {
        Pos { pos: P::SWC(Rc::clone(source_map), span) }
    }

    pub const UNKNOWN: Pos = Pos { pos: P::Unknown };
}

impl Default for Pos {
    fn default() -> Pos {
        Pos { pos: P::Unknown }
    }
}

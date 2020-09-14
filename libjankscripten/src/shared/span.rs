use swc_common;

pub type Span = swc_common::Span;

pub type SourceMap = swc_common::SourceMap;

pub fn span_to_string(span: &Span, source_map: &SourceMap) -> String {
    source_map.span_to_string(*span)
}

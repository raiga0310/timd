#[derive(Debug, PartialEq)]
pub struct Node {
    node_type: NodeType,
    children: Vec<Box<Node>>,
}

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Span(Span),
    Text(Text),
}

#[derive(Debug, PartialEq)]
pub struct Span {
    span_type: SpanType,
    content: Text,
}

#[derive(Debug, PartialEq)]
pub enum SpanType {
    Em,
    Strong,
    Link,
    Code,
}

impl Span {
    pub fn new(span_type: SpanType, content: Text, children: Vec<Box<Node>>) -> Box<Node> {
        Box::new(Node {
            node_type: NodeType::Span(Span {
                span_type,
                content,
            }),
            children,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Text {
    pub data: String,
}

impl Text {
    pub fn new(text: String) -> Box<Node> {
        Box::new(Node {
            node_type: NodeType::Text(Text { data: text }),
            children: vec![],
        })
    }
}
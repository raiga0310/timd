
#[cfg(test)]
mod tests {
    use crate::node::{Link, Span, Text};

    use super::*;

    #[test]
    fn parse_link() {
        assert_eq!(
            link().parse("[display](link)"),
            Ok(Span::new(crate::node::SpanType::Link(Link::new(
                "display".to_string(),
                "link".to_string()
            )), vec![]))
        )
    }
}
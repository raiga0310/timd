use combine::{between, many1, ParseError, Parser, Stream};
use combine::parser::char::{char, letter};

use crate::node::{Link, Node, Span, SpanType};

pub fn link<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        between(
            char('['), 
            char(']'), 
            many1(letter())
        ),
        between(
            char('('), 
            char(')'), 
            many1(letter())
        ),
    )
        .map(|(display, link)| Span::new(SpanType::Link(Link::new(display, link)), vec![]))
}

#[cfg(test)]
mod tests {
    use crate::node::{Link, Span};

    use super::*;

    #[test]
    fn parse_link() {
        assert_eq!(
            link().parse("[display](link)"),
            Ok((
                Span::new(SpanType::Link(Link::new(
                    "display".to_string(),
                    "link".to_string()
                )), 
                vec![]), 
                ""
            ))
        )
    }
}
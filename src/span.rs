use combine::parser::char::{char, letter};
use combine::{between, choice, many1, satisfy, ParseError, Parser, Stream};

use crate::node::{Link, Node, Span, SpanType, Text};

pub fn strong<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        between(
            (char('*'), char('*')),
            (char('*'), char('*')),
            parse_with_delimiter('*'), // temp
        ),
        between(
            (char('_'), char('_')),
            (char('_'), char('_')),
            parse_with_delimiter('_'), // temp
        ),
    ))
    .map(|content| Span::new(SpanType::Strong, vec![content]))
}

pub fn em<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        between(
            char('*'),
            char('*'),
            parse_with_delimiter('*'), // temp
        ),
        between(
            char('_'),
            char('_'),
            parse_with_delimiter('_'), // temp
        ),
    ))
    .map(|content| Span::new(SpanType::Em, vec![content]))
}

pub fn parse_with_delimiter<Input>(delimiter: char) -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(satisfy(move |c| c != delimiter)).map(|s| Text::new(s))
}

pub fn link<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        between(char('['), char(']'), many1(letter())),
        between(char('('), char(')'), many1(letter())),
    )
        .map(|(display, link)| Span::new(SpanType::Link(Link::new(display, link)), vec![]))
}

#[cfg(test)]
mod tests {
    use crate::node::{Link, Span};

    use super::*;

    #[test]
    fn parse_strong() {
        {
            assert_eq!(
                strong().parse("**foo**"),
                Ok((
                    Span::new(SpanType::Strong, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                strong().parse("__bar__"),
                Ok((
                    Span::new(SpanType::Strong, vec![Text::new("bar".to_string())]),
                    ""
                ))
            )
        }
    }

    #[test]
    fn parse_em() {
        {
            assert_eq!(
                em().parse("*foo*"),
                Ok((
                    Span::new(SpanType::Em, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                em().parse("_bar_"),
                Ok((
                    Span::new(SpanType::Em, vec![Text::new("bar".to_string())]),
                    ""
                ))
            )
        }
    }
    #[test]
    fn parse_link() {
        assert_eq!(
            link().parse("[display](link)"),
            Ok((
                Span::new(
                    SpanType::Link(Link::new("display".to_string(), "link".to_string())),
                    vec![]
                ),
                ""
            ))
        )
    }
}

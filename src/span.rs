use combine::parser::char::{char, letter};
use combine::{any, between, choice, many1, satisfy, ParseError, Parser, Stream};

use crate::node::{Link, Node, Span, SpanType, Text};

pub fn code<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(char('`'), char('`'), parse_with_delimiter('`'))
        .map(|content| Span::new(SpanType::Code, vec![content]))
}

pub fn del<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        (char('~'), char('~')),
        (char('~'), char('~')),
        parse_with_delimiter('~'),
    )
    .map(|content| Span::new(SpanType::Del, vec![content]))
}
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

pub fn text<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(any()).map(|t| Text::new(t))
}

#[cfg(test)]
mod tests {
    use crate::node::{Link, Span};

    use super::*;

    #[test]
    fn parse_code() {
        assert_eq!(
            code().parse("`foo`"),
            Ok((
                Span::new(SpanType::Code, vec![Text::new("foo".to_string())]),
                ""
            ))
        )
    }
    #[test]
    fn parse_del() {
        assert_eq!(
            del().parse("~~foo~~"),
            Ok((
                Span::new(SpanType::Del, vec![Text::new("foo".to_string())]),
                ""
            ))
        )
    }

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

    #[test]
    fn parse_text() {
        assert_eq!(
            text().parse("hoge"),
            Ok((Text::new("hoge".to_string()), ""))
        )
    }
}

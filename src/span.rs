use combine::parser::char::{char, letter};
use combine::{any, attempt, between, choice, many1, ParseError, Parser, Stream};

use crate::node::{Link, Node, Span, SpanType, Text};

pub fn code<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(char('`'), char('`'), many1(letter()))
        .map(|content| Span::new(SpanType::Code, vec![Text::new(content)]))
}

pub fn del<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        (char('~'), char('~')),
        (char('~'), char('~')),
        many1(letter()),
    )
    .map(|content| parse_with_children(SpanType::Del, content))
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
            many1(letter()), // temp
        ),
        between(
            (char('_'), char('_')),
            (char('_'), char('_')),
            many1(letter()), // temp
        ),
    ))
    .map(|content| parse_with_children(SpanType::Strong, content))
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
            many1(letter()), // temp
        ),
        between(
            char('_'),
            char('_'),
            many1(letter()), // temp
        ),
    ))
    .map(|content| parse_with_children(SpanType::Em, content))
}

pub fn parse_with_children(span_type: SpanType, content: String) -> Box<Node> {
    let children: Vec<Box<Node>> = match span_elements().parse(content.as_str()) {
        Ok((mut children, remain)) => {
            if !remain.is_empty() {
                children.push(Text::new(remain.to_string()));
            }
            children
        }

        Err(_) => vec![Text::new(content.clone())],
    };
    Span::new(span_type, children)
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
    many1(any()).map(Text::new)
}

pub fn span_element<Input>() -> impl Parser<Input, Output = Box<Node>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((attempt(strong()), attempt(em()), attempt(link()), attempt(del()), attempt(code()), attempt(text())))
}

pub fn span_elements<Input>() -> impl Parser<Input, Output = Vec<Box<Node>>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(span_element())
}

#[cfg(test)]
mod tests {
    use crate::node::{Link, Span};

    use super::*;

    #[test]
    fn parse_with_span_element() {
        {
            assert_eq!(
                span_element().parse("`foo`"),
                Ok((
                    Span::new(SpanType::Code, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("~~foo~~"),
                Ok((
                    Span::new(SpanType::Del, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("**foo**"),
                Ok((
                    Span::new(SpanType::Strong, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("__bar__"),
                Ok((
                    Span::new(SpanType::Strong, vec![Text::new("bar".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("*foo*"),
                Ok((
                    Span::new(SpanType::Em, vec![Text::new("foo".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("_bar_"),
                Ok((
                    Span::new(SpanType::Em, vec![Text::new("bar".to_string())]),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("[display](link)"),
                Ok((
                    Span::new(
                        SpanType::Link(Link::new("display".to_string(), "link".to_string())),
                        vec![]
                    ),
                    ""
                ))
            )
        }
        {
            assert_eq!(
                span_element().parse("hoge"),
                Ok((Text::new("hoge".to_string()), ""))
            )
        }
    }

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

use combine::parser::char::{char, letter};
use combine::{attempt, choice, many1, ParseError, Parser, Stream};

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Em,
    Strong,
    Del,
    Code,
    Text(String),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    symbol: Symbol,
}

impl Token {
    pub fn new(symbol: Symbol) -> Box<Token> {
        Box::new(Token { symbol })
    }
}

pub fn em<Input>() -> impl Parser<Input, Output = Box<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((char('*'), char('_'))).map(|_| Token::new(Symbol::Em))
}

pub fn strong<Input>() -> impl Parser<Input, Output = Box<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice(((char('*'), char('*')), (char('_'), char('_')))).map(|_| Token::new(Symbol::Strong))
}

pub fn del<Input>() -> impl Parser<Input, Output = Box<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (char('~'), char('~')).map(|_| Token::new(Symbol::Del))
}

pub fn code<Input>() -> impl Parser<Input, Output = Box<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    char('`').map(|_| Token::new(Symbol::Code))
}

pub fn text<Input>() -> impl Parser<Input, Output = Box<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(letter()).map(|s| Token::new(Symbol::Text(s)))
}

pub fn lexer<Input>() -> impl Parser<Input, Output = Vec<Box<Token>>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(choice((
        attempt(strong()),
        attempt(em()),
        attempt(del()),
        attempt(code()),
        text(),
    )))
}
#[cfg(test)]
mod tests {
    use crate::lex::*;

    #[test]
    fn multiple_lexer_symbol() {
        {
            assert_eq!(
                lexer().parse("`foo`~~bar~~*buz*__hoge___huga_**piyo**"),
                Ok((
                    vec![
                        Token::new(Symbol::Code),
                        Token::new(Symbol::Text("foo".to_string())),
                        Token::new(Symbol::Code),
                        Token::new(Symbol::Del),
                        Token::new(Symbol::Text("bar".to_string())),
                        Token::new(Symbol::Del),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Text("buz".to_string())),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Strong),
                        Token::new(Symbol::Text("hoge".to_string())),
                        Token::new(Symbol::Strong),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Text("huga".to_string())),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Strong),
                        Token::new(Symbol::Text("piyo".to_string())),
                        Token::new(Symbol::Strong),
                    ],
                    ""
                ))
            )
        }
        {
            assert_eq!(
                lexer().parse("**foo_bar~~buz~~_**"),
                Ok((
                    vec![
                        Token::new(Symbol::Strong),
                        Token::new(Symbol::Text("foo".to_string())),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Text("bar".to_string())),
                        Token::new(Symbol::Del),
                        Token::new(Symbol::Text("buz".to_string())),
                        Token::new(Symbol::Del),
                        Token::new(Symbol::Em),
                        Token::new(Symbol::Strong),                  
                    ],
                    ""
                ))
            )
        }
    }

    #[test]
    fn lexer_symbol() {
        {
            assert_eq!(lexer().parse("*"), Ok((vec![Token::new(Symbol::Em)], "")))
        }
        {
            assert_eq!(lexer().parse("_"), Ok((vec![Token::new(Symbol::Em)], "")))
        }
        {
            assert_eq!(
                lexer().parse("**"),
                Ok((vec![Token::new(Symbol::Strong)], ""))
            )
        }
        {
            assert_eq!(
                lexer().parse("__"),
                Ok((vec![Token::new(Symbol::Strong)], ""))
            )
        }
        {
            assert_eq!(lexer().parse("~~"), Ok((vec![Token::new(Symbol::Del)], "")))
        }
        {
            assert_eq!(lexer().parse("`"), Ok((vec![Token::new(Symbol::Code)], "")))
        }
        {
            assert_eq!(
                lexer().parse("hoge"),
                Ok((vec![Token::new(Symbol::Text("hoge".to_string()))], ""))
            )
        }
    }

    #[test]
    fn parse_symbol() {
        {
            assert_eq!(em().parse("*"), Ok((Token::new(Symbol::Em), "")))
        }
        {
            assert_eq!(em().parse("_"), Ok((Token::new(Symbol::Em), "")))
        }
        {
            assert_eq!(strong().parse("**"), Ok((Token::new(Symbol::Strong), "")))
        }
        {
            assert_eq!(strong().parse("__"), Ok((Token::new(Symbol::Strong), "")))
        }
        {
            assert_eq!(del().parse("~~"), Ok((Token::new(Symbol::Del), "")))
        }
        {
            assert_eq!(code().parse("`"), Ok((Token::new(Symbol::Code), "")))
        }
        {
            assert_eq!(
                text().parse("hoge"),
                Ok((Token::new(Symbol::Text("hoge".to_string())), ""))
            )
        }
    }
}

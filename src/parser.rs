use crate::{Spanned, lexer};
use colored::Colorize;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Spanned<lexer::Token>],
    prev_token: &'a Spanned<lexer::Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Spanned<lexer::Token>>) -> Self {
        Self {
            tokens: tokens,
            prev_token: &tokens[0],
        }
    }
    fn finished(&mut self) -> bool {
        self.tokens.is_empty()
    }

    pub fn parse(mut self) -> (Vec<Spanned<Expression>>, Vec<Spanned<Error>>) {
        let mut errs = vec![];
        let mut es = vec![];

        while !self.finished() {
            match self.parse_expression() {
                Ok(t) => es.push(t),
                Err(e) => {
                    errs.push(e);
                    self.eat();
                }
            }
        }

        (es, errs)
    }

    fn parse_expression(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_term()
    }
    fn parse_term(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        let mut left = self.parse_factor()?;

        while self.current().is_some_and(|t| {
            matches!(
                t.v,
                lexer::Token::Operator(lexer::Operator::Plus | lexer::Operator::Minus)
            )
        }) {
            let op = match self.current() {
                None => {
                    return Err(Spanned {
                        offset: self.prev_token.offset,
                        len: self.prev_token.len,
                        line_beginning: self.prev_token.line_beginning,
                        v: Error::ExpectedBinaryOperator,
                    });
                }
                Some(Spanned {
                    v: lexer::Token::Operator(op),
                    ..
                }) => *op,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_primary()?;

            left = Spanned {
                offset: left.offset,
                len: right.offset - left.offset,
                line_beginning: left.line_beginning,
                v: Expression::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }
    fn parse_factor(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        let mut left = self.parse_primary()?;

        while self.current().is_some_and(|t| {
            matches!(
                t.v,
                lexer::Token::Operator(lexer::Operator::Slash | lexer::Operator::Star)
            )
        }) {
            let op = match self.current() {
                None => {
                    return Err(Spanned {
                        offset: self.prev_token.offset,
                        len: self.prev_token.len,
                        line_beginning: self.prev_token.line_beginning,
                        v: Error::ExpectedBinaryOperator,
                    });
                }
                Some(Spanned {
                    v: lexer::Token::Operator(op),
                    ..
                }) => *op,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_primary()?;

            left = Spanned {
                offset: left.offset,
                len: right.offset - left.offset,
                line_beginning: left.line_beginning,
                v: Expression::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }
    fn parse_primary(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        match self.current() {
            None => Err(Spanned {
                v: Error::ExpectedPrimaryExpresion,
                offset: self.prev_token.offset,
                len: self.prev_token.len,
                line_beginning: self.prev_token.line_beginning,
            }),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: lexer::Token::Integer(n),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Integer(*n),
                });
                self.eat();
                r
            }
            Some(Spanned {
                v: lexer::Token::OpenParen,
                ..
            }) => {
                self.eat();

                let expr = self.parse_expression()?;

                match self.current() {
                    None => {
                        return Err(Spanned {
                            offset: self.prev_token.offset,
                            len: self.prev_token.len,
                            line_beginning: self.prev_token.line_beginning,
                            v: Error::UnclosedParenthesis,
                        });
                    }
                    Some(Spanned {
                        v: lexer::Token::CloseParen,
                        ..
                    }) => {
                        self.eat();
                    }
                    Some(Spanned {
                        offset,
                        len,
                        line_beginning,
                        v: _,
                    }) => {
                        return Err(Spanned {
                            offset: *offset,
                            len: *len,
                            line_beginning: *line_beginning,
                            v: Error::UnclosedParenthesis,
                        });
                    }
                };

                Ok(expr)
            }
            Some(t) => Err(Spanned {
                offset: t.offset,
                len: t.len,
                line_beginning: t.line_beginning,
                v: Error::UnexpectedTokenInExpression,
            }),
        }
    }

    fn current(&self) -> Option<&Spanned<lexer::Token>> {
        self.tokens.get(0)
    }
    fn eat(&mut self) -> Option<Spanned<lexer::Token>> {
        match self.tokens.split_at_checked(1) {
            None => None,
            Some((l, r)) => {
                self.tokens = r;
                self.prev_token = &l[0];
                return Some(l[0].clone());
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    ExpectedPrimaryExpresion,
    UnexpectedTokenInExpression,
    ExpectedBinaryOperator,
    UnclosedParenthesis,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedPrimaryExpresion => {
                write!(
                    f,
                    "[{}]\n  Expected a primary expression here\n  [{}]: A primary expression is only a number literal for now",
                    "Error".red(),
                    "Note".green()
                )
            }
            Self::UnexpectedTokenInExpression => {
                write!(
                    f,
                    "[{}]\n  Unexpected token found when parsing an expression here",
                    "Error".red(),
                )
            }
            Self::ExpectedBinaryOperator => {
                write!(
                    f,
                    "[{}]\n  Expected a binary operator here\n  [{}]\n  A binary operator in Goop can only be one of these: `+`, `-`, `*', `/`",
                    "Error".red(),
                    "Note".green(),
                )
            }
            Self::UnclosedParenthesis => {
                write!(
                    f,
                    "[{}]\n  Found unbalanced parenthesis when parsing an enclosed expression",
                    "Error".red(),
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(u64),
    Binary {
        left: Box<Spanned<Expression>>,
        op: lexer::Operator,
        right: Box<Spanned<Expression>>,
    },
}

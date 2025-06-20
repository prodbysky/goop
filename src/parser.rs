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
            tokens,
            prev_token: &tokens[0],
        }
    }
    fn finished(&mut self) -> bool {
        self.tokens.is_empty()
    }

    pub fn parse(mut self) -> (Vec<Spanned<Statement>>, Vec<Spanned<Error>>) {
        let mut errs = vec![];
        let mut sts = vec![];

        while !self.finished() {
            match self.parse_statement() {
                Ok(s) => sts.push(s),
                Err(e) => errs.push(e),
            }
        }

        (sts, errs)
    }

    fn error_from_last_tk(&self, e: Error) -> Spanned<Error> {
        Spanned {
            offset: self.prev_token.offset,
            len: self.prev_token.len,
            line_beginning: self.prev_token.line_beginning,
            v: e,
        }
    }

    fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        match self.current().cloned() {
            Some(Spanned {
                offset,
                len: _,
                line_beginning,
                v: lexer::Token::Keyword(k),
            }) => match k {
                lexer::Keyword::Return => {
                    self.eat();
                    let expr = self.parse_expression()?;
                    let semicolon = self.expect(&lexer::Token::Semicolon)?;
                    Ok(Spanned {
                        offset,
                        len: semicolon.offset - offset,
                        line_beginning,
                        v: Statement::Return(expr),
                    })
                }
                lexer::Keyword::Let => {
                    let l = self.eat().unwrap();
                    let ident = match self.current().cloned() {
                        Some(Spanned {
                            v: lexer::Token::Identifier(ident),
                            ..
                        }) => ident,
                        Some(Spanned { .. }) | None => {
                            return Err(self.error_from_last_tk(Error::ExpectedIdentifier));
                        }
                    };
                    self.expect(&lexer::Token::Identifier("".to_string()))?;
                    self.expect(&lexer::Token::Colon)?;
                    let type_name = match self.current().cloned() {
                        Some(Spanned {
                            v: lexer::Token::Identifier(ident),
                            ..
                        }) => ident,
                        Some(Spanned { .. }) | None => {
                            return Err(self.error_from_last_tk(Error::ExpectedIdentifier));
                        }
                    };
                    self.eat();
                    self.expect(&lexer::Token::Assign)?;
                    let expr = self.parse_expression()?;
                    let e = self.eat().unwrap();
                    Ok(Spanned {
                        offset: l.offset,
                        len: e.offset - l.offset,
                        line_beginning: l.line_beginning,
                        v: Statement::VarAssign {
                            name: ident.to_string(),
                            t: type_name.to_string(),
                            expr,
                        },
                    })
                }
                lexer::Keyword::If => {
                    let begin = self.eat().unwrap();
                    let expr = self.parse_expression()?;
                    self.expect(&lexer::Token::OpenCurly)?;
                    let mut body = vec![];
                    while self
                        .current()
                        .is_some_and(|t| t.v != lexer::Token::CloseCurly)
                    {
                        body.push(self.parse_statement()?);
                    }
                    let end = self.expect(&lexer::Token::CloseCurly)?;
                    Ok(Spanned {
                        offset: begin.offset,
                        len: end.offset - begin.offset,
                        line_beginning: begin.line_beginning,
                        v: Statement::If { cond: expr, body },
                    })
                }
                lexer::Keyword::While => {
                    let begin = self.eat().unwrap();
                    let expr = self.parse_expression()?;
                    self.expect(&lexer::Token::OpenCurly)?;
                    let mut body = vec![];
                    while self
                        .current()
                        .is_some_and(|t| t.v != lexer::Token::CloseCurly)
                    {
                        body.push(self.parse_statement()?);
                    }
                    let end = self.expect(&lexer::Token::CloseCurly)?;
                    Ok(Spanned {
                        offset: begin.offset,
                        len: end.offset - begin.offset,
                        line_beginning: begin.line_beginning,
                        v: Statement::While { cond: expr, body },
                    })
                }
                lexer::Keyword::True | lexer::Keyword::False => unreachable!(),
            },
            Some(Spanned {
                offset,
                len: _,
                line_beginning,
                v: lexer::Token::Identifier(name),
            }) => {
                self.eat().unwrap(); // var reassign
                self.expect(&lexer::Token::Assign)?;
                let value = self.parse_expression()?;
                let end = self.expect(&lexer::Token::Semicolon)?;
                Ok(Spanned {
                    offset,
                    len: end.offset - offset,
                    line_beginning,
                    v: Statement::VarReassign { name, expr: value },
                })
            }

            None => unreachable!(),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: _,
            }) => {
                let e = Err(Spanned {
                    offset,
                    len,
                    line_beginning,
                    v: Error::UnexpectedToken,
                });
                self.eat();
                e
            }
        }
    }

    fn expect(&mut self, t: &lexer::Token) -> Result<Spanned<lexer::Token>, Spanned<Error>> {
        match self.eat() {
            Some(x) => {
                if std::mem::discriminant(&x.v) == std::mem::discriminant(t) {
                    Ok(x)
                } else {
                    Err(self.error_from_last_tk(Error::UnexpectedToken))
                }
            }
            None => Err(self.error_from_last_tk(Error::UnexpectedToken)),
        }
    }

    // https://craftinginterpreters.com/parsing-expressions.html
    fn parse_expression(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_comparison()
    }

    // hell yeah
    fn parse_comparison(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_expr_help(
            |t| {
                matches!(
                    t.v,
                    lexer::Token::Operator(lexer::Operator::Less | lexer::Operator::More)
                )
            },
            Parser::parse_term,
        )
    }

    fn parse_term(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_expr_help(
            |t| {
                matches!(
                    t.v,
                    lexer::Token::Operator(lexer::Operator::Plus | lexer::Operator::Minus)
                )
            },
            Parser::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_expr_help(
            |t| {
                matches!(
                    t.v,
                    lexer::Token::Operator(
                        lexer::Operator::Slash | lexer::Operator::Star | lexer::Operator::Percent
                    )
                )
            },
            Parser::parse_unary,
        )
    }

    fn parse_unary(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        if matches!(
            self.current(),
            Some(Spanned {
                v: lexer::Token::Operator(lexer::Operator::Not | lexer::Operator::Minus),
                ..
            })
        ) {
            let (offset, _len, line_beginning, op) = match self.eat().unwrap() {
                Spanned {
                    offset,
                    len,
                    line_beginning,
                    v: lexer::Token::Operator(a @ (lexer::Operator::Not | lexer::Operator::Minus)),
                } => (offset, len, line_beginning, a),
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            return Ok(Spanned {
                offset,
                len: right.offset - offset,
                line_beginning,
                v: Expression::Unary {
                    op,
                    right: Box::new(right),
                },
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        match self.current() {
            None => Err(self.error_from_last_tk(Error::ExpectedPrimaryExpresion)),
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
                offset,
                len,
                line_beginning,
                v: lexer::Token::Char(n),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Char(*n),
                });
                self.eat();
                r
            }
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: lexer::Token::Keyword(lexer::Keyword::True),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Bool(true),
                });
                self.eat();
                r
            }
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: lexer::Token::Keyword(lexer::Keyword::False),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Bool(false),
                });
                self.eat();
                r
            }
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: lexer::Token::Identifier(ident),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Identifier(ident.to_string()),
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
                        return Err(self.error_from_last_tk(Error::UnclosedParenthesis));
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

    fn parse_expr_help(
        &mut self,
        cond: fn(&Spanned<lexer::Token>) -> bool,
        lower_element: fn(&mut Parser<'a>) -> Result<Spanned<Expression>, Spanned<Error>>,
    ) -> Result<Spanned<Expression>, Spanned<Error>> {
        let mut left = lower_element(self)?;

        while self.current().is_some_and(cond) {
            let op = match self.current() {
                None => return Err(self.error_from_last_tk(Error::ExpectedBinaryOperator)),
                Some(Spanned {
                    v: lexer::Token::Operator(op),
                    ..
                }) => *op,
                _ => unreachable!(),
            };
            self.eat();
            let right = lower_element(self)?;

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

    fn current(&self) -> Option<&Spanned<lexer::Token>> {
        self.tokens.first()
    }
    fn eat(&mut self) -> Option<Spanned<lexer::Token>> {
        match self.tokens.split_at_checked(1) {
            None => None,
            Some((l, r)) => {
                self.tokens = r;
                self.prev_token = &l[0];
                Some(l[0].clone())
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
    UnexpectedToken,
    ExpectedIdentifier,
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
            Self::UnexpectedToken => {
                write!(
                    f,
                    "[{}]\n  Found an unexpected token when parsing statement",
                    "Error".red()
                )
            }
            Self::ExpectedIdentifier => {
                write!(
                    f,
                    "[{}]\n  Expected an identifier here (variable name?)",
                    "Error".red()
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(u64),
    Char(char),
    Bool(bool),
    Identifier(String),
    Binary {
        left: Box<Spanned<Expression>>,
        op: lexer::Operator,
        right: Box<Spanned<Expression>>,
    },
    Unary {
        op: lexer::Operator,
        right: Box<Spanned<Expression>>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Spanned<Expression>),
    VarAssign {
        name: String,
        t: String,
        expr: Spanned<Expression>,
    },
    VarReassign {
        name: String,
        expr: Spanned<Expression>,
    },
    If {
        cond: Spanned<Expression>,
        body: Vec<Spanned<Statement>>,
    },
    While {
        cond: Spanned<Expression>,
        body: Vec<Spanned<Statement>>,
    },
}

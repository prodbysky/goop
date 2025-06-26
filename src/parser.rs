use crate::{Spanned, lexer, logging};
use colored::Colorize;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Spanned<lexer::Token>],
    prev_token: &'a Spanned<lexer::Token>,
}

#[derive(Debug, Default)]
pub struct AstModule {
    funcs: Vec<Spanned<Function>>,
}

impl AstModule {
    pub fn funcs(&self) -> &[Spanned<Function>] {
        &self.funcs
    }
}

#[derive(Debug, Default)]
pub struct Function {
    pub name: String,
    ret_type: String,
    body: Vec<Spanned<Statement>>,
}

impl Function {
    pub fn get_type(&self) -> crate::ir::FunctionType {
        crate::ir::FunctionType {
            name: self.name.clone(),
            ret: crate::ir::type_from_type_name(&self.ret_type),
            args: vec![],
        }
    }
    pub fn body(&self) -> &[Spanned<Statement>] {
        &self.body
    }
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

    pub fn parse(mut self) -> (AstModule, Vec<Spanned<Error>>) {
        let mut errs = vec![];
        let mut module = AstModule::default();

        while !self.finished() {
            match self.parse_function() {
                Ok(s) => module.funcs.push(s),
                Err(e) => errs.push(e),
            }
        }

        (module, errs)
    }

    fn error_from_last_tk(&self, e: Error) -> Spanned<Error> {
        Spanned {
            offset: self.prev_token.offset,
            len: self.prev_token.len,
            line_beginning: self.prev_token.line_beginning,
            v: e,
        }
    }

    fn parse_function(&mut self) -> Result<Spanned<Function>, Spanned<Error>> {
        let begin = self.expect(
            &lexer::Token::Keyword(lexer::Keyword::Func),
            Error::UnexpectedToken,
        )?;
        let ident = self.expect_ident(Error::ExpectedFunctionName)?;
        self.expect(
            &lexer::Token::OpenParen,
            Error::ExpectedFunctionArgListBegin,
        )?;
        self.expect(&lexer::Token::CloseParen, Error::ExpectedFunctionArgListEnd)?;
        let ret_type = self.expect_ident(Error::ExpectedFunctionReturnType)?;
        self.expect(&lexer::Token::OpenCurly, Error::ExpectedBlockBegin)?;

        let mut body = vec![];
        while self
            .current()
            .is_some_and(|t| t.v != lexer::Token::CloseCurly)
        {
            body.push(self.parse_statement()?);
        }

        let last = self.expect(&lexer::Token::CloseCurly, Error::ExpectedBlockEnd)?;
        Ok(Spanned {
            offset: begin.offset,
            len: last.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Function {
                name: ident.v,
                body,
                ret_type: ret_type.v,
            },
        })
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
                    let semicolon =
                        self.expect_semicolon(Error::ExpectedSemicolonAfterStatement)?;
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
                    self.expect_ident(Error::ExpectedBindingName)?;
                    self.expect(
                        &lexer::Token::Colon,
                        Error::ExpectedColonAfterLetBindingName,
                    )?;
                    let type_name = self.expect_ident(Error::ExpectedTypeName)?;
                    self.expect(&lexer::Token::Assign, Error::UnexpectedToken)?;
                    let expr = self.parse_expression()?;
                    let semicolon =
                        self.expect_semicolon(Error::ExpectedSemicolonAfterStatement)?;
                    Ok(Spanned {
                        offset: l.offset,
                        len: semicolon.offset - l.offset,
                        line_beginning: l.line_beginning,
                        v: Statement::VarAssign {
                            name: ident.to_string(),
                            t: type_name.v,
                            expr,
                        },
                    })
                }
                lexer::Keyword::If => {
                    let begin = self.eat().unwrap();
                    let expr = self.parse_expression()?;
                    self.expect(&lexer::Token::OpenCurly, Error::ExpectedBlockBegin)?;
                    let mut body = vec![];
                    while self
                        .current()
                        .is_some_and(|t| t.v != lexer::Token::CloseCurly)
                    {
                        body.push(self.parse_statement()?);
                    }
                    let end = self.expect(&lexer::Token::CloseCurly, Error::ExpectedBlockEnd)?;
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
                    self.expect(&lexer::Token::OpenCurly, Error::ExpectedBlockBegin)?;
                    let mut body = vec![];
                    while self
                        .current()
                        .is_some_and(|t| t.v != lexer::Token::CloseCurly)
                    {
                        body.push(self.parse_statement()?);
                    }
                    let end = self.expect(&lexer::Token::CloseCurly, Error::ExpectedBlockEnd)?;
                    Ok(Spanned {
                        offset: begin.offset,
                        len: end.offset - begin.offset,
                        line_beginning: begin.line_beginning,
                        v: Statement::While { cond: expr, body },
                    })
                }
                lexer::Keyword::Func => {
                    Err(self.error_from_last_tk(Error::FunctionDefinitionWithinFunction))
                }
                lexer::Keyword::True | lexer::Keyword::False => unreachable!(),
            },
            Some(Spanned {
                offset,
                len: _,
                line_beginning,
                v: lexer::Token::Identifier(name),
            }) => {
                self.eat().unwrap();
                match self.current().unwrap().v {
                    lexer::Token::Assign => {
                        self.eat().unwrap();
                        let value = self.parse_expression()?;
                        let semicolon =
                            self.expect_semicolon(Error::ExpectedSemicolonAfterStatement)?;
                        Ok(Spanned {
                            offset,
                            len: semicolon.offset - offset,
                            line_beginning,
                            v: Statement::VarReassign { name, expr: value },
                        })
                    }
                    lexer::Token::OpenParen => {
                        let begin = self.eat().unwrap();
                        let mut args = vec![];
                        while self
                            .current()
                            .is_some_and(|t| t.v != lexer::Token::CloseParen)
                        {
                            let expr = self.parse_expression()?;
                            args.push(expr);
                            match self.current() {
                                None => return Err(self.error_from_last_tk(Error::UnexpectedToken)),
                                Some(Spanned {
                                    v: lexer::Token::Comma,
                                    ..
                                }) => self.eat(),
                                Some(Spanned {
                                    v: lexer::Token::CloseParen,
                                    ..
                                }) => {
                                    break;
                                }
                                Some(Spanned { .. }) => {
                                    return Err(self.error_from_last_tk(Error::UnexpectedToken));
                                }
                            };
                        }
                        self.expect(&lexer::Token::CloseParen, Error::UnclosedParenthesis)?;
                        let semicolon =
                            self.expect_semicolon(Error::ExpectedSemicolonAfterStatement)?;
                        Ok(Spanned {
                            offset: begin.offset,
                            len: semicolon.offset - begin.offset,
                            line_beginning: begin.line_beginning,
                            v: Statement::FuncCall { name, args },
                        })
                    }
                    _ => Err(self.error_from_last_tk(Error::UnexpectedToken)),
                }
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

    fn expect(
        &mut self,
        t: &lexer::Token,
        e: Error,
    ) -> Result<Spanned<lexer::Token>, Spanned<Error>> {
        match self.eat() {
            Some(x) => {
                if std::mem::discriminant(&x.v) == std::mem::discriminant(t) {
                    Ok(x)
                } else {
                    Err(self.error_from_last_tk(e))
                }
            }
            None => Err(self.error_from_last_tk(e)),
        }
    }

    fn expect_semicolon(&mut self, e: Error) -> Result<Spanned<lexer::Token>, Spanned<Error>> {
        self.expect(&lexer::Token::Semicolon, e)
    }

    fn expect_ident(&mut self, e: Error) -> Result<Spanned<String>, Spanned<Error>> {
        match self.expect(&lexer::Token::Identifier("".to_string()), e) {
            Ok(t) => match t.v {
                lexer::Token::Identifier(name) => Ok(Spanned {
                    offset: t.offset,
                    len: t.len,
                    line_beginning: t.line_beginning,
                    v: name,
                }),
                _ => unreachable!(),
            },
            Err(e) => Err(e),
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
        match self.current().cloned() {
            None => Err(self.error_from_last_tk(Error::ExpectedPrimaryExpresion)),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: lexer::Token::Integer(n),
            }) => {
                let r = Ok(Spanned {
                    offset,
                    len,
                    line_beginning,
                    v: Expression::Integer(n),
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
                    offset,
                    len,
                    line_beginning,
                    v: Expression::Char(n),
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
                    offset,
                    len,
                    line_beginning,
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
                    offset,
                    len,
                    line_beginning,
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
                self.eat().unwrap();
                match self.current().cloned() {
                    Some(Spanned {
                        offset,
                        len: _,
                        line_beginning,
                        v: lexer::Token::OpenParen,
                    }) => {
                        self.eat().unwrap();
                        let mut args = vec![];
                        while self
                            .current()
                            .is_some_and(|t| t.v != lexer::Token::CloseParen)
                        {
                            let expr = self.parse_expression()?;
                            args.push(expr);
                            match self.current() {
                                None => return Err(self.error_from_last_tk(Error::UnexpectedToken)),
                                Some(Spanned {
                                    v: lexer::Token::Comma,
                                    ..
                                }) => self.eat(),
                                Some(Spanned {
                                    v: lexer::Token::CloseParen,
                                    ..
                                }) => {
                                    break;
                                }
                                Some(Spanned { .. }) => {
                                    return Err(self.error_from_last_tk(Error::UnexpectedToken));
                                }
                            };
                        }
                        let end =
                            self.expect(&lexer::Token::CloseParen, Error::UnclosedParenthesis)?;
                        Ok(Spanned {
                            offset,
                            len: end.offset - offset,
                            line_beginning,
                            v: Expression::FuncCall {
                                name: ident.to_string(),
                                args,
                            },
                        })
                    }
                    None | Some(_) => Ok(Spanned {
                        offset,
                        len,
                        line_beginning,
                        v: Expression::Identifier(ident.to_string()),
                    }),
                }
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
    ExpectedSemicolonAfterStatement,
    ExpectedBinaryOperator,
    UnclosedParenthesis,
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedBindingName,
    ExpectedTypeName,
    ExpectedColonAfterLetBindingName,
    ExpectedBlockBegin,
    ExpectedBlockEnd,
    ExpectedFunctionName,
    ExpectedFunctionArgListBegin,
    ExpectedFunctionArgListEnd,
    ExpectedFunctionReturnType,
    FunctionDefinitionWithinFunction,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedPrimaryExpresion => {
                logging::error!(f, "Expected a primary expression here")
            }
            Self::UnexpectedTokenInExpression => {
                logging::error!(f, "Unexpected token found when parsing an expression here")
            }
            Self::ExpectedBinaryOperator => {
                logging::error!(f, "Expected a binary operator here")
            }
            Self::UnclosedParenthesis => {
                logging::error!(f, "Found unbalanced parenthesis when parsing an enclosed expression")
            }
            Self::UnexpectedToken => {
                logging::error!(f, "Found an unexpected token when parsing statement")
            }
            Self::ExpectedIdentifier => {
                logging::error!(f, "Expected an identifier here (variable name?)")
            }
            Self::ExpectedSemicolonAfterStatement => {
                logging::error!(f, "Expected a semicolon here to terminate a statement")
            }
            Self::ExpectedBlockBegin => {
                logging::error!(f, "Expected a `{{` here to start a block, for a `if` or `while` statement")
            }
            Self::ExpectedBlockEnd => {
                logging::error!(f, "Expected a `}}` here to end a block, of a `if` or `while` statement")
            }
            Self::ExpectedBindingName => {
                logging::error!(f, "Expected here to be a name for a variable")
            }
            Self::ExpectedTypeName => {
                logging::error!(f, "Expected here to be a name of a type")
            }
            Self::ExpectedColonAfterLetBindingName => {
                logging::error!(f, "Expected a colon separator after the name of a variable")
            }
            Self::ExpectedFunctionName => {
                logging::error!(f, "Expected a function name to be here")
            }
            Self::ExpectedFunctionArgListBegin => {
                logging::error!(f, "Expected a `(` to begin a function argument list")
            }
            Self::ExpectedFunctionArgListEnd => {
                logging::error!(f, "Expected a `)` to end a function argument list")
            }
            Self::ExpectedFunctionReturnType => {
                logging::error!(f, "Expected a type name to end the function `header`")
            }
            Self::FunctionDefinitionWithinFunction => {
                logging::error!(f, "Found an attempt to define a function within a function, that is not allowed")
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
    FuncCall {
        name: String,
        args: Vec<Spanned<Expression>>,
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
    FuncCall {
        name: String,
        args: Vec<Spanned<Expression>>,
    },
}

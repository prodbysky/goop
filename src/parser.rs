use crate::lexer::{Keyword, Operator, Token};
use crate::{ir, logging};
use crate::Spanned;
use colored::Colorize;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self {
            tokens,
            prev_token: &tokens[0],
        }
    }

    pub fn parse(mut self) -> Result<Module, Spanned<Error>> {
        let mut module = Module::new();

        while !self.finished() {
            module.functions.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> Result<Spanned<Function>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned { v: Token::Keyword(Keyword::Func), .. }) => {
                self.parse_function()
            }
            Some(Spanned { v: Token::Keyword(Keyword::Extern), .. }) => {
                self.parse_extern()
            }
            _ => todo!()
        }
    }

    fn parse_function(&mut self) -> Result<Spanned<Function>, Spanned<Error>> {
        let begin = self.expect_keyword(Keyword::Func)?;
        let identifier = self.expect_name()?;
        self.expect_token(Token::OpenParen)?;

        let mut args = vec![];

        while self.peek().is_some_and(|t| t.v != Token::CloseParen) {
            let name = self.expect_name()?;
            let type_name = self.expect_name()?;
            args.push((name.v, type_name.v));
            match self.peek() {
                None => return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
                Some(Spanned {
                    v: Token::Comma,
                    ..
                }) => self.next(),
                Some(Spanned {
                    v: Token::CloseParen,
                    ..
                }) => {
                    break;
                }
                Some(Spanned { .. }) => {
                    return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken));
                }
            };
        }
        self.expect_token(Token::CloseParen)?;
        let return_type = self.expect_name()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Function {
                name: identifier.v,
                body: Some(body),
                ret_type: return_type.v,
                args
            },
        })
    }

    fn parse_block(&mut self) -> Result<(Vec<Spanned<Statement>>, Spanned<()>), Spanned<Error>> {
        self.expect_token(Token::OpenCurly)?;
        let mut body = vec![];

        while self.peek().is_some_and(|t| t.v != Token::CloseCurly) {
            body.push(self.parse_statement()?);
        }
        let end = self.expect_token(Token::CloseCurly)?;
        let end_span = Spanned {
            v: (),
            offset: end.offset,
            line_beginning: end.line_beginning,
            len: end.len,
        };
        Ok((body, end_span))
    }

    fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned {
                v: Token::Keyword(Keyword::Return),
                ..
            }) => self.parse_return(),
            Some(Spanned {
                v: Token::Keyword(Keyword::If),
                ..
            }) => self.parse_if(),
            Some(Spanned {
                v: Token::Keyword(Keyword::While),
                ..
            }) => self.parse_while(),
            Some(Spanned {
                v: Token::Keyword(Keyword::Let),
                ..
            }) => self.parse_let(),
            Some(Spanned {
                v: Token::Keyword(Keyword::Func),
                ..
            }) => Err(self.spanned_error_from_last_tk(Error::NestedFunctionDefinition)),
            Some(Spanned {
                v: Token::Keyword(Keyword::True | Keyword::False),
                ..
            }) => Err(self.spanned_error_from_last_tk(Error::BooleanExpressionAsStatement)),
            Some(Spanned {
                v: Token::Identifier(name),
                offset,
                len,
                line_beginning,
            }) => {
                self.next().unwrap();
                match self.peek() {
                    Some(Spanned {
                        v: Token::Assign, ..
                    }) => {
                        self.next();
                        self.parse_assign(&Spanned {
                            offset,
                            len,
                            line_beginning,
                            v: name,
                        })
                    }
                    Some(Spanned {
                        v: Token::OpenParen,
                        ..
                    }) => {
                        self.next();
                        self.parse_call(&Spanned {
                            offset,
                            len,
                            line_beginning,
                            v: name,
                        })
                    }
                    None => Err(self.spanned_error_from_last_tk(
                        Error::UnexpectedTokenAfterIdentifierInStatement { got: None },
                    )),
                    Some(Spanned { v, .. }) => Err(self.spanned_error_from_last_tk(
                        Error::UnexpectedTokenAfterIdentifierInStatement { got: Some(v) },
                    )),
                }
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedStartOfStatement)),
            Some(_) => Err(self.spanned_error_from_last_tk(Error::ExpectedStartOfStatement)),
        }
    }

    /// return Optional[Expression];
    /// NOTE: For now we assume that the return value is not void
    fn parse_return(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        let begin = self.next().unwrap();
        let expr = self.parse_expression()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Statement::Return (expr),
        })
    }

    /// if [expr] { ... }
    /// NOTE: The `expr` has to be a boolean
    fn parse_if(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        let begin = self.next().unwrap();
        let cond = self.parse_expression()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Statement::If { cond, body },
        })
    }

    /// while [expr] { ... }
    /// NOTE: The `expr` has to be a boolean
    fn parse_while(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        let begin = self.next().unwrap();
        let cond = self.parse_expression()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Statement::While { cond, body },
        })
    }

    /// let [name]: Optional[type_name] = [expr];
    fn parse_let(&mut self) -> Result<Spanned<Statement>, Spanned<Error>> {
        let begin = self.next().unwrap();
        let name = self.expect_name()?.v;
        self.expect_token(Token::Colon)?;
        let type_name = if self.peek().is_some_and(|t| t.v == Token::Assign) {
            None
        } else {
            Some(self.expect_name()?.v)
        };
        self.expect_token(Token::Assign)?;
        let value = self.parse_expression()?;
        let end = self.expect_semicolon()?;

        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Statement::VarAssign {
                name,
                t: type_name,
                expr: value,
            },
        })
    }

    /// [name] = [expr];
    /// NOTE: `expr` has to be the same type as the previous value of the variable
    fn parse_assign(
        &mut self,
        begin_token: &Spanned<String>,
    ) -> Result<Spanned<Statement>, Spanned<Error>> {
        let name = begin_token.v.clone();
        let value = self.parse_expression()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned {
            offset: begin_token.offset,
            len: end.offset - begin_token.offset,
            line_beginning: begin_token.line_beginning,
            v: Statement::VarReassign { name, expr: value },
        })
    }

    /// [func_name]([name]*);
    fn parse_call(
        &mut self,
        begin_token: &Spanned<String>,
    ) -> Result<Spanned<Statement>, Spanned<Error>> {
        let name = begin_token.v.clone();
        let mut args = vec![];
        while self
            .peek()
            .is_some_and(|t| t.v != Token::CloseParen)
        {
            let expr = self.parse_expression()?;
            args.push(expr);
            match self.peek() {
                None => return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
                Some(Spanned {
                    v: Token::Comma,
                    ..
                }) => self.next(),
                Some(Spanned {
                    v: Token::CloseParen,
                    ..
                }) => {
                    break;
                }
                Some(Spanned { .. }) => {
                    return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken));
                }
            };
        }
        self.expect_token(Token::CloseParen)?;
        let semicolon = self.expect_semicolon()?;
        Ok(Spanned {
            offset: begin_token.offset,
            len: semicolon.offset - begin_token.offset,
            line_beginning: begin_token.line_beginning,
            v: Statement::FuncCall { name, args },
        })
    }

    /// extern func [name](([name] [type])*) [ret];
    fn parse_extern(&mut self) -> Result<Spanned<Function>, Spanned<Error>> {
        let begin = self.expect_keyword(Keyword::Extern)?;
        self.expect_keyword(Keyword::Func)?;
        let identifier = self.expect_name()?;
        self.expect_token(Token::OpenParen)?;

        let mut args = vec![];

        while self.peek().is_some_and(|t| t.v != Token::CloseParen) {
            let name = self.expect_name()?;
            let type_name = self.expect_name()?;
            args.push((name.v, type_name.v));
            match self.peek() {
                None => return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
                Some(Spanned {
                    v: Token::Comma,
                    ..
                }) => self.next(),
                Some(Spanned {
                    v: Token::CloseParen,
                    ..
                }) => {
                    break;
                }
                Some(Spanned { .. }) => {
                    return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken));
                }
            };
        }
        self.expect_token(Token::CloseParen)?;
        let return_type = self.expect_name()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned {
            offset: begin.offset,
            len: end.offset - begin.offset,
            line_beginning: begin.line_beginning,
            v: Function {
                name: identifier.v,
                body: None,
                ret_type: return_type.v,
                args
            },
        })
    }

    fn parse_expression(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_cmp()
    }

    fn parse_cmp(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_base_binary_expression(
            |t| matches!(t.v, Token::Operator(Operator::Less | Operator::More)),
            Parser::parse_term,
        )
    }

    fn parse_term(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_base_binary_expression(
            |t| matches!(t.v, Token::Operator(Operator::Plus | Operator::Minus)),
            Parser::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        self.parse_base_binary_expression(
            |t| {
                matches!(
                    t.v,
                    Token::Operator(Operator::Slash | Operator::Star | Operator::Percent)
                )
            },
            Parser::parse_unary,
        )
    }

    fn parse_unary(&mut self) -> Result<Spanned<Expression>, Spanned<Error>> {
        if let Some(Spanned {
            v: Token::Operator(op @ (Operator::Not | Operator::Minus)),
            offset,
            line_beginning,
            len: _,
        }) = self.peek()
        {
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
        match self.next() {
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Integer(n),
            }) => Ok(Spanned {
                offset,
                len,
                line_beginning,
                v: Expression::Integer(n),
            }),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Char(n),
            }) => Ok(Spanned {
                offset,
                len,
                line_beginning,
                v: Expression::Char(n),
            }),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Keyword(Keyword::True),
            }) => Ok(Spanned {
                offset,
                len,
                line_beginning,
                v: Expression::Bool(true),
            }),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Keyword(Keyword::False),
            }) => Ok(Spanned {
                offset,
                len,
                line_beginning,
                v: Expression::Bool(false),
            }),
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Identifier(ident),
            }) => match self.peek() {
                Some(Spanned {
                    offset,
                    len: _,
                    line_beginning,
                    v: Token::OpenParen,
                }) => {
                    self.next();
                    let mut args = vec![];
                    while self.peek().is_some_and(|t| t.v != Token::CloseParen) {
                        let expr = self.parse_expression()?;
                        args.push(expr);
                        match self.peek() {
                            None => {
                                return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken));
                            }
                            Some(Spanned {
                                v: Token::Comma, ..
                            }) => self.next(),
                            Some(Spanned {
                                v: Token::CloseParen,
                                ..
                            }) => {
                                break;
                            }
                            Some(Spanned { .. }) => {
                                return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken));
                            }
                        };
                    }
                    let end = self.expect_token(Token::CloseParen)?;
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
            },
            Some(Spanned {
                v: Token::OpenParen,
                ..
            }) => {
                self.next();
                let expr = self.parse_expression()?;

                match self.peek() {
                    None => {
                        return Err(self.spanned_error_from_last_tk(Error::UnbalancedParenthesis));
                    }
                    Some(Spanned {
                        v: Token::CloseParen,
                        ..
                    }) => {
                        self.next();
                    }
                    Some(Spanned {
                        offset,
                        len,
                        line_beginning,
                        v: _,
                    }) => {
                        return Err(Spanned {
                            offset,
                            len,
                            line_beginning,
                            v: Error::UnbalancedParenthesis,
                        });
                    }
                };

                Ok(expr)
            }
            Some(Spanned { offset, line_beginning, v: Token::Keyword(Keyword::Cast), .. }) => {
                self.expect_token(Token::OpenParen)?;
                let t_name = self.expect_name()?;
                self.expect_token(Token::Comma)?;
                let value = self.parse_expression()?;
                self.expect_token(Token::CloseParen)?;
                let end = self.expect_token(Token::Semicolon)?;
                Ok(Spanned { offset: offset, len: end.offset - offset, line_beginning, v: Expression::Cast { value: Box::new(value), to: t_name.v } }) 
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedExpression)),
            Some(_) => Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
        }
    }

    // https://craftinginterpreters.com/parsing-expressions.html
    fn parse_base_binary_expression(
        &mut self,
        predicate: fn(Spanned<Token>) -> bool,
        lower_precedence_parser: fn(&mut Parser<'a>) -> Result<Spanned<Expression>, Spanned<Error>>,
    ) -> Result<Spanned<Expression>, Spanned<Error>> {
        let mut l = lower_precedence_parser(self)?;

        while self.peek().is_some_and(predicate) {
            let op = match self.peek() {
                None => return Err(self.spanned_error_from_last_tk(Error::ExpectedBinaryOperator)),
                Some(Spanned {
                    v: Token::Operator(op),
                    ..
                }) => op,
                _ => unreachable!(),
            };
            self.next();
            let right = lower_precedence_parser(self)?;

            l = Spanned {
                offset: l.offset,
                len: right.offset - l.offset,
                line_beginning: l.line_beginning,
                v: Expression::Binary {
                    left: Box::new(l),
                    op,
                    right: Box::new(right),
                },
            };
        }

        Ok(l)
    }

    fn expect_token(&mut self, t: Token) -> Result<Spanned<Token>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v,
            }) if v == t => {
                self.next();
                Ok(Spanned {
                    offset,
                    len,
                    line_beginning,
                    v,
                })
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedToken {
                expected: t,
                got: None,
            })),
            Some(Spanned { v, .. }) => Err(self.spanned_error_from_last_tk(Error::ExpectedToken {
                expected: t,
                got: Some(v.clone()),
            })),
        }
    }

    fn expect_keyword(&mut self, k: Keyword) -> Result<Spanned<Token>, Spanned<Error>> {
        self.expect_token(Token::Keyword(k))
    }

    fn expect_name(&mut self) -> Result<Spanned<String>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned {
                offset,
                len,
                line_beginning,
                v: Token::Identifier(i),
            }) => {
                self.next();
                Ok(Spanned {
                    offset,
                    len,
                    line_beginning,
                    v: i.to_string(),
                })
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedToken {
                expected: Token::Identifier("name".to_string()),
                got: None,
            })),
            Some(Spanned { v, .. }) => Err(self.spanned_error_from_last_tk(Error::ExpectedToken {
                expected: Token::Identifier("name".to_string()),
                got: Some(v.clone()),
            })),
        }
    }

    fn expect_semicolon(&mut self) -> Result<Spanned<Token>, Spanned<Error>> {
        self.expect_token(Token::Semicolon)
    }

    fn finished(&self) -> bool {
        self.tokens.is_empty()
    }

    fn spanned_error_from_last_tk(&self, e: Error) -> Spanned<Error> {
        Spanned {
            offset: self.prev_token.offset,
            len: self.prev_token.len,
            line_beginning: self.prev_token.line_beginning,
            v: e,
        }
    }

    fn peek(&self) -> Option<Spanned<Token>> {
        self.tokens.first().cloned()
    }

    fn next(&mut self) -> Option<Spanned<Token>> {
        let tk = self.tokens.first().cloned();
        self.tokens = &self.tokens[1..];
        tk
    }
}


#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Spanned<Token>],
    prev_token: &'a Spanned<Token>,
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Spanned<Function>>,
}

impl Module {
    pub fn new() -> Self {
        Self { functions: vec![] }
    }
    pub fn funcs(&self) -> &[Spanned<Function>] {
        &self.functions
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    /// If body is None then its an external function
    body: Option<Vec<Spanned<Statement>>>,
    args: Vec<(String, String)>,
    ret_type: String,
}

impl Function {
    pub fn get_type(&self) -> FunctionType {
        let name = &self.name;
        let args = self.args.iter().map(|(name, type_name)| (name.clone(), ir::type_from_type_name(type_name))).collect();
        let ret = ir::type_from_type_name(&self.ret_type);
        FunctionType { name: name.to_string(), args, ret }
    }
    pub fn body(&self) -> Option<&[Spanned<Statement>]> {
        self.body.as_deref()
    }
}


/// Type used in the IR generation
#[derive(Debug, Clone)]
pub struct FunctionType {
    pub name: String,
    pub args: Vec<(String, ir::Type)>,
    pub ret: ir::Type
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "func {}(", self.name)?;
        for arg in &self.args {
            write!(f, "{arg:?},")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Spanned<Expression>),
    If {
        cond: Spanned<Expression>,
        body: Vec<Spanned<Statement>>,
    },
    While {
        cond: Spanned<Expression>,
        body: Vec<Spanned<Statement>>,
    },
    VarAssign {
        name: String,
        t: Option<String>,
        expr: Spanned<Expression>,
    },
    VarReassign {
        name: String,
        expr: Spanned<Expression>,
    },
    FuncCall {
        name: String,
        args: Vec<Spanned<Expression>>
    },
}

#[derive(Debug)]
pub enum Expression {
    Integer(u64),
    Char(char),
    Bool(bool),
    Identifier(String),
    Unary {
        op: Operator,
        right: Box<Spanned<Expression>>,
    },
    Binary {
        left: Box<Spanned<Expression>>,
        op: Operator,
        right: Box<Spanned<Expression>>,
    },
    FuncCall {
        name: String,
        args: Vec<Spanned<Expression>>,
    },
    Cast {
        value: Box<Spanned<Expression>>,
        to: String
    }
}

pub enum Error {
    ExpectedToken { expected: Token, got: Option<Token> },
    UnexpectedToken,
    ExpectedStartOfStatement,
    NestedFunctionDefinition,
    BooleanExpressionAsStatement,
    UnexpectedTokenAfterIdentifierInStatement { got: Option<Token> },
    ExpectedBinaryOperator,
    UnbalancedParenthesis,
    ExpectedExpression,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedToken { expected, got } => {
                let got_name = match got {
                    None => "nothing".to_string(),
                    Some(v) => format!("{v:?}"),
                };
                logging::error!(f, "Expected {expected:?}, instead got {got_name}")
            }
            Self::ExpectedStartOfStatement => {
                logging::error!(f, "Expected start of statement")
            }
            Self::NestedFunctionDefinition => {
                logging::error!(f, "Nested function definitions are not allowed YET")
            }
            Self::BooleanExpressionAsStatement => {
                logging::error!(
                    f,
                    "Found an attempt to use a boolean (`true` or `false`) as a statement"
                )
            }
            Self::UnexpectedTokenAfterIdentifierInStatement { got } => {
                let got_name = match got {
                    None => "nothing".to_string(),
                    Some(v) => format!("{v:?}"),
                };
                logging::errorln!(
                    f,
                    "Unexpected token found after an identifier in a statement context: {got_name}"
                )?;
                logging::helpln!(
                    f,
                    "After that name you can put a parenthesis pair to make a function call or make a assigment operation"
                )?;
                logging::noteln!(f, "Function call: name(...)")?;
                logging::note!(f, "Assignment: name = ...")
            }
            Self::ExpectedBinaryOperator => {
                logging::error!(f, "Expected here to be a binary operator")
            }
            Self::UnexpectedToken => {
                logging::error!(f, "Found an unexpected token")
            }
            Self::UnbalancedParenthesis => {
                logging::error!(f, "Found an unbalanced parenthesis pairs")
            }
            Self::ExpectedExpression => {
                logging::error!(f, "Expected here to be an expression here")
            }
        }
    }
}

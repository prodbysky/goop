use crate::lexer::{Keyword, Operator, Token};
use crate::{Span, Spanned};
use crate::{ir, logging};
use colored::Colorize;

type ParserResult<T> = Result<T, Spanned<Error>>;
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self {
            tokens,
            prev_token: &tokens[0],
        }
    }

    pub fn parse(mut self) -> ParserResult<Module> {
        let mut module = Module::new();

        while !self.finished() {
            module.functions.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> ParserResult<Spanned<Function>> {
        match self.peek() {
            Some(Spanned {
                v: Token::Keyword(Keyword::Func),
                ..
            }) => self.parse_function(),
            Some(Spanned {
                v: Token::Keyword(Keyword::Extern),
                ..
            }) => self.parse_extern(),
            _ => todo!(),
        }
    }

    fn parse_function(&mut self) -> ParserResult<Spanned<Function>> {
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
        self.expect_token(Token::CloseParen)?;
        let return_type = self.expect_name()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned::new(
            Function {
                name: identifier.v,
                body: Some(body),
                ret_type: return_type.v,
                args,
            },
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_block(&mut self) -> ParserResult<(Vec<Spanned<Statement>>, Spanned<()>)> {
        self.expect_token(Token::OpenCurly)?;
        let mut body = vec![];

        while self.peek().is_some_and(|t| t.v != Token::CloseCurly) {
            body.push(self.parse_statement()?);
        }
        let end = self.expect_token(Token::CloseCurly)?;
        let end_span = Spanned::new((), Span::new(end.begin(), end.end()));
        Ok((body, end_span))
    }

    fn parse_statement(&mut self) -> ParserResult<Spanned<Statement>> {
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
                s,
                v: Token::Identifier(name),
            }) => {
                self.next().unwrap();
                match self.peek() {
                    Some(Spanned {
                        v: Token::Assign, ..
                    }) => {
                        self.next();
                        self.parse_assign(&Spanned::new(name, s))
                    }
                    Some(Spanned {
                        v: Token::OpenParen,
                        ..
                    }) => {
                        self.next();
                        self.parse_call(&Spanned::new(name, s))
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

    fn parse_return(&mut self) -> ParserResult<Spanned<Statement>> {
        let begin = self.next().unwrap();
        if self.peek().is_some_and(|t| t.v == Token::Semicolon) {
            let end = self.next().unwrap();
            return Ok(Spanned::new(
                Statement::Return(None),
                Span::new(begin.begin(), end.end()),
            ));
        }
        let expr = self.parse_expression()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned::new(
            Statement::Return(Some(expr)),
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_if(&mut self) -> ParserResult<Spanned<Statement>> {
        let begin = self.next().unwrap();
        let cond = self.parse_expression()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned::new(
            Statement::If { cond, body },
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_while(&mut self) -> ParserResult<Spanned<Statement>> {
        let begin = self.next().unwrap();
        let cond = self.parse_expression()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned::new(
            Statement::While { cond, body },
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_let(&mut self) -> ParserResult<Spanned<Statement>> {
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

        Ok(Spanned::new(
            Statement::VarAssign {
                name,
                t: type_name,
                expr: value,
            },
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_assign(&mut self, begin_token: &Spanned<String>) -> ParserResult<Spanned<Statement>> {
        let name = begin_token.v.clone();
        let value = self.parse_expression()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned::new(
            Statement::VarReassign { name, expr: value },
            Span::new(begin_token.begin(), end.end()),
        ))
    }

    fn parse_call(&mut self, begin_token: &Spanned<String>) -> ParserResult<Spanned<Statement>> {
        let name = begin_token.v.clone();
        let mut args = vec![];
        while self.peek().is_some_and(|t| t.v != Token::CloseParen) {
            let expr = self.parse_expression()?;
            args.push(expr);
            match self.peek() {
                None => return Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
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
        self.expect_token(Token::CloseParen)?;
        let semicolon = self.expect_semicolon()?;
        Ok(Spanned::new(
            Statement::FuncCall { name, args },
            Span::new(begin_token.begin(), semicolon.end()),
        ))
    }

    fn parse_extern(&mut self) -> ParserResult<Spanned<Function>> {
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
        self.expect_token(Token::CloseParen)?;
        let return_type = self.expect_name()?;
        let end = self.expect_semicolon()?;
        Ok(Spanned::new(
            Function {
                name: identifier.v,
                body: None,
                args,
                ret_type: return_type.v,
            },
            Span::new(begin.begin(), end.end()),
        ))
    }

    fn parse_expression(&mut self) -> ParserResult<Spanned<Expression>> {
        self.parse_cmp()
    }

    fn parse_cmp(&mut self) -> ParserResult<Spanned<Expression>> {
        self.parse_base_binary_expression(
            |t| matches!(t.v, Token::Operator(Operator::Less | Operator::More)),
            Parser::parse_term,
        )
    }

    fn parse_term(&mut self) -> ParserResult<Spanned<Expression>> {
        self.parse_base_binary_expression(
            |t| matches!(t.v, Token::Operator(Operator::Plus | Operator::Minus)),
            Parser::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> ParserResult<Spanned<Expression>> {
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

    fn parse_unary(&mut self) -> ParserResult<Spanned<Expression>> {
        if let Some(Spanned {
            v: Token::Operator(op @ (Operator::Not | Operator::Minus)),
            s,
        }) = self.peek()
        {
            let right = self.parse_unary()?;
            let span = Span::new(right.begin(), s.end);
            return Ok(Spanned::new(
                Expression::Unary {
                    op,
                    right: Box::new(right),
                },
                span,
            ));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParserResult<Spanned<Expression>> {
        match self.next() {
            Some(Spanned {
                s,
                v: Token::Integer(n),
            }) => Ok(Spanned::new(Expression::Integer(n), s)),
            Some(Spanned {
                s,
                v: Token::Char(n),
            }) => Ok(Spanned::new(Expression::Char(n), s)),
            Some(Spanned {
                s,
                v: Token::Keyword(Keyword::True),
            }) => Ok(Spanned::new(Expression::Bool(true), s)),
            Some(Spanned {
                s,
                v: Token::Keyword(Keyword::False),
            }) => Ok(Spanned::new(Expression::Bool(false), s)),
            Some(Spanned {
                s,
                v: Token::Identifier(ident),
            }) => match self.peek() {
                Some(Spanned {
                    v: Token::OpenParen,
                    s,
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
                    Ok(Spanned::new(
                        Expression::FuncCall {
                            name: ident.to_string(),
                            args,
                        },
                        Span::new(s.begin, end.end()),
                    ))
                }
                None | Some(_) => Ok(Spanned::new(Expression::Identifier(ident.to_string()), s)),
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
                    Some(Spanned { s, v: _ }) => {
                        return Err(Spanned::new(Error::UnbalancedParenthesis, s));
                    }
                };

                Ok(expr)
            }
            Some(Spanned {
                s,
                v: Token::Keyword(Keyword::Cast),
                ..
            }) => {
                self.expect_token(Token::OpenParen)?;
                let t_name = self.expect_name()?;
                self.expect_token(Token::Comma)?;
                let value = self.parse_expression()?;
                let end = self.expect_token(Token::CloseParen)?;
                Ok(Spanned::new(
                    Expression::Cast {
                        value: Box::new(value),
                        to: t_name.v,
                    },
                    Span::new(s.begin, end.end()),
                ))
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedExpression)),
            Some(_) => Err(self.spanned_error_from_last_tk(Error::UnexpectedToken)),
        }
    }

    // https://craftinginterpreters.com/parsing-expressions.html
    fn parse_base_binary_expression(
        &mut self,
        predicate: fn(Spanned<Token>) -> bool,
        lower_precedence_parser: fn(&mut Parser<'a>) -> ParserResult<Spanned<Expression>>,
    ) -> ParserResult<Spanned<Expression>> {
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
            let span = Span::new(l.begin(), right.end());

            l = Spanned::new(
                Expression::Binary {
                    left: Box::new(l),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(l)
    }

    fn expect_token(&mut self, t: Token) -> ParserResult<Spanned<Token>> {
        match self.peek() {
            Some(Spanned { s, v }) if v == t => {
                self.next();
                Ok(Spanned::new(v, s))
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

    fn expect_keyword(&mut self, k: Keyword) -> ParserResult<Spanned<Token>> {
        self.expect_token(Token::Keyword(k))
    }

    fn expect_name(&mut self) -> ParserResult<Spanned<String>> {
        match self.peek() {
            Some(Spanned {
                s,
                v: Token::Identifier(i),
            }) => {
                self.next();
                Ok(Spanned::new(i.to_string(), s))
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedToken {
                expected: Token::Identifier("name".to_string()),
                got: None,
            })),
            Some(Spanned { s, v }) => Err(Spanned::new(
                Error::ExpectedToken {
                    expected: Token::Identifier("any_name".to_string()),
                    got: Some(v),
                },
                s,
            )),
        }
    }

    fn expect_semicolon(&mut self) -> ParserResult<Spanned<Token>> {
        self.expect_token(Token::Semicolon)
    }

    fn finished(&self) -> bool {
        self.tokens.is_empty()
    }

    fn spanned_error_from_last_tk(&self, e: Error) -> Spanned<Error> {
        Spanned::new(e, Span::new(self.prev_token.s.begin, self.prev_token.s.end))
    }

    fn peek(&self) -> Option<Spanned<Token>> {
        self.tokens.first().cloned()
    }

    fn next(&mut self) -> Option<Spanned<Token>> {
        match self.tokens.split_first() {
            None => None,
            Some((tk, rest)) => {
                self.prev_token = tk;
                self.tokens = rest;
                Some(tk.clone())
            }
        }
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
        let args = self
            .args
            .iter()
            .map(|(name, type_name)| (name.clone(), ir::type_from_type_name(type_name)))
            .collect();
        let ret = ir::type_from_type_name(&self.ret_type);
        FunctionType {
            name: name.to_string(),
            args,
            ret,
        }
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
    pub ret: ir::Type,
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
    Return(Option<Spanned<Expression>>),
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
        args: Vec<Spanned<Expression>>,
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
        to: String,
    },
}

#[derive(Debug)]
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

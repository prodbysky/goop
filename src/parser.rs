use crate::Spanned;
use crate::lexer::{Keyword, Token};
use crate::logging;
use colored::Colorize;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self {
            tokens,
            prev_token: &tokens[0]
        }
    }

    pub fn parse(mut self) -> Result<Module, Spanned<Error>> {
        let mut module = Module::new();

        while !self.finished() {
            module.functions.push(self.parse_function()?);
        }

        Ok(module)
    }
 
    fn parse_function(&mut self) -> Result<Spanned<Function>, Spanned<Error>> {
        let begin = self.expect_keyword(Keyword::Func)?;
        let identifier = self.expect_name()?;
        self.expect_token(Token::OpenParen)?;
        self.expect_token(Token::CloseParen)?;
        let return_type = self.expect_name()?;
        let (body, end) = self.parse_block()?;
        Ok(Spanned { 
            offset: begin.offset, 
            len: end.offset - begin.offset, 
            line_beginning: begin.line_beginning, 
            v: Function { name: identifier.v, body, ret_type: return_type.v }
        })
    }

    fn parse_block(&mut self) -> Result<(Vec<Spanned<Statement>>, Spanned<()>), Spanned<Error>> {
        self.expect_token(Token::OpenCurly)?;
        let mut body = vec![];

        while self.peek().is_some_and(|t| &t.v != &Token::CloseCurly) {
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
        todo!("parse_statement")
    }

    fn expect_token(&mut self, t: Token) -> Result<Spanned<Token>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned { offset, len, line_beginning, v }) if v == t => {
                self.next();
                Ok(Spanned { offset, len, line_beginning, v })
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedToken { expected: t, got: None })),
            Some(Spanned { v, .. }) => Err(self.spanned_error_from_last_tk(Error::ExpectedToken { expected: t, got: Some(v.clone()) }))
        }
    }


    fn expect_keyword(&mut self, k: Keyword) -> Result<Spanned<Token>, Spanned<Error>> {
        self.expect_token(Token::Keyword(k))
    }

    fn expect_name(&mut self) -> Result<Spanned<String>, Spanned<Error>> {
        match self.peek() {
            Some(Spanned { offset, len, line_beginning, v: Token::Identifier(i) }) => {
                self.next();
                Ok(Spanned { offset, len, line_beginning, v: i.to_string() })
            }
            None => Err(self.spanned_error_from_last_tk(Error::ExpectedToken { expected: Token::Identifier("name".to_string()), got: None })),
            Some(Spanned { v, .. }) => Err(self.spanned_error_from_last_tk(Error::ExpectedToken { expected: Token::Identifier("name".to_string()), got: Some(v.clone()) }))
        }
    }

    fn finished(&self) -> bool {
        self.tokens.is_empty()
    }

    fn spanned_error_from_last_tk(&self, e: Error) -> Spanned<Error> {
        Spanned { offset: self.prev_token.offset, len: self.prev_token.len, line_beginning: self.prev_token.line_beginning, v: e }
    }

    fn peek(&self) -> Option<Spanned<Token>> {
        self.tokens.first().cloned()
    }

    fn next(&mut self) -> Option<&Spanned<Token>> {
        let tk = self.tokens.first().clone();
        self.tokens = &self.tokens[1..];
        tk 
    }

}

pub struct Parser<'a> {
    tokens: &'a [Spanned<Token>],
    prev_token: &'a Spanned<Token>,
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Spanned<Function>>
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: vec![]
        }
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    body: Vec<Spanned<Statement>>,
    ret_type: String,
}

#[derive(Debug)]
pub enum Statement {
    Return {
        value: Option<Spanned<Expression>>,
    }
}

#[derive(Debug)]
pub enum Expression {
    Integer(u64)
}

pub enum Error {
    ExpectedToken {expected: Token, got: Option<Token>}
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedToken { expected, got } => {
                let got_name = match got {
                    None => "nothing".to_string(),
                    Some(v) => format!("{v:?}")
                };
                logging::error!(f, "Expected {expected:?}, instead got {got_name}")
            }
        } 
    }
}

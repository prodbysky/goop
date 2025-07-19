use crate::logging;
use crate::{Span, Spanned};
use colored::Colorize;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [char],
    offset: usize,
    line_beginning: usize,
}

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char),
    InvalidNumberLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(u64),
    Operator(Operator),
    Char(char),
    Keyword(Keyword),
    Identifier(String),
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    Assign,
    Colon,
    Comma,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Return,
    Let,
    True,
    False,
    If,
    While,
    Func,
    Extern,
    Cast,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Less,
    More,
    Not,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [char]) -> Self {
        Self {
            input: src,
            offset: 0,
            line_beginning: 0,
        }
    }

    pub fn lex(mut self) -> Result<Vec<Spanned<Token>>, Spanned<Error>> {
        let mut tokens = vec![];
        while !self.finished() {
            self.skip_ws();
            if self.finished() {
                return Ok(tokens);
            }

            match self.input[self.offset] {
                c if c.is_ascii_digit() => {
                    tokens.push(
                        self.lex_number()
                            .map(|op| op.map(|op| Token::Integer(op.v)))?,
                    );
                }
                c if "+-*/<>%!".contains(c) => {
                    tokens.push(
                        self.lex_operator()
                            .map(|op| op.map(|op| Token::Operator(op.v)))?,
                    );
                }
                '(' => {
                    tokens.push(self.lex_and_skip_single_char(Token::OpenParen)?);
                }
                ')' => {
                    tokens.push(self.lex_and_skip_single_char(Token::CloseParen)?);
                }
                '{' => {
                    tokens.push(self.lex_and_skip_single_char(Token::OpenCurly)?);
                }
                '}' => {
                    tokens.push(self.lex_and_skip_single_char(Token::CloseCurly)?);
                }
                ';' => {
                    tokens.push(self.lex_and_skip_single_char(Token::Semicolon)?);
                }
                ':' => {
                    tokens.push(self.lex_and_skip_single_char(Token::Colon)?);
                }
                '=' => {
                    tokens.push(self.lex_and_skip_single_char(Token::Assign)?);
                }
                ',' => {
                    tokens.push(self.lex_and_skip_single_char(Token::Comma)?);
                }
                // TODO: Proper char lexing omg
                '\'' => {
                    let begin = self.offset;
                    self.eat().unwrap();
                    let c = *self.eat().unwrap();
                    let end_c = *self.eat().unwrap();
                    assert!(end_c == '\'');
                    tokens.push(Spanned::new(Token::Char(c), Span::new(begin, begin + 3)));
                }
                'a'..='z' | 'A'..='Z' => {
                    let begin = self.offset;
                    while self
                        .current()
                        .is_some_and(|c| c.is_ascii_alphanumeric() || *c == '_')
                    {
                        self.eat();
                    }
                    let end = self.offset;
                    let slice: String = self.input[begin..end].iter().collect();
                    match slice.as_str() {
                        "return" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::Return),
                            Span::new(begin, end),
                        )),
                        "let" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::Let),
                            Span::new(begin, end),
                        )),
                        "true" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::True),
                            Span::new(begin, end),
                        )),
                        "false" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::False),
                            Span::new(begin, end),
                        )),
                        "if" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::If),
                            Span::new(begin, end),
                        )),
                        "while" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::While),
                            Span::new(begin, end),
                        )),
                        "func" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::Func),
                            Span::new(begin, end),
                        )),
                        "extern" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::Extern),
                            Span::new(begin, end),
                        )),
                        "cast" => tokens.push(Spanned::new(
                            Token::Keyword(Keyword::Cast),
                            Span::new(begin, end),
                        )),
                        _ => tokens.push(Spanned::new(
                            Token::Identifier(slice),
                            Span::new(begin, end),
                        )),
                    }
                }
                c => {
                    return Err(Spanned::new(
                        Error::UnexpectedChar(c),
                        Span::new(self.offset, self.offset + 1),
                    ));
                }
            };
        }
        Ok(tokens)
    }

    fn skip_ws(&mut self) {
        while !self.finished() && self.input[self.offset].is_whitespace() {
            match self.input[self.offset] {
                ' ' | '\t' => self.offset += 1,
                '\n' => {
                    self.offset += 1;
                    self.line_beginning = self.offset;
                }
                _ => unreachable!(),
            }
        }
    }

    fn lex_number(&mut self) -> Result<Spanned<u64>, Spanned<Error>> {
        let begin = self.offset;
        while self.current().is_some_and(|c| c.is_ascii_digit()) {
            self.eat();
        }
        if self.current().is_some_and(|c| c.is_alphabetic()) {
            self.eat();
            return Err(Spanned::new(
                Error::InvalidNumberLiteral,
                Span::new(begin, self.offset - 1 - begin),
            ));
        }
        let number = self.input[begin..self.offset]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap();
        Ok(Spanned::new(number, Span::new(begin, self.offset)))
    }

    fn lex_and_skip_single_char<T>(&mut self, o: T) -> Result<Spanned<T>, Spanned<Error>> {
        self.eat().unwrap();
        Ok(Spanned::new(o, Span::new(self.offset - 1, self.offset)))
    }

    fn lex_operator(&mut self) -> Result<Spanned<Operator>, Spanned<Error>> {
        // TODO: Operation assign (^=) operator
        match self.current().unwrap() {
            '+' => self.lex_and_skip_single_char(Operator::Plus),
            '-' => self.lex_and_skip_single_char(Operator::Minus),
            '*' => self.lex_and_skip_single_char(Operator::Star),
            '/' => self.lex_and_skip_single_char(Operator::Slash),
            '<' => self.lex_and_skip_single_char(Operator::Less),
            '>' => self.lex_and_skip_single_char(Operator::More),
            '%' => self.lex_and_skip_single_char(Operator::Percent),
            '!' => self.lex_and_skip_single_char(Operator::Not),
            c => Err(Spanned::new(
                Error::UnexpectedChar(*c),
                Span::new(self.offset, self.offset + 1),
            )),
        }
    }

    fn finished(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current(&self) -> Option<&char> {
        self.input.get(self.offset)
    }

    fn eat(&mut self) -> Option<&char> {
        self.offset += 1;
        self.input.get(self.offset - 1)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::UnexpectedChar(_c) => {
                logging::errorln!(f, "unexpected char found during lexing")?;
                logging::helpln!(
                    f,
                    "If the arrow is pointing at whitespace, try retyping the line since you might have inserted invisible unicode characters.\n  If this error still occurs this is a bug in the compiler, please report it in the issues tab of the github repository"
                )
            }
            Self::InvalidNumberLiteral => {
                logging::errorln!(f, "Invalid number literal found during lexing")?;
                logging::noteln!(
                    f,
                    "Numbers must be separated by whitespace or other characters that are not a..z etc.\n  For example this `123 123` is two valid number literals\n  `123a 123a` is not."
                )?;
                logging::help!(
                    f,
                    "You might have tried to use a binary (0b) or hexadecimal (0x) literal. They are not supported as of now"
                )
            }
        }
    }
}

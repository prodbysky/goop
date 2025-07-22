use super::error::Error;
use crate::location::{Span, Spanned};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [char],
    offset: usize,
    line_beginning: usize,
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
        const OPERATOR_CHARS: &str = "+-*/<>%!";
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
                            .map(|op| op.map(|op| Token::Integer(op.to_inner())))?,
                    );
                }
                c if OPERATOR_CHARS.contains(c) => {
                    tokens.push(
                        self.lex_operator()
                            .map(|op| op.map(|op| Token::Operator(op.to_inner())))?,
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
                    tokens.push(self.lex_char_literal()?);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    tokens.push(self.lex_ident_or_keyword()?);
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

    fn lex_ident_or_keyword(&mut self) -> Result<Spanned<Token>, Spanned<Error>> {
        let begin = self.offset;
        while self
            .current()
            .is_some_and(|c| c.is_ascii_alphanumeric() || *c == '_')
        {
            self.eat();
        }
        let end = self.offset;
        let slice: String = self.input[begin..end].iter().collect();
        Ok(match slice.as_str() {
            "return" => Spanned::new(Token::Keyword(Keyword::Return), Span::new(begin, end)),
            "let" => Spanned::new(Token::Keyword(Keyword::Let), Span::new(begin, end)),
            "true" => Spanned::new(Token::Keyword(Keyword::True), Span::new(begin, end)),
            "false" => Spanned::new(Token::Keyword(Keyword::False), Span::new(begin, end)),
            "if" => Spanned::new(Token::Keyword(Keyword::If), Span::new(begin, end)),
            "while" => Spanned::new(Token::Keyword(Keyword::While), Span::new(begin, end)),
            "func" => Spanned::new(Token::Keyword(Keyword::Func), Span::new(begin, end)),
            "extern" => Spanned::new(Token::Keyword(Keyword::Extern), Span::new(begin, end)),
            "cast" => Spanned::new(Token::Keyword(Keyword::Cast), Span::new(begin, end)),
            _ => Spanned::new(Token::Identifier(slice), Span::new(begin, end)),
        })
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
                Span::new(begin, self.offset),
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

    fn lex_char_literal(&mut self) -> Result<Spanned<Token>, Spanned<Error>> {
        assert!(self.current().is_some_and(|c| *c == '\''));
        let begin = self.offset;
        self.eat();
        let c = match self.eat() {
            None => {
                return Err(Spanned::new(
                    Error::UnterminatedCharLiteral,
                    Span::new(begin, self.offset),
                ));
            }
            Some(other) => *other,
        };
        match self.eat() {
            Some('\'') => Ok(Spanned::new(Token::Char(c), Span::new(begin, self.offset))),
            None | Some(_) => {
                return Err(Spanned::new(
                    Error::UnterminatedCharLiteral,
                    Span::new(begin, self.offset),
                ));
            }
        }
    }

    fn finished(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current(&self) -> Option<&char> {
        self.input.get(self.offset)
    }

    fn eat(&mut self) -> Option<&char> {
        let ch = self.input.get(self.offset);
        self.offset += 1;
        ch
    }

    fn peek(&self, offset: usize) -> Option<&char> {
        self.input.get(self.offset + offset)
    }
}

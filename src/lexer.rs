use crate::Spanned;
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
#[derive(Debug, Clone)]
pub enum Token {
    Integer(u64),
    Operator(Operator),
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [char]) -> Self {
        Self {
            input: src,
            offset: 0,
            line_beginning: 0,
        }
    }

    fn skip_ws(&mut self) {
        while !self.finished() && self.input[self.offset].is_whitespace() {
            match self.input[self.offset] {
                ' ' | '\t' => self.offset += 1,
                '\n' => {
                    self.offset += 1;
                    self.line_beginning += self.offset;
                }
                _ => unreachable!(),
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
        self.offset += 1;
        self.input.get(self.offset - 1)
    }

    fn lex_number(&mut self) -> Result<Spanned<u64>, Spanned<Error>> {
        let begin = self.offset;
        while self.current().is_some_and(|c| c.is_ascii_digit()) {
            self.eat();
        }
        if self.current().is_some_and(|c| c.is_alphabetic()) {
            self.eat();
            return Err(Spanned {
                offset: begin,
                len: self.offset - 1 - begin,
                line_beginning: self.line_beginning,
                v: Error::InvalidNumberLiteral,
            });
        }
        let number = self.input[begin..self.offset]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap();
        Ok(Spanned {
            offset: begin,
            len: self.offset - begin,
            line_beginning: self.line_beginning,
            v: number,
        })
    }

    fn lex_operator(&mut self) -> Result<Spanned<Operator>, Spanned<Error>> {
        // TODO: Operation assign (^=) operator
        match self.current().unwrap() {
            '+' => {
                self.eat();
                Ok(Spanned {
                    len: 1,
                    line_beginning: self.line_beginning,
                    offset: self.offset - 1,
                    v: Operator::Plus,
                })
            }
            '-' => {
                self.eat();
                Ok(Spanned {
                    len: 1,
                    line_beginning: self.line_beginning,
                    offset: self.offset - 1,
                    v: Operator::Minus,
                })
            }
            '*' => {
                self.eat();
                Ok(Spanned {
                    len: 1,
                    line_beginning: self.line_beginning,
                    offset: self.offset - 1,
                    v: Operator::Star,
                })
            }
            '/' => {
                self.eat();
                Ok(Spanned {
                    len: 1,
                    line_beginning: self.line_beginning,
                    offset: self.offset - 1,
                    v: Operator::Slash,
                })
            }
            c => Err(Spanned {
                offset: self.offset,
                len: 1,
                line_beginning: self.line_beginning,
                v: Error::UnexpectedChar(*c),
            }),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Token>, Spanned<Error>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();
        if self.finished() {
            return None;
        }
        match self.input[self.offset] {
            c if c.is_ascii_digit() => {
                let t = self.lex_number().map(|t| Spanned {
                    v: Token::Integer(t.v),
                    offset: t.offset,
                    len: t.len,
                    line_beginning: t.line_beginning,
                });
                Some(t)
            }
            c if "+-*/".contains(c) => {
                let op = self.lex_operator().map(|t| Spanned {
                    v: Token::Operator(t.v),
                    offset: t.offset,
                    len: t.len,
                    line_beginning: t.line_beginning,
                });
                Some(op)
            }
            '(' => {
                self.eat();
                Some(Ok(Spanned {
                    offset: self.offset - 1,
                    len: 1,
                    line_beginning: self.line_beginning,
                    v: Token::OpenParen,
                }))
            }
            ')' => {
                self.eat();
                Some(Ok(Spanned {
                    offset: self.offset - 1,
                    len: 1,
                    line_beginning: self.line_beginning,
                    v: Token::CloseParen,
                }))
            }
            c => {
                self.eat();
                Some(Err(Spanned {
                    offset: self.offset - 1,
                    line_beginning: self.line_beginning,
                    len: 1,
                    v: Error::UnexpectedChar(c),
                }))
            }
        }
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::UnexpectedChar(c) => {
                writeln!(
                    f,
                    "[{}]\n  Unexpected char found during lexing `{c}`",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n  If the arrow is pointing at whitespace, try retyping the line since you might have inserted invisible unicode characters.\n  If this error still occurs this is a bug in the compiler, please report it in the issues tab of the github repository",
                    "Help".blue()
                )
            }
            Self::InvalidNumberLiteral => {
                writeln!(
                    f,
                    "[{}]\n  Invalid number literal found during lexing",
                    "Error".red()
                )?;
                writeln!(
                    f,
                    "[{}]\n  Numbers must be separated by whitespace or other characters that are not a..z etc.\n  For example this `123 123` is two valid number literals\n  `123a 123a` is not.",
                    "Note".green()
                )?;
                write!(
                    f,
                    "[{}]\n  You might have tried to use a binary (0b) or hexadecimal (0x) literal. They are not supported as of now",
                    "Help".blue()
                )
            }
        }
    }
}

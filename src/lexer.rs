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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Return,
    Let,
    True,
    False,
    If,
    While,
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

    fn lex_and_skip_single_char<T>(&mut self, o: T) -> Result<Spanned<T>, Spanned<Error>> {
        self.eat();
        Ok(Spanned {
            len: 1,
            line_beginning: self.line_beginning,
            offset: self.offset - 1,
            v: o,
        })
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
            c => Err(Spanned {
                offset: self.offset,
                len: 1,
                line_beginning: self.line_beginning,
                v: Error::UnexpectedChar(*c),
            }),
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

impl Iterator for Lexer<'_> {
    type Item = Result<Spanned<Token>, Spanned<Error>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();
        if self.finished() {
            return None;
        }
        match self.input[self.offset] {
            c if c.is_ascii_digit() => Some(self.lex_number().map(|t| Spanned {
                v: Token::Integer(t.v),
                offset: t.offset,
                len: t.len,
                line_beginning: t.line_beginning,
            })),
            c if "+-*/<>%!".contains(c) => Some(self.lex_operator().map(|t| Spanned {
                v: Token::Operator(t.v),
                offset: t.offset,
                len: t.len,
                line_beginning: t.line_beginning,
            })),
            '(' => Some(self.lex_and_skip_single_char(Token::OpenParen)),
            ')' => Some(self.lex_and_skip_single_char(Token::CloseParen)),
            '{' => Some(self.lex_and_skip_single_char(Token::OpenCurly)),
            '}' => Some(self.lex_and_skip_single_char(Token::CloseCurly)),
            ';' => Some(self.lex_and_skip_single_char(Token::Semicolon)),
            ':' => Some(self.lex_and_skip_single_char(Token::Colon)),
            '=' => Some(self.lex_and_skip_single_char(Token::Assign)),
            '\'' => {
                let begin = self.offset;
                let begin_line = self.line_beginning;
                self.eat().unwrap();
                let c = *self.eat().unwrap();
                let end_c = *self.eat().unwrap();
                assert!(end_c == '\'');
                Some(Ok(Spanned {
                    offset: begin,
                    len: 3,
                    line_beginning: begin_line,
                    v: Token::Char(c),
                }))
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
                    "return" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::Return),
                    })),
                    "let" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::Let),
                    })),
                    "true" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::True),
                    })),
                    "false" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::False),
                    })),
                    "if" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::If),
                    })),
                    "while" => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Keyword(Keyword::While),
                    })),
                    _ => Some(Ok(Spanned {
                        offset: begin,
                        len: end - begin,
                        line_beginning: self.line_beginning,
                        v: Token::Identifier(slice),
                    })),
                }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        const SRC: &str = "return let let";
        let chars: Vec<_> = SRC.chars().collect();
        let tks: Vec<_> = Lexer::new(&chars).map(|r| r.unwrap()).collect();
        assert_eq!(
            tks,
            vec![
                Spanned {
                    offset: 0,
                    len: 6,
                    line_beginning: 0,
                    v: Token::Keyword(Keyword::Return)
                },
                Spanned {
                    offset: 7,
                    len: 3,
                    line_beginning: 0,
                    v: Token::Keyword(Keyword::Let)
                },
                Spanned {
                    offset: 11,
                    len: 3,
                    line_beginning: 0,
                    v: Token::Keyword(Keyword::Let)
                },
            ]
        )
    }

    #[test]
    fn punctuation() {
        const SRC: &str = "( ( ) ) ; :";
        let chars: Vec<_> = SRC.chars().collect();
        let tks: Vec<_> = Lexer::new(&chars).map(|r| r.unwrap()).collect();
        assert_eq!(
            tks,
            vec![
                Spanned {
                    offset: 0,
                    len: 1,
                    line_beginning: 0,
                    v: Token::OpenParen
                },
                Spanned {
                    offset: 2,
                    len: 1,
                    line_beginning: 0,
                    v: Token::OpenParen
                },
                Spanned {
                    offset: 4,
                    len: 1,
                    line_beginning: 0,
                    v: Token::CloseParen
                },
                Spanned {
                    offset: 6,
                    len: 1,
                    line_beginning: 0,
                    v: Token::CloseParen
                },
                Spanned {
                    offset: 8,
                    len: 1,
                    line_beginning: 0,
                    v: Token::Semicolon
                },
                Spanned {
                    offset: 10,
                    len: 1,
                    line_beginning: 0,
                    v: Token::Colon
                },
            ]
        )
    }
}

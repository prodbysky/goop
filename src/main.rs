mod config;
mod lexer;

use colored::Colorize;

fn main() {
    let config = match config::Config::from_args(std::env::args()) {
        Some(c) => c,
        None => return,
    };
    let input = match std::fs::read_to_string(&config.input_name) {
        Ok(i) => i,
        Err(e) => {
            eprintln!(
                "[{}]: Failed to read {input_name}: {e}",
                "Error".red(),
                input_name = config.input_name,
            );
            usage(&config.program_name);
            return;
        }
    };

    let input_chars: Vec<_> = input.chars().collect();

    let lexer = lexer::Lexer::new(&input_chars);
    let elements: Vec<_> = lexer.collect();
    let lexer_errors: Vec<_> = elements
        .iter()
        .filter_map(|e| match e {
            Err(e) => Some(e),
            Ok(_) => None,
        })
        .collect();

    for e in &lexer_errors {
        match &e.v {
            lexer::Error::UnexpectedChar(c) => {
                eprintln!(
                    "[{}]\n  Unexpected char found during lexing `{c}`",
                    "Error".red()
                );
            }
            lexer::Error::InvalidNumberLiteral => {
                eprintln!(
                    "[{}]\n  Invalid number literal found during lexing",
                    "Error".red()
                );
                eprintln!(
                    "[{}]\n  Numbers must be separated by whitespace or other characters that are not a..z etc.\n  For example this `123 123` is two valid number literals\n  `123a 123a` is not.",
                    "Note".green()
                )
            }
        }
        display_diagnostic_info(&input, &config.input_name, e);
    }

    if !lexer_errors.is_empty() {
        return;
    }
    let tokens: Vec<_> = elements
        .iter()
        .filter_map(|e| match e {
            Err(_) => None,
            Ok(t) => Some(t.clone()),
        })
        .collect();

    let parser = Parser::new(&tokens);
    let (exprs, parser_errors) = parser.parse();
    for e in &parser_errors {
        match &e.v {
            Error::ExpectedPrimaryExpresion => {
                eprintln!(
                    "[{}]\n  Expected a primary expression here\n  [{}]: A primary expression is only a number literal for now",
                    "Error".red(),
                    "Note".blue()
                )
            }
            Error::UnexpectedTokenInExpression => {
                eprintln!(
                    "[{}]\n  Unexpected token found when parsing an expression here",
                    "Error".red(),
                )
            }
            Error::ExpectedBinaryOperator => {
                eprintln!(
                    "[{}]\n  Expected a binary operator here\n  [{}]\n  A binary operator in Goop can only be one of these: `+`, `-`, `*', `/`",
                    "Error".red(),
                    "Note".blue(),
                )
            }
            Error::UnclosedParenthesis => {
                eprintln!(
                    "[{}]\n  Found unbalanced parenthesis when parsing an enclosed expression",
                    "Error".red(),
                )
            }
        };
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !parser_errors.is_empty() {
        return;
    }
    for e in exprs {
        dbg!(e);
    }
}

fn display_diagnostic_info<T>(input: &str, input_name: &str, e: &Spanned<T>) {
    let line_offset = e.offset - e.line_beginning;
    let line_end = input[e.line_beginning..].find('\n').unwrap_or(input.len());
    let line = &input[e.line_beginning..line_end];

    let line_count = {
        let upto = &input[0..e.line_beginning];
        upto.chars().filter(|c| *c == '\n').count() + 1
    };

    let prefix = format!("  ./{}:{}:{}", input_name, line_count, line_offset + 1);
    eprintln!("{prefix}\n{}", line);
    eprintln!(
        "{}{}",
        " ".repeat(e.offset - e.line_beginning),
        "^".repeat(e.len)
    );
}

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

    fn parse(mut self) -> (Vec<Spanned<Expression>>, Vec<Spanned<Error>>) {
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
                v: lexer::Token::Number(n),
            }) => {
                let r = Ok(Spanned {
                    offset: *offset,
                    len: *len,
                    line_beginning: *line_beginning,
                    v: Expression::Number(*n),
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

#[derive(Debug, Clone)]
pub enum Expression {
    Number(u64),
    Binary {
        left: Box<Spanned<Expression>>,
        op: lexer::Operator,
        right: Box<Spanned<Expression>>,
    },
}

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub offset: usize,
    pub len: usize,
    pub line_beginning: usize,
    pub v: T,
}

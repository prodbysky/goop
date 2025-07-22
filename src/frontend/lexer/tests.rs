use super::*;
use crate::frontend::lexer::error::Error;
use crate::location::{Span, Spanned};
use lexer::{Keyword, Token};

macro_rules! spanned {
    ($e:expr, $begin:expr, $end:expr) => {
        Spanned::new($e, Span::new($begin, $end))
    };
    ($e:expr, $begin:expr) => {
        Spanned::new($e, Span::new($begin, $begin + 1))
    };
}

#[test]
fn punctuation() {
    let tks = parse_source("( ) { }\n, =\n; :");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Token::OpenParen, 0),
            spanned!(Token::CloseParen, 2),
            spanned!(Token::OpenCurly, 4),
            spanned!(Token::CloseCurly, 6),
            spanned!(Token::Comma, 8),
            spanned!(Token::Assign, 10),
            spanned!(Token::Semicolon, 12),
            spanned!(Token::Colon, 14),
        ],
    )
}

#[test]
fn keywords() {
    let tks = parse_source("return let true false if while func extern cast");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Token::Keyword(Keyword::Return), 0, 6),
            spanned!(Token::Keyword(Keyword::Let), 7, 10),
            spanned!(Token::Keyword(Keyword::True), 11, 15),
            spanned!(Token::Keyword(Keyword::False), 16, 21),
            spanned!(Token::Keyword(Keyword::If), 22, 24),
            spanned!(Token::Keyword(Keyword::While), 25, 30),
            spanned!(Token::Keyword(Keyword::Func), 31, 35),
            spanned!(Token::Keyword(Keyword::Extern), 36, 42),
            spanned!(Token::Keyword(Keyword::Cast), 43, 47),
        ],
    )
}

#[test]
fn char_literals() {
    let tks = parse_source("'n' '\n' '\t'");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Token::Char('n'), 0, 3),
            spanned!(Token::Char('\n'), 4, 7),
            spanned!(Token::Char('\t'), 8, 11),
        ],
    )
}

fn parse_source(src: &str) -> Result<Vec<Spanned<Token>>, Spanned<Error>> {
    let chars = src.chars().collect::<Vec<_>>();
    lexer::Lexer::new(&chars).lex()
}

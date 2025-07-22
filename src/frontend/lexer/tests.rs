use super::*;
use crate::frontend::lexer::error::Error;
use crate::location::{Span, Spanned};
use lexer::{Keyword, Token, Operator};

macro_rules! spanned {
    (Punct, $v:ident, $begin:expr) => {
        Spanned::new(Token::$v, Span::new($begin, $begin + 1))
    };
    (Keyword, $v:ident, $begin:expr) => {
        Spanned::new(Token::Keyword(Keyword::$v), Span::new($begin, $begin + stringify!($v).len()))
    };
    (Char, $v:expr, $begin:expr) => {
        Spanned::new(Token::Char($v), Span::new($begin, $begin + 3))
    };
    (Num, $v:expr, $begin:expr) => {
        Spanned::new(Token::Integer($v), Span::new($begin, $begin + stringify!($v).len()))
    };
    (Op, $v:ident, $begin:expr) => {
        Spanned::new(Token::Operator(Operator::$v), Span::new($begin, $begin + 1))
    };
    ($id:ident, $begin:expr) => {
        Spanned::new(Token::Identifier(stringify!($id).to_string()), Span::new($begin, $begin + stringify!($id).len()))
    };
}

#[test]
fn punctuation() {
    let tks = parse_source("( ) { }\n, =\n; :");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Punct, OpenParen, 0),
            spanned!(Punct, CloseParen, 2),
            spanned!(Punct, OpenCurly, 4),
            spanned!(Punct, CloseCurly, 6),
            spanned!(Punct, Comma, 8),
            spanned!(Punct, Assign, 10),
            spanned!(Punct, Semicolon, 12),
            spanned!(Punct, Colon, 14),
        ],
    )
}

#[test]
fn keywords() {
    let tks = parse_source("return let true false if while func extern cast");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Keyword, Return, 0),
            spanned!(Keyword, Let, 7),
            spanned!(Keyword, True, 11),
            spanned!(Keyword, False, 16),
            spanned!(Keyword, If, 22),
            spanned!(Keyword, While, 25),
            spanned!(Keyword, Func, 31),
            spanned!(Keyword, Extern, 36),
            spanned!(Keyword, Cast, 43),
        ],
    )
}

#[test]
fn char_literals() {
    let tks = parse_source("'n' '\n' '\t'");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Char, 'n', 0),
            spanned!(Char, '\n', 4),
            spanned!(Char, '\t', 8),
        ],
    )
}

#[test]
fn full_grammar() {
    let tks = parse_source("func main() i32 { let i: i32 = 0; i = i + 10; return i;}");
    assert_eq!(
        tks.unwrap(),
        vec![
            spanned!(Keyword, Func, 0),
            spanned!(main, 5),
            spanned!(Punct, OpenParen, 9),
            spanned!(Punct, CloseParen, 10),
            spanned!(i32, 12),
            spanned!(Punct, OpenCurly, 16),
            spanned!(Keyword, Let, 18),
            spanned!(i, 22),
            spanned!(Punct, Colon, 23),
            spanned!(i32, 25),
            spanned!(Punct, Assign, 29),
            spanned!(Num, 0, 31),
            spanned!(Punct, Semicolon, 32),
            spanned!(i, 34),
            spanned!(Punct, Assign, 36),
            spanned!(i, 38),
            spanned!(Op, Plus, 40),
            spanned!(Num, 10, 42),
            spanned!(Punct, Semicolon, 44),
            spanned!(Keyword, Return, 46),
            spanned!(i, 53),
            spanned!(Punct, Semicolon, 54),
            spanned!(Punct, CloseCurly, 55),
        ],
    )
}

fn parse_source(src: &str) -> Result<Vec<Spanned<Token>>, Spanned<Error>> {
    let chars = src.chars().collect::<Vec<_>>();
    lexer::Lexer::new(&chars).lex()
}

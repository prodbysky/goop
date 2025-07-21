use crate::frontend::lexer::lexer::Token;
use crate::logging;
use colored::Colorize;

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

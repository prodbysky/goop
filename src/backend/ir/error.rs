use colored::Colorize;

use super::ir::Type;
use crate::logging;
use crate::frontend::parser::parser;

#[derive(Debug)]
pub enum Error {
    MismatchedReturnType {
        got: Type,
        expect: Type,
    },
    VariableRedefinition,
    UnexpectedType {
        got: Type,
        expect: Type,
    },
    UndefinedVariableRedefinition,
    NotBooleanCondition,
    UndefinedFunction,
    MismatchedArgumentCount {
        callee_type: parser::FunctionType,
        expect: usize,
        got: usize,
    },
    MismatchedArgumentTypes {
        callee_type: parser::FunctionType,
        expect: usize,
        got: usize,
    },
    UndefinedVariable,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedFunction => {
                logging::errorln!(f, "You tried to call an undefined function")?;
                logging::help!(f, "Maybe you have misspelled the name?")
            }
            Self::UndefinedVariable => {
                logging::errorln!(f, "You tried to use an undefined variable")?;
                logging::help!(f, "Maybe you have misspelled the name?")
            }
            Self::NotBooleanCondition => {
                logging::error!(
                    f,
                    "You tried to use a non-boolean condition in a `if` or `while` statement"
                )
            }
            Self::UndefinedVariableRedefinition => {
                logging::errorln!(f, "You tried to redefine an undefined variable",)?;
                logging::help!(f, "Maybe you have misspelled the name?",)
            }
            Self::MismatchedArgumentTypes { callee_type, .. } => {
                logging::errorln!(f, "Called a function with mismatched argument types",)?;
                logging::note!(f, "Callee type: {callee_type}")
            }
            Self::MismatchedArgumentCount { callee_type, .. } => {
                logging::errorln!(
                    f,
                    "Called a function with either not enough or too many arguments",
                )?;
                logging::note!(f, "Callee type: {callee_type}")
            }
            Self::UnexpectedType { got, expect } => {
                logging::errorln!(f, "You mismatched some types")?;
                logging::note!(f, "Expected: {expect:?}, got: {got:?}",)
            }
            Self::MismatchedReturnType { got, expect } => {
                logging::errorln!(
                    f,
                    "You tried to return a value that does not match the functions expected return type"
                )?;
                logging::note!(f, "Expected: {expect:?}, got: {got:?}")
            }
            Self::VariableRedefinition => {
                logging::error!(f, "You tried to redefine a variable")
            }
        }
    }
}

use crate::logging;
use colored::Colorize;

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char),
    UnterminatedCharLiteral,
    InvalidNumberLiteral,
    UnknownEscapeChar,
    MissingEscapeChar,
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
            Self::UnterminatedCharLiteral => {
                logging::error!(f, "Unterminated character literal found")
            }
            Self::MissingEscapeChar => {
                logging::errorln!(f, "Expected an escape character to be here")?;
                logging::help!(f, "One of these: n, t, ', \", \\")
            }
            Self::UnknownEscapeChar => {
                logging::error!(f, "Found an unknown escape character")
            }
        }
    }
}

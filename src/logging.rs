macro_rules! error {
    ($f:expr, $($arg:tt)*) => {
        write!($f, "[{}]\n  {}", "Error".red(), format!($($arg)*))
    };
}
macro_rules! errorln {
    ($f:expr, $($arg:tt)*) => {
        writeln!($f, "[{}]\n  {}", "Error".red(), format!($($arg)*))
    };
}

macro_rules! help {
    ($f:expr, $($arg:tt)*) => {
        write!($f, "[{}]\n  {}", "Help".blue(), format!($($arg)*))
    };
}

macro_rules! helpln {
    ($f:expr, $($arg:tt)*) => {
        writeln!($f, "[{}]\n  {}", "Help".blue(), format!($($arg)*))
    };
}

macro_rules! note {
    ($f:expr, $($arg:tt)*) => {
        write!($f, "[{}]\n  {}", "Note".green(), format!($($arg)*))
    };
}

macro_rules! noteln {
    ($f:expr, $($arg:tt)*) => {
        writeln!($f, "[{}]\n  {}", "Note".green(), format!($($arg)*))
    };
}

pub(crate) use errorln;
pub(crate) use error;

pub(crate) use helpln;
pub(crate) use help;

pub(crate) use noteln;
pub(crate) use note;

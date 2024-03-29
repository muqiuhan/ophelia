pub mod E0002;
pub mod E0003;
pub mod E0004;
pub mod E0005;
pub mod E0006;
pub mod E0007;
pub mod E0008;
pub mod E0009;
pub mod E0010;

use crate::error::{self, Reporter};

/// Result type of IR generator.
pub type Result<T> = std::result::Result<T, error::Error>;
pub static mut REPORTER: Option<Reporter> = None;

/// Possible errors during the IR generator process
pub enum Error {
    DuplicatedDef,
    SymbolNotFound,
    FailedToEval,
    InvalidArrayLen,
    InvalidInit,
    ArrayAssign,
    NotInLoop,
    RetValInVoidFunc,
    DerefInt,
    UseVoidValue,
    ArgMismatch,
    NonIntCalc,
}